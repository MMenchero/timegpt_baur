
# VN1 Forecasting Competition Solution with nixtlar ----
# -----------------------------------------------------------------------------*

library(tidyverse)
library(data.table)

# Installation ----
#install.packages("nixtlar")
library(nixtlar)
packageVersion("nixtlar")

source("functions.R") # same directory as main.R

# Load Data ----
sales0 <- read_and_prepare_data("phase0_sales", "y")
sales1 <- read_and_prepare_data("phase1_sales", "y")
test_df <- read_and_prepare_data("phase2_sales", "y")

# Prepare Training Dataset ----
train_df <- get_train_data(sales0, sales1, "y")

head(train_df) # 3 required columns

# Data visualization ----
print(paste0("Number of series: ", length(unique(train_df$unique_id))))

dates <- sort((unique(train_df$ds)))
print(paste0("Number of dates: ", length(dates)))
print(paste0("Earliest date: ", dates[1]))
print(paste0("Last date: ", dates[length(dates)]))

nixtla_client_plot(train_df, max_insample_length = 100)

# Evaluate Top 5 Competition Solutions ----
scores <- lapply(1:5, function(i){ # Top 5
  winner_df <- read_and_prepare_data(paste0("winners", i), "y")
  vn1_competition_evaluation(test_df, winner_df, model = paste0("winners", i))
})

scores_df <- data.frame(
  "Result" = c(paste0("Place #", 1:5)),
  "Score" = c(as.numeric(scores))
)

scores_df <- scores_df |> arrange(Score)
scores_df

# Generate TimeGPT Forecast  ----

## Set your API key ----
# Learn how to set up your API key here: https://nixtla.github.io/nixtlar/articles/setting-up-your-api-key.html
# Not secure
# nixtla_client_setup(api_key = "Your API key here")

# More secure
library(usethis)
usethis::edit_r_environ()

nixtla_validate_api_key()


## Vanilla TimeGPT ----
fc_vanilla <- nixtla_client_forecast(
  df = train_df,
  h = 13
)

nixtla_client_plot(train_df, fc_vanilla)

timegpt_vanilla <- vn1_competition_evaluation(test_df, fc_vanilla, "TimeGPT")

scores_df <- add_score("TimeGPT vanilla", timegpt_vanilla, scores_df)

## TimeGPT long-horizon model----
fc_long_horizon <- nixtla_client_forecast(
  df = train_df,
  h = 13,
  model = "timegpt-1-long-horizon"
)

nixtla_client_plot(train_df, fc_long_horizon)

timegpt_long_horizon <- vn1_competition_evaluation(test_df, fc_long_horizon, "TimeGPT")

scores_df <- add_score("TimeGPT long-horizon", timegpt_long_horizon, scores_df)


## Exogenous variables ----
prices0 <- read_and_prepare_data("phase0_prices", "price")
nixtla_client_plot(prices0, target_col = "price")

prices1<- read_and_prepare_data("phase1_prices", "price")
nixtla_client_plot(prices1, target_col = "price")

prices <- get_train_data(prices0, prices1, "price")
nixtla_client_plot(prices, target_col = "price")

df_exo_vars <- train_df |>
  left_join(prices, by = c("unique_id", "ds")) |>
  group_by(unique_id) |>
  fill(price, .direction = "down") |>
  mutate(price = replace_na(price, 1)) |>
  ungroup()

df_exo_vars |> # make sure there are no missing values
  filter(is.na(price))

### Historic exogenous ----
fc_exo_vars <- nixtla_client_forecast(
  df = df_exo_vars,
  h = 13,
  model = "timegpt-1-long-horizon",
  hist_exog_list = c("price")
)

nixtla_client_plot(df_exo_vars, fc_exo_vars)

timegpt_exo_vars <- vn1_competition_evaluation(test_df, fc_exo_vars, "TimeGPT")

scores_df <- add_score("TimeGPT exo vars", timegpt_exo_vars, scores_df)

### Future exogenous ----
prices_prepared <- df_exo_vars |>
  select(-y)

fc_prices <- nixtla_client_forecast(
  df = prices_prepared,
  h = 13,
  model = "timegpt-1-long-horizon",
  target_col = "price"
)

names(fc_prices)[which(names(fc_prices) == "TimeGPT")] <- "price"

fc_future_exo_vars <- nixtla_client_forecast(
  df = df_exo_vars,
  h = 13,
  X_df = fc_prices,
  model = "timegpt-1-long-horizon"
)

nixtla_client_plot(df_exo_vars, fc_future_exo_vars)

timegpt_future_exo_vars <- vn1_competition_evaluation(test_df, fc_future_exo_vars, "TimeGPT")

scores_df <- add_score("TimeGPT future exo vars", timegpt_future_exo_vars, scores_df)


## Fine-tuning & AutoTimeGPT ----
min_obs <- 104

dfs <- separate_series(df_exo_vars, min_obs)
train_long <- dfs$data_long
train_short <- dfs$data_short

nrow(train_df) == nrow(train_long)+nrow(train_short)

fc_long <- nixtla_client_forecast(
  df = train_long,
  h = 13,
  model = "timegpt-1-long-horizon",
  finetune_steps = 200,
  finetune_depth = 4,
  finetune_loss = "default",
  hist_exog_list = c("price")
)

fc_short <- fc_exo_vars |>
  filter(unique_id %in% train_short$unique_id)

fc_finetune <- rbind(fc_long, fc_short)

timegpt_finetune <- vn1_competition_evaluation(test_df, fc_finetune, "TimeGPT")

scores_df <- add_score("TimeGPT fine-tuning", timegpt_finetune, scores_df)

## Probabilistic forecasting ----
### Prediction intervals ----
conf_lvl <- c(80,95)

fc_prob <- nixtla_client_forecast(
  df = train_long,
  h = 13,
  model = "timegpt-1-long-horizon",
  level = conf_lvl,
  hist_exog_list = c("price")
)

head(fc_prob)

nixtla_client_plot(train_long, fc_prob, max_insample_length = 13*4)

### Quantiles ----
quantiles <- seq(0.1, 0.9, 0.1)

fc_quant <- nixtla_client_forecast(
  df = train_long,
  h = 13,
  model = "timegpt-1-long-horizon",
  quantiles = quantiles,
  hist_exog_list = c("price")
)

head(fc_quant)

nixtla_client_plot(train_long, fc_quant, max_insample_length = 13*4)

## Anomaly detection ----
anomalies <- nixtla_client_detect_anomalies(
  df = train_long,
  level = c(99)
)

head(anomalies)

anomalies |>
  filter(anomaly == TRUE) |>
  head()

## Cross-validation ----
cv <- nixtla_client_cross_validation(
  df = train_long,
  h = 13,
  n_windows = 4,
  step_size = 1
)

head(cv)
