
# AutoTimeGPPT ----

## Create search space ----

finetune_steps <- seq(50,500,50)
finetune_depth <- c(4)
finetune_loss <- c("default")

search_space <- expand.grid(
  finetune_steps = finetune_steps,
  finetune_depth = finetune_depth,
  finetune_loss = finetune_loss,
  stringsAsFactors = FALSE
)

autotimegpt <- function(train_data, test_data, h, search_space, hist_exog_list, min_obs=NULL){

  # Generate multiple AutoTimeGPT models based on a search space and record their score
  train_long <- train_data |>
    group_by(unique_id) |>
    filter(n() >= min_obs) |>
    ungroup()

  train_short <- train_data |>
    group_by(unique_id) |>
    filter(n() < min_obs) |>
    ungroup()

  res <- data.frame(matrix(NA, nrow = nrow(search_space), ncol = 4))
  names(res) = c("finetune_steps", "finetune_depth", "finetune_loss", "score")

  for (i in 1:nrow(search_space)) {
    tryCatch({
      finetune_steps <- search_space$finetune_steps[i]
      finetune_depth  <- search_space$finetune_depth[i]
      finetune_loss   <- search_space$finetune_loss[i]

      fc_long <- nixtla_client_forecast(
        df = train_long,
        h = h,
        model = "timegpt-1-long-horizon",
        finetune_steps = finetune_steps,
        finetune_depth = finetune_depth,
        finetune_loss = finetune_loss,
        hist_exog_list = hist_exog_list
      )

      fc_short <- nixtla_client_forecast(
        df = train_short,
        h = h,
        model = "timegpt-1-long-horizon",
        hist_exog_list = hist_exog_list
      )

      fc_finetune <- rbind(fc_long, fc_short)

      # Check that the row counts are correct
      if (nrow(train_df) != nrow(train_long) + nrow(train_short)) {
        stop("Incorrect number of rows")
      }

      score <- vn1_competition_evaluation(test_df, fc_finetune, "TimeGPT")

      res$finetune_steps[i] <- finetune_steps
      res$finetune_depth[i] <- finetune_depth
      res$finetune_loss[i] <- finetune_loss
      res$score[i] <- score

      print(paste0("Ready test ", i, "/", nrow(search_space)))
    }, error = function(e) {
      print(paste("Skipping test", i, "due to error:", e$message))
    })
  }
  write.csv(res, "output/autotimegpt.csv", row.names = FALSE, quote = FALSE)
}

autotimegpt(df_exo_vars, test_df, 13, search_space, hist_exog_list=c("price"), min_obs=104)

results_finetuning <- fread(paste0(getwd(),"/output/autotimegpt.csv")) # results are already included

# Plot specific results
res <- results_finetuning |>
  filter(finetune_loss == "default") |>
  filter(finetune_depth == 4)

ggplot(res, aes(x = finetune_steps, y = score, color = factor(finetune_depth))) +
  geom_point()+
  geom_line()+
  labs(title = paste0("Scores using the ", unique(res$finetune_loss), " finetune loss"))

