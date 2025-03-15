
# Functions for VN1 Forecasting Competition ----

read_and_prepare_data <- function(dataset, colname){
  # Reads data in wide format and returns it in long format with columns `unique_id`, `ds`, and `y`
  path <- paste0(getwd(), "/data/", dataset, ".csv")
  df_wide <- fread(path)
  df_wide <- df_wide |>
    mutate(unique_id = paste0(Client, "/", Warehouse, "/", Product)) |>
    select(c(unique_id, everything())) |>
    select(-c(Client, Warehouse, Product))

  df <- pivot_longer(
    data = df_wide,
    cols = -unique_id,
    names_to = "ds",
    values_to = colname
  )

  if(startsWith(dataset, "winners")){
    names(df)[which(names(df) == "y")] <- dataset
  }

  return(df)
}

get_train_data <- function(df0, df1, col_name) {
  df <- bind_rows(df0, df1) |>
    arrange(unique_id, ds)

  df_clean <- df |>
    group_by(unique_id) |>
    mutate(cumsum = cumsum(.data[[col_name]])) |>
    filter(cumsum > 0) |>
    select(-cumsum) |>
    ungroup()

  df_clean$ds <- as.POSIXct(df_clean$ds)

  return(df_clean)
}

separate_series <- function(data, min_obs){
  # Select series that have at least min_obs observations
  data_long <- data |>
    group_by(unique_id) |>
    filter(n() >= min_obs) |>
    ungroup()

  data_short <- data |>
    group_by(unique_id) |>
    filter(n() < min_obs) |>
    ungroup()

  return(list(data_long = data_long, data_short = data_short))
}

vn1_competition_evaluation <- function(test, forecast, model){
  # Computes competition evaluation
  if(!is.character(forecast$ds)){
    forecast$ds <- format(forecast$ds,"%Y-%m-%d")
  }

  res <- merge(forecast, test, by=c("unique_id", "ds"))

  res <- res |>
    mutate(abs_err = abs(res[[model]]-res$y)) |>
    mutate(err = res[[model]]-res$y)

  abs_err = sum(res$abs_err, na.rm = TRUE)
  err = sum(res$err, na.rm = TRUE)
  score = abs_err+abs(err)
  score = score/sum(res$y)
  score = round(score, 4)

  return(score)
}

add_score <- function(model, score, scores_df){
  # Add new score to table
  scores_df <- scores_df |>
    add_row(Result = model, Score = score) |>
    arrange(Score) |>
    distinct()

  print(scores_df)

  return(scores_df)
}
