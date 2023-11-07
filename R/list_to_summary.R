#' Create a list of summary tables from list.
#'
#' Break down a set of summary data frames by each metric, model, and set.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param metrics_list (list of metrics per model) The metrics associated with
#' each metric and model and split.
#' @param digits (string) The number of digits to round to for each metric.
#' @return df_list (list of data frames) The list of summary data frames.
#'
#' @export list_to_summary

list_to_summary = function(metrics_list, digits = 3) {
  by_metric = lists_to_df(metrics_list, "metric")
  unique_metrics = names(by_metric)
  unique_models = unique(by_metric[[1]]$model)

  init_df = data.frame(
    "Model" = rep(unique_models, each = 2),
    "Set" = rep(c("train", "test"), length(unique_models))
  )
  init_df$Min = 0
  init_df$`1st Qu` = 0
  init_df$Median = 0
  init_df$Mean = 0
  init_df$`3rd Qu` = 0
  init_df$Max = 0

  df_list = list()
  for (i in unique_metrics) {
    tmp_metrics = by_metric[[i]]
    df_list[[i]] = init_df
    for (j in unique_models) {
      tmp_model = tmp_metrics[which(tmp_metrics$model == j), ]
      train = round(
        summary(tmp_model$value[which(tmp_model$set == "train")]),
        digits = digits
      )
      test = round(
        summary(tmp_model$value[which(tmp_model$set == "test")]),
        digits = digits
      )
      df_list[[i]][which(init_df$Model == j), 3:8] = rbind(train, test)
    }
  }

  df_list
}
