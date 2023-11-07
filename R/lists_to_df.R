#' Create a list of data frames from list.
#'
#' Break down a set of data frames by either a metric, model, or set.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param metrics_list (list of metrics per model) The metrics associated with
#' each metric and model and split.
#' @param column (string) The column indicator to create each list element.
#' @return df_list (list of data frames) The list of data frames for each
#' unique element in the column argument.
#'
#' @export lists_to_df

lists_to_df = function(metrics_list, column = "metric") {
  values = unlist(metrics_list)
  keys = strsplit(names(values), '[.]')

  df = as.data.frame(do.call(rbind, keys))
  names(df) = c("model", "set", "metric")
  df$value = unlist(metrics_list)

  column_index = which(names(df) == column)
  unique_column = unique(df[, column_index])
  df_list = list()
  for (i in unique_column) {
    df_list[[i]] = df[which(df[, column_index] == i), -column_index]
  }
  df_list
}
