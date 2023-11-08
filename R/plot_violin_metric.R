#' Plot violin metric
#'
#' Plot violin metrics for a given data frame with the following column names:
#' model, value, and set. Model indicates the machine learning model used,
#' value is the corresponding value of the metric used (e.g., rmse), and set
#' determines if the metric was from the training or test set.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#' 
#' @param df (data frame) The numeric values to be scaled.
#' @return fig (plotly figure) The descaled values of x.
#'
#' @import plotly
#' @export plot_violin_metric

plot_violin_metric = function(df) {
  fig = df %>% plot_ly(type = 'violin')
  fig = fig %>%
    add_trace(
      x = ~model[df$set == 'train'], y = ~value[df$set == 'train'],
      legendgroup = 'train', scalegroup = 'train', name = 'train',
      side = 'negative', box = list(visible = T),
      meanline = list(visible = T), color = I("blue")
    )
  fig = fig %>%
    add_trace(
      x = ~model[df$set == 'test'], y = ~value[df$set == 'test'],
      legendgroup = 'test', scalegroup = 'test', name = 'test',
      side = 'positive', box = list(visible = T),
      meanline = list(visible = T), color = I("red")
    )

  fig = fig %>%
    layout(
      xaxis = list(title = ""), yaxis = list(title = "", zeroline = F),
      violingap = 0, violingroupgap = 0, violinmode = 'overlay'
    )

  fig
}
