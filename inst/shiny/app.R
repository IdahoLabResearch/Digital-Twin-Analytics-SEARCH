library(shiny)
library(search)
library(plotly)
library(ggplot2)
library(ggdendro)
library(shinythemes)
library(DT)
options(shiny.host = '0.0.0.0')
options(shiny.port = 8888)

ui = navbarPage(
  "SEARCH", theme = shinytheme("united"),
  tags$head(tags$style(HTML('.navbar-static-top {background-color: #07519E;}',
                            '.navbar-default .navbar-nav>.active>a {background-color: #07519E;}'))),
  tabPanel(
    "Input",
    fluidRow(
      column(3,fileInput(inputId = "data_file", label = "Data", accept = ".csv")),

      column(3, uiOutput(outputId = "ui_response")),
      column(3, numericInput(inputId = "num_splits",
                             label = "Number of iterations",
                             value = 5)),
      column(3, actionButton("build_models", label = "Build!"))
    ),

    plotlyOutput("plot_response"),
    fluidRow(
      column(6, plotlyOutput("plot_data", height = 500)),
      column(6, dataTableOutput("table_summary"))
    ),
    textOutput("build_time")
  ),
  tabPanel(
    "Results",
    fluidRow(
      column(6, plotlyOutput("plot_violin_r2", height = 500)),
      column(6, dataTableOutput("table_r2"))
    ),
    fluidRow(
      column(6, plotlyOutput("plot_violin_rmse", height = 500)),
      column(6, dataTableOutput("table_rmse"))
    ),
    fluidRow(
      column(6, plotlyOutput("plot_violin_mape", height = 500)),
      column(6, dataTableOutput("table_mape"))
    ),
    fluidRow(
      column(6, plotlyOutput("plot_violin_smape", height = 500)),
      column(6, dataTableOutput("table_smape"))
    )
  ),
  tabPanel(
    "Information",
    h3("Potential Anomalies"),
    plotlyOutput("plot_outliers", height = 500),
    h3("Important Variables"),
    plotlyOutput("plot_enet", height = 500),
    plotlyOutput("plot_rf_vif", height = 500),
    h3("Eigen Structure"),
    plotlyOutput("plot_eigen", height = 500),
    plotlyOutput("plot_components", height = 500),
    plotlyOutput("plot_scores", height = 500),
    h3("Residual Information"),
    plotlyOutput("plot_residuals", height = 500)
  ),
  tabPanel(
    "Glossary",
    dataTableOutput("explain_methods")
  )
)


# Define server logic required to draw a histogram
server = function(input, output) {
  df = reactive({
    if(is.null(input$data_file)) {
      return()
    }
    read.csv(input$data_file$datapath)
  })

  model_values = eventReactive(input$build_models, {
    if(is.null(input$response)) {
      return()
    }

    y = df()[[input$response]]
    x = df()[, -which(names(df()) %in% input$response)] #input$exclude_predictors


    tmp_data = list()
    startTime = proc.time()
    withProgress(message = 'Building', value = 0, {
      for (i in 1:input$num_splits){
        tmp_models = holistic_models(x, y)
        tmp_data[[i]] = tmp_models$result
        incProgress(i/input$num_splits, detail = "Building...")
      }
      #tmp_data = holistic_splits(x, y, input$num_splits) # parallel version
    })
    endTime = as.numeric((proc.time() - startTime)[3])

    list(results = tmp_data, fits = tmp_models$fits, build_time = endTime,
         var_names = names(x), resids = tmp_models$resids,
         fitted = tmp_models$fitted)
  })

  output$build_time = renderText({
    if(is.null(model_values())) {
      return()
    }

    paste0("Total build time: ", model_values()$build_time)
  })


  # Response select
  output$ui_response = renderUI({
    if(is.null(df())) {
      return()
    }

    fluidRow(
      selectInput(
        inputId = "response", label = "Set Response",
        choices = names(df()), multiple = FALSE
      )
    )
  })
    # output$ui_exclude_predictors = renderUI({
  #   if(is.null(df())) {
  #     return()
  #   }
  #
  #   fluidRow(
  #     selectInput(
  #       inputId = "exclude_predictors", label = "Exclude Predictors",
  #       choices = names(df()), multiple = TRUE
  #     )
  #   )
  # })

  output$table_summary = renderDataTable({
    if(is.null(df())) {
      return()
    }

    summary_table = as.data.frame(t(as.data.frame(apply(df(), 2, summary))))
    summary_table$index = 1:nrow(summary_table)

    datatable(summary_table, options = list(pageLength = 10))
  })


  output$plot_response = renderPlotly({
    if (is.null(input$response)) {
      return()
    }

    tmp_df = data.frame(
      Response = df()[[input$response]]
    )
    tmp_df$Index = 1:nrow(tmp_df)

    plot_ly(tmp_df, x = ~Index, y = ~Response,
            type = "scatter", mode = "markers")
  })

  output$plot_data = renderPlotly({
    if (is.null(df())) {
      return()
    }

    clust = protoclust::protoclust(dist(scale(df())))
    clust = as.dendrogram(clust)
    p = ggdendrogram(clust, rotate = FALSE, size = 2)

    ggplotly(p)
  })

  output$plot_violin_rmse = renderPlotly({
    if(is.null(model_values())) {
      return()
    }

    all_metrics = lists_to_df(model_values()$results, "metric")

    fig = plot_violin_metric(all_metrics$rmse)
    fig %>% layout(title = "The root mean square error values per model.")
  })

  output$table_rmse = renderDataTable({
    if(is.null(model_values()$results)) {
      return()
    }

    all_summary = list_to_summary(model_values()$results)
    datatable(all_summary$rmse, options = list(pageLength = 10))
  })

  output$plot_violin_r2 = renderPlotly({
    if(is.null(model_values())) {
      return()
    }

    all_metrics = lists_to_df(model_values()$results, "metric")

    fig = plot_violin_metric(all_metrics$r2)
    fig %>% layout(title = "The R2 (coefficient of determination) values per model.")
  })

  output$table_r2 = renderDataTable({
    if(is.null(model_values())) {
      return()
    }

    all_summary = list_to_summary(model_values()$results)
    datatable(all_summary$r2, options = list(pageLength = 10))
  })


  output$plot_violin_mape = renderPlotly({
    if(is.null(model_values())) {
      return()
    }

    all_metrics = lists_to_df(model_values()$results, "metric")

    fig = plot_violin_metric(all_metrics$mape)
    fig %>% layout(title = "The mape values per model.")
  })

  output$table_mape = renderDataTable({
    if(is.null(model_values())) {
      return()
    }

    all_summary = list_to_summary(model_values()$results)
    datatable(all_summary$mape, options = list(pageLength = 10))
  })

  output$plot_violin_smape = renderPlotly({
    if(is.null(model_values())) {
      return()
    }

    all_metrics = lists_to_df(model_values()$results, "metric")

    fig = plot_violin_metric(all_metrics$smape)
    fig %>% layout(title = "The smape values per model.")
  })

  output$table_smape = renderDataTable({
    if(is.null(model_values())) {
      return()
    }

    all_summary = list_to_summary(model_values()$results)
    datatable(all_summary$smape, options = list(pageLength = 10))
  })



  output$explain_methods = renderDataTable({
    if(is.null(model_values())) {
      return()
    }

    fits = model_values()$fits
    tmp_df = data.frame(
      name = names(fits)
    )
    init_val = rep("a", nrow(tmp_df))
    tmp_df$model = init_val
    tmp_df$tag = init_val
    tmp_df$descrip = init_val
    tmp_df$pros = init_val
    tmp_df$cons = init_val
    tmp_df$assumptions = init_val

    for (i in names(fits)) {
      row_index = which(tmp_df$name == i)
      tmp_doc = holistic_model(fits[[i]]) 
      tmp_df$model[row_index] = paste(unlist(tmp_doc["model"]), collapse = "... ") #  getElement(tmp_doc, "model")
      tmp_df$tag[row_index] = paste(unlist(tmp_doc["tag"]), collapse = "... ") #  getElement(tmp_doc, "tag")
      tmp_df$descrip[row_index] = paste(unlist(tmp_doc["descrip"]), collapse = "... ") #  getElement(tmp_doc, "descrip")
      tmp_df$pros[row_index] = paste(unlist(tmp_doc["pros"]), collapse = "... ") # getElement(tmp_doc, "pros")
      tmp_df$cons[row_index] = paste(unlist(tmp_doc["cons"]), collapse = "... ") #  getElement(tmp_doc, "cons")
      tmp_df$assumptions[row_index] = paste(unlist(tmp_doc["assumptions"]), collapse = "... ") #  getElement(tmp_doc, "assumptions")
    }

    datatable(tmp_df, options = list(pageLength = 50))
  })



  output$plot_enet = renderPlotly({
    if(is.null(model_values())) {
      return()
    }
    fits = model_values()$fits
    lasso = fits[["LASSO"]]
    ridge = fits[["RIDGE"]]
    enet = fits[["ENET"]]
    tmp_df = data.frame(
      LASSO = lasso$glmnet.fit$beta[, which(lasso$lambda == lasso$lambda.1se)],
      RIDGE = ridge$glmnet.fit$beta[, which(ridge$lambda == ridge$lambda.1se)],
      ENET = enet$glmnet.fit$beta[, which(enet$lambda == enet$lambda.1se)],
      Variable = model_values()$var_names
    )
    tmp_df$Index = 1:nrow(tmp_df)


    p = plot_ly(tmp_df, x = ~Index, y = ~RIDGE, name = 'RIDGE',
                type = 'scatter', mode = 'markers', text = ~Variable)
    p = p %>% add_trace(y = ~ENET, name = 'ENET', mode = 'markers')
    p = p %>% add_trace(y = ~LASSO, name = 'LASSO', mode = 'markers')
    p = p %>% layout(title = "Regression Coefficients per Variable",
               xaxis = list(title = "Variable Index"),
               yaxis = list (title = "Coefficient"))
    p = p %>% layout(hovermode = "x unified")

    p
  })


  output$plot_scores = renderPlotly({
    if(is.null(model_values())) {
      return()
    }
    fits = model_values()$fits
    pcr = fits[["PCR"]]
    tmp_df = scale(attr(pcr, "scores"))
    tmp_df = as.data.frame(tmp_df)
    names(tmp_df) = paste0("PC_", 1:ncol(tmp_df))
    tmp_df$Index = 1:nrow(tmp_df)


    p = plot_ly(tmp_df, x = ~Index, y = ~PC_1, name = "PC_1",
                type = 'scatter', mode = 'markers')
    if (ncol(tmp_df) > 2) {
      for (i in 2:(ncol(tmp_df) - 1)) {
        p = p %>% add_trace(y = tmp_df[, i], name = names(tmp_df)[i],
                            mode = 'markers')
      }
    }

    p = p %>% layout(title = "Dimension Reduction of the Data",
                     xaxis = list(title = "Index"),
                     yaxis = list (title = "Component"))
    p = p %>% layout(hovermode = "x unified")

    p
  })

  output$plot_eigen = renderPlotly({
    if(is.null(model_values())) {
      return()
    }
    fits = model_values()$fits
    pcr = fits[["PCR"]]
    tmp_df = data.frame(
      Eigenvalues = attr(pcr, "eigenvalues")
    )
    tmp_df$Index = 1:nrow(tmp_df)

    vline = function(x = 0, color = "red") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color)
      )
    }

    p = plot_ly(tmp_df, x = ~Index, y = ~Eigenvalues, name = "Eigenvalues",
                type = 'scatter', mode = 'markers')

    p = p %>% layout(title = "Eigenvalues of the data",
                     xaxis = list(title = "Index"),
                     yaxis = list (title = "Eigenvalue"),
                     shapes = list(vline(ncol(attr(pcr, "scores")))))

    p
  })

  output$plot_components = renderPlotly({
    if(is.null(model_values())) {
      return()
    }
    fits = model_values()$fits

    pcr = fits[["PCR"]]
    tmp_df = scale(attr(pcr, "scores"))
    tmp_df = as.data.frame(tmp_df)
    names(tmp_df) = paste0("PC_", 1:ncol(tmp_df))

    p = plot_ly(tmp_df, x = ~PC_1, y = ~PC_2,
                type = 'scatter', mode = 'markers')

    p = p %>% layout(title = "Comparison of the First Two Components",
                     xaxis = list(title = "PC1"),
                     yaxis = list (title = "PC2"))

    p
  })


  output$plot_outliers = renderPlotly({
    if (is.null(input$response)) {
      return()
    }

    tmp_df = data.frame(
      Response = df()[[input$response]]
    )
    tmp_df$Index = 1:nrow(tmp_df)
    tmp_df$ts = 0
    tmp_df$norm = 0
    tmp_df$svm = 0

    x = exploreDetectAnomaly(tmp_df$Response)
    if (length(x$ts) > 0) {
      tmp_df$ts[x$ts] = tmp_df$Response[x$ts]
    }

    if (length(x$norm) > 0) {
      tmp_df$norm[x$norm] = tmp_df$Response[x$norm]
    }

    if (length(x$svm) > 0) {
      tmp_df$svm[x$svm] = tmp_df$Response[x$svm]
    }

    p = plot_ly(tmp_df, x = ~Index, y = ~Response,
            type = "scatter", mode = "markers", name = "Response")
    p = p %>% add_trace(y = ~ts, name = 'ts', mode = 'markers')
    p = p %>% add_trace(y = ~norm, name = 'norm', mode = 'markers')
    p = p %>% add_trace(y = ~svm, name = "svm", mode = "markers")
    p = p %>% layout(title = "Potential Outliers",
                     xaxis = list(title = "Sample Index"),
                     yaxis = list (title = "Value"))
    p = p %>% layout(hovermode = "x unified")
    p
  })

  output$plot_residuals = renderPlotly({
    if(is.null(model_values())) {
      return()
    }
    resids = model_values()$resids
    fitted_vals = model_values()$fitted

    p = plot_ly(x = fitted_vals[[names(resids)[1]]],
                y = resids[[names(resids)[1]]],
                name = names(resids)[1], type = 'scatter', mode = 'markers')
    if (length(resids) > 2) {
      for (i in names(resids)[-1]) {
        p = p %>% add_trace(x = fitted_vals[[i]],
                            y = resids[[i]],
                            name = i, mode = 'markers')
      }
    }

    p = p %>% layout(title = "Comparison of the Training Residuals",
                     xaxis = list(title = "Fitted"),
                     yaxis = list (title = "Residuals"))
    p = p %>% layout(hovermode = "x unified")

    p
  })

  output$plot_rf_vif = renderPlotly({
    if(is.null(model_values())) {
      return()
    }

    fits = model_values()$fits
    rffit = fits[["RF"]]
    class(rffit) = rffit$orig_class

    impvals = randomForest::importance(rffit)[,1]
    tmp_df = data.frame(
      Importance = impvals,
      Variable = model_values()$var_names
    )
    tmp_df$Index = 1:nrow(tmp_df)

    tmp_df = tmp_df[order(tmp_df$Importance, decreasing = TRUE), ]
    tmp_df = tmp_df[1:10, ]

    markervals = list(
      color = tmp_df$Importance,
      colorscale = "Viridis",
      reversescale = TRUE
    )

    p = plot_ly(tmp_df, x = ~Importance, y = ~Variable, type = "bar",
                 orientation = "h", marker = markervals)
    p = p %>% layout(title = "Random Forest Variable Importance")

    p
  })



}

# Run the application
shinyApp(ui = ui, server = server)
