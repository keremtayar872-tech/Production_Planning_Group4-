# R/mod_box_jenkins.R
library(shiny)
library(readxl)
library(ggplot2)
library(forecast)

box_jenkins_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload Data (.xlsx or .csv)", accept = c(".xlsx", ".csv")),
        uiOutput(ns("col_select")),
        hr(),
        h4("Box-Jenkins (ARIMA) Model"),
        
        checkboxInput(ns("auto_arima"), "Auto-Select ARIMA Parameters (auto.arima)", value = TRUE),
        
        conditionalPanel(
          condition = sprintf("!input['%s']", ns("auto_arima")),
          numericInput(ns("p"), "AR Order (p):", value = 1, min = 0),
          numericInput(ns("d"), "Differencing (d):", value = 0, min = 0),
          numericInput(ns("q"), "MA Order (q):", value = 0, min = 0)
        ),
        
        hr(),
        numericInput(ns("horizon"), "Forecast Horizon (periods ahead):", value = 4, min = 1)
      ),
      
      mainPanel(
        h4("Diagnostic Plots (ACF & PACF)"),
        plotOutput(ns("acf_pacf_plot"), height = "300px"),
        hr(),
        h4("ARIMA Forecast Plot"),
        plotOutput(ns("plot")),
        hr(),
        h4("Parameter Estimates"),
        tableOutput(ns("coef_table")),
        hr(),
        h4("Model Error Metrics"),
        tableOutput(ns("metrics_table")),
        hr(),
        h4("Forecast Data Table"),
        tableOutput(ns("forecast_table"))
      )
    )
  )
}

box_jenkins_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    raw_data <- reactive({
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      if (ext == "xlsx") {
        readxl::read_excel(input$file$datapath)
      } else if (ext == "csv") {
        read.csv(input$file$datapath)
      } else {
        stop("Invalid file format")
      }
    })
    
    output$col_select <- renderUI({
      req(raw_data())
      ns <- session$ns
      numeric_cols <- names(raw_data())[sapply(raw_data(), is.numeric)]
      selectInput(ns("selected_col"), "Select Demand / Series Column", choices = numeric_cols)
    })
    
    output$acf_pacf_plot <- renderPlot({
      req(raw_data(), input$selected_col)
      y <- as.numeric(raw_data()[[input$selected_col]])
      y <- y[!is.na(y)]
      
      par(mfrow = c(1, 2))
      acf(y, main = "Autocorrelation (ACF)")
      pacf(y, main = "Partial Autocorrelation (PACF)")
    })
    
    results <- reactive({
      req(raw_data(), input$selected_col)
      y_raw <- as.numeric(raw_data()[[input$selected_col]])
      n_obs <- length(y_raw)
      h <- input$horizon
      
      if (input$auto_arima) {
        fit <- forecast::auto.arima(y_raw)
      } else {
        fit <- forecast::Arima(y_raw, order = c(input$p, input$d, input$q))
      }
      
      fitted_vals <- as.numeric(fitted(fit))
      fc <- forecast::forecast(fit, h = h)
      future_f <- as.numeric(fc$mean)
      
      metrics <- calc_metrics(y_raw, fitted_vals)
      
      df_historical <- data.frame(
        Period = 1:n_obs,
        Actual = round(y_raw, 2),
        Forecast = round(fitted_vals, 2)
      )
      
      df_future <- data.frame(
        Period = (n_obs + 1):(n_obs + h),
        Actual = NA,
        Forecast = round(future_f, 2)
      )
      
      df_full <- rbind(df_historical, df_future)
      
      coefs <- coef(fit)
      if (length(coefs) > 0) {
        se <- sqrt(diag(vcov(fit)))
        z_vals <- coefs / se
        p_vals <- 2 * (1 - pnorm(abs(z_vals)))
        coef_df <- data.frame(
          Parameter = names(coefs),
          Estimate = round(unname(coefs), 4),
          Std_Error = round(unname(se), 4),
          p_value = round(unname(p_vals), 4)
        )
      } else {
        coef_df <- data.frame(Parameter = "None", Estimate = NA, Std_Error = NA, p_value = NA)
      }
      
      list(
        df = df_full,
        metrics = metrics,
        coef_df = coef_df,
        model_name = as.character(fit)
      )
    })
    
    output$plot <- renderPlot({
      res <- results()
      ggplot(res$df, aes(x = Period)) +
        geom_line(aes(y = Actual, color = "Actual"), size = 1, na.rm = TRUE) +
        geom_line(aes(y = Forecast, color = "Forecast"), size = 1, linetype = "dashed") +
        scale_color_manual(values = c("Actual" = "black", "Forecast" = "blue")) +
        theme_minimal() +
        labs(y = "Value", color = "Legend", title = paste("ARIMA Model:", res$model_name))
    })
    
    output$coef_table <- renderTable({
      results()$coef_df
    })
    
    output$metrics_table <- renderTable({
      res <- results()
      data.frame(
        Metric = c("MAD", "MSE", "MAPE (%)"),
        Value = c(res$metrics$MAD, res$metrics$MSE, res$metrics$MAPE)
      )
    })
    
    output$forecast_table <- renderTable({
      results()$df
    })
    
  })
}