# R/mod_seasonal.R
library(shiny)
library(readxl)
library(ggplot2)

seasonal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload Data (.xlsx or .csv)", accept = c(".xlsx", ".csv")),
        uiOutput(ns("col_select")),
        hr(),
        h4("Seasonal Models"),
        selectInput(ns("method"), "Select Method", choices = c(
          "Seasonal Decomposition (Classical)" = "decomp",
          "Winters' Additive Method" = "winters_add",
          "Winters' Multiplicative Method" = "winters_mult"
        )),
        numericInput(ns("period"), "Seasonal Period (s):", value = 4, min = 2),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'decomp'", ns("method")),
          selectInput(ns("decomp_type"), "Decomposition Type:", choices = c("Additive" = "additive", "Multiplicative" = "multiplicative"))
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] != 'decomp'", ns("method")),
          checkboxInput(ns("auto_params"), "Search Best Alpha, Beta, Gamma (Optimize)", value = TRUE),
          conditionalPanel(
            condition = sprintf("!input['%s']", ns("auto_params")),
            sliderInput(ns("alpha"), "Alpha (α - Level):", min = 0.01, max = 0.99, value = 0.2, step = 0.01),
            sliderInput(ns("beta"), "Beta (β - Trend):", min = 0.01, max = 0.99, value = 0.1, step = 0.01),
            sliderInput(ns("gamma"), "Gamma (γ - Season):", min = 0.01, max = 0.99, value = 0.1, step = 0.01)
          )
        ),
        
        hr(),
        numericInput(ns("horizon"), "Forecast Horizon (periods ahead):", value = 4, min = 1)
      ),
      
      mainPanel(
        h4("Seasonal Forecast Plot"),
        plotOutput(ns("plot")),
        hr(),
        h4("Model & Error Metrics"),
        tableOutput(ns("metrics_table")),
        hr(),
        h4("Forecast Data Table"),
        tableOutput(ns("forecast_table"))
      )
    )
  )
}

seasonal_server <- function(id) {
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
    
    results <- reactive({
      req(raw_data(), input$selected_col)
      y_raw <- as.numeric(raw_data()[[input$selected_col]])
      n_obs <- length(y_raw)
      s <- input$period
      h <- input$horizon
      
      req(n_obs >= 2 * s) # At least 2 periods required for seasonality
      
      ts_data <- ts(y_raw, frequency = s)
      
      fit_vals <- rep(NA, n_obs)
      future_f <- rep(NA, h)
      alpha_val <- NA; beta_val <- NA; gamma_val <- NA
      
      if (input$method == "decomp") {
        decomp <- decompose(ts_data, type = input$decomp_type)
        trend <- as.numeric(decomp$trend)
        seasonal <- as.numeric(decomp$seasonal)
        
        t_idx <- 1:n_obs
        lm_trend <- lm(trend ~ t_idx, na.action = na.exclude)
        
        if (input$decomp_type == "additive") {
          fit_vals <- trend + seasonal
          future_trend <- predict(lm_trend, newdata = data.frame(t_idx = (n_obs + 1):(n_obs + h)))
          future_seas <- rep(seasonal[1:s], length.out = n_obs + h)[(n_obs + 1):(n_obs + h)]
          future_f <- future_trend + future_seas
        } else {
          fit_vals <- trend * seasonal
          future_trend <- predict(lm_trend, newdata = data.frame(t_idx = (n_obs + 1):(n_obs + h)))
          future_seas <- rep(seasonal[1:s], length.out = n_obs + h)[(n_obs + 1):(n_obs + h)]
          future_f <- future_trend * future_seas
        }
      } else {
        is_seasonal_type <- ifelse(input$method == "winters_add", "additive", "multiplicative")
        
        if (input$auto_params) {
          hw_fit <- HoltWinters(ts_data, seasonal = is_seasonal_type)
          alpha_val <- unname(hw_fit$alpha)
          beta_val <- unname(hw_fit$beta)
          gamma_val <- unname(hw_fit$gamma)
        } else {
          hw_fit <- HoltWinters(
            ts_data, 
            alpha = input$alpha, 
            beta = input$beta, 
            gamma = input$gamma, 
            seasonal = is_seasonal_type
          )
          alpha_val <- input$alpha
          beta_val <- input$beta
          gamma_val <- input$gamma
        }
        
        fitted_matrix <- hw_fit$fitted
        fit_series <- as.numeric(fitted_matrix[, "xhat"])
        pad_len <- n_obs - length(fit_series)
        fit_vals <- c(rep(NA, pad_len), fit_series)
        
        pred <- predict(hw_fit, n.ahead = h)
        future_f <- as.numeric(pred)
      }
      
      metrics <- calc_metrics(y_raw, fit_vals)
      
      df_historical <- data.frame(
        Period = 1:n_obs,
        Actual = round(y_raw, 2),
        Forecast = round(fit_vals, 2)
      )
      
      df_future <- data.frame(
        Period = (n_obs + 1):(n_obs + h),
        Actual = NA,
        Forecast = round(future_f, 2)
      )
      
      df_full <- rbind(df_historical, df_future)
      
      list(
        df = df_full,
        metrics = metrics,
        alpha = ifelse(is.na(alpha_val), "N/A", round(alpha_val, 4)),
        beta = ifelse(is.na(beta_val), "N/A", round(beta_val, 4)),
        gamma = ifelse(is.na(gamma_val), "N/A", round(gamma_val, 4))
      )
    })
    
    output$plot <- renderPlot({
      res <- results()
      ggplot(res$df, aes(x = Period)) +
        geom_line(aes(y = Actual, color = "Actual"), size = 1, na.rm = TRUE) +
        geom_line(aes(y = Forecast, color = "Forecast"), size = 1, linetype = "dashed") +
        scale_color_manual(values = c("Actual" = "black", "Forecast" = "blue")) +
        theme_minimal() +
        labs(y = "Value", color = "Legend", title = "Seasonal Series Forecast")
    })
    
    output$metrics_table <- renderTable({
      res <- results()
      data.frame(
        Parameter_Metric = c("Alpha (α)", "Beta (β)", "Gamma (γ)", "MAD", "MSE", "MAPE (%)"),
        Value = c(as.character(res$alpha), as.character(res$beta), as.character(res$gamma), 
                  res$metrics$MAD, res$metrics$MSE, res$metrics$MAPE)
      )
    })
    
    output$forecast_table <- renderTable({
      results()$df
    })
    
  })
}