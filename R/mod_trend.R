# R/mod_trend.R
library(shiny)
library(readxl)
library(ggplot2)

trend_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload Data (.xlsx or .csv)", accept = c(".xlsx", ".csv")),
        uiOutput(ns("col_select")),
        hr(),
        h4("Holt's Linear Trend Model"),
        
        checkboxInput(ns("auto_params"), "Search Best Alpha & Beta (Optimize)", value = TRUE),
        
        conditionalPanel(
          condition = sprintf("!input['%s']", ns("auto_params")),
          sliderInput(ns("alpha"), "Level Alpha (α):", min = 0.01, max = 0.99, value = 0.2, step = 0.01),
          sliderInput(ns("beta"), "Trend Beta (β):", min = 0.01, max = 0.99, value = 0.1, step = 0.01)
        ),
        
        hr(),
        h4("Initial Values"),
        checkboxInput(ns("auto_initial"), "Fit OLS Regression for Initial Level (L0) & Slope (T0)", value = TRUE),
        
        conditionalPanel(
          condition = sprintf("!input['%s']", ns("auto_initial")),
          numericInput(ns("manual_l0"), "Initial Level (L0):", value = 100),
          numericInput(ns("manual_t0"), "Initial Slope (T0):", value = 0)
        ),
        
        hr(),
        numericInput(ns("horizon"), "Forecast Horizon (periods ahead):", value = 4, min = 1)
      ),
      
      mainPanel(
        h4("Holt's Method Forecast Plot"),
        plotOutput(ns("plot")),
        hr(),
        h4("Model & Error Metrics"),
        tableOutput(ns("metrics_table")),
        hr(),
        h4("Forecast & Component Data Table"),
        tableOutput(ns("forecast_table"))
      )
    )
  )
}

trend_server <- function(id) {
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
      y <- as.numeric(raw_data()[[input$selected_col]])
      n_obs <- length(y)
      
      # Determine Initial Values L0 and T0
      if (input$auto_initial) {
        init <- fit_initial_values(y)
        L0 <- init$level
        T0 <- init$slope
      } else {
        L0 <- input$manual_l0
        T0 <- input$manual_t0
      }
      
      # Determine Alpha and Beta
      alpha <- input$alpha
      beta <- input$beta
      
      if (input$auto_params) {
        opt <- optim(par = c(0.2, 0.1), fn = function(par) {
          a <- par[1]; b <- par[2]
          if (a <= 0 || a >= 1 || b <= 0 || b >= 1) return(Inf)
          L <- numeric(n_obs)
          T_vec <- numeric(n_obs)
          F_vec <- numeric(n_obs)
          
          L_prev <- L0; T_prev <- T0
          for (t in 1:n_obs) {
            F_vec[t] <- L_prev + T_prev
            L[t] <- a * y[t] + (1 - a) * (L_prev + T_prev)
            T_vec[t] <- b * (L[t] - L_prev) + (1 - b) * T_prev
            L_prev <- L[t]; T_prev <- T_vec[t]
          }
          mean((y - F_vec)^2, na.rm = TRUE)
        }, method = "L-BFGS-B", lower = c(0.01, 0.01), upper = c(0.99, 0.99))
        
        alpha <- opt$par[1]
        beta <- opt$par[2]
      }
      
      # Run Holt's Method
      L <- numeric(n_obs)
      T_vec <- numeric(n_obs)
      F_vec <- numeric(n_obs)
      
      L_prev <- L0
      T_prev <- T0
      
      for (t in 1:n_obs) {
        F_vec[t] <- L_prev + T_prev
        L[t] <- alpha * y[t] + (1 - alpha) * (L_prev + T_prev)
        T_vec[t] <- beta * (L[t] - L_prev) + (1 - beta) * T_prev
        L_prev <- L[t]
        T_prev <- T_vec[t]
      }
      
      # Future Horizon Forecasts
      h <- input$horizon
      future_f <- numeric(h)
      for (m in 1:h) {
        future_f[m] <- L[n_obs] + m * T_vec[n_obs]
      }
      
      metrics <- calc_metrics(y, F_vec)
      
      df_historical <- data.frame(
        Period = 1:n_obs,
        Actual = round(y, 2),
        Forecast = round(F_vec, 2),
        Level = round(L, 2),
        Trend = round(T_vec, 2)
      )
      
      df_future <- data.frame(
        Period = (n_obs + 1):(n_obs + h),
        Actual = NA,
        Forecast = round(future_f, 2),
        Level = NA,
        Trend = NA
      )
      
      df_full <- rbind(df_historical, df_future)
      
      list(
        df = df_full,
        metrics = metrics,
        alpha = round(alpha, 4),
        beta = round(beta, 4),
        L0 = round(L0, 2),
        T0 = round(T0, 2)
      )
    })
    
    output$plot <- renderPlot({
      res <- results()
      ggplot(res$df, aes(x = Period)) +
        geom_line(aes(y = Actual, color = "Actual"), size = 1, na.rm = TRUE) +
        geom_line(aes(y = Forecast, color = "Forecast"), size = 1, linetype = "dashed") +
        scale_color_manual(values = c("Actual" = "black", "Forecast" = "blue")) +
        theme_minimal() +
        labs(y = "Value", color = "Legend", title = paste0("Holt's Method (α=", res$alpha, ", β=", res$beta, ")"))
    })
    
    output$metrics_table <- renderTable({
      res <- results()
      data.frame(
        Parameter_Metric = c("Alpha (α)", "Beta (β)", "Initial Level (L0)", "Initial Slope (T0)", "MAD", "MSE", "MAPE (%)"),
        Value = c(res$alpha, res$beta, res$L0, res$T0, res$metrics$MAD, res$metrics$MSE, res$metrics$MAPE)
      )
    })
    
    output$forecast_table <- renderTable({
      results()$df
    })
    
  })
}