# R/mod_stationary.R
library(shiny)
library(readxl)
library(ggplot2)

stationary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload Data (.xlsx or .csv)", accept = c(".xlsx", ".csv")),
        uiOutput(ns("col_select")),
        hr(),
        selectInput(ns("method"), "Select Method", choices = c(
          "Simple Moving Average (SMA)" = "sma",
          "Weighted Moving Average (WMA)" = "wma",
          "Simple Exponential Smoothing (SES)" = "ses",
          "Seasonal Factors (Quick & Dirty)" = "seasonal_quick"
        )),
        
        # Method-specific inputs
        conditionalPanel(
          condition = sprintf("input['%s'] == 'sma'", ns("method")),
          numericInput(ns("sma_n"), "Window Size (n):", value = 3, min = 1)
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'wma'", ns("method")),
          numericInput(ns("wma_n"), "Window Size (n):", value = 3, min = 1),
          textInput(ns("wma_weights"), "Weights (comma separated, sum=1):", value = "0.5, 0.3, 0.2")
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'ses'", ns("method")),
          checkboxInput(ns("ses_auto"), "Search Best Alpha (Optimize)", value = TRUE),
          conditionalPanel(
            condition = sprintf("!input['%s']", ns("ses_auto")),
            sliderInput(ns("ses_alpha"), "Alpha (α):", min = 0.01, max = 0.99, value = 0.2, step = 0.01)
          )
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'seasonal_quick'", ns("method")),
          numericInput(ns("season_period"), "Season Length (s):", value = 4, min = 2)
        )
      ),
      
      mainPanel(
        h4("Forecast Plot"),
        plotOutput(ns("plot")),
        hr(),
        h4("Error Metrics"),
        tableOutput(ns("metrics_table")),
        hr(),
        h4("Forecast Data Table"),
        tableOutput(ns("forecast_table"))
      )
    )
  )
}

stationary_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive: Read Data
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
    
    # UI Column Selector
    output$col_select <- renderUI({
      req(raw_data())
      ns <- session$ns
      numeric_cols <- names(raw_data())[sapply(raw_data(), is.numeric)]
      selectInput(ns("selected_col"), "Select Demand / Series Column", choices = numeric_cols)
    })
    
    # Reactive Calculations
    results <- reactive({
      req(raw_data(), input$selected_col)
      y <- as.numeric(raw_data()[[input$selected_col]])
      n_obs <- length(y)
      fit <- rep(NA, n_obs)
      
      if (input$method == "sma") {
        k <- input$sma_n
        if (n_obs >= k) {
          for (t in (k + 1):n_obs) {
            fit[t] <- mean(y[(t - k):(t - 1)])
          }
        }
      } else if (input$method == "wma") {
        k <- input$wma_n
        w <- as.numeric(unlist(strsplit(input$wma_weights, ",")))
        w <- w / sum(w)
        if (n_obs >= k && length(w) == k) {
          for (t in (k + 1):n_obs) {
            fit[t] <- sum(y[(t - k):(t - 1)] * rev(w))
          }
        }
      } else if (input$method == "ses") {
        alpha <- input$ses_alpha
        if (input$ses_auto) {
          opt <- optim(par = 0.2, fn = function(a) {
            if (a <= 0 || a >= 1) return(Inf)
            f <- numeric(n_obs)
            f[1] <- y[1]
            for (t in 2:n_obs) f[t] <- a * y[t - 1] + (1 - a) * f[t - 1]
            mean((y[2:n_obs] - f[2:n_obs])^2, na.rm = TRUE)
          }, method = "L-BFGS-B", lower = 0.01, upper = 0.99)
          alpha <- opt$par
        }
        fit[1] <- y[1]
        for (t in 2:n_obs) {
          fit[t] <- alpha * y[t - 1] + (1 - alpha) * fit[t - 1]
        }
      } else if (input$method == "seasonal_quick") {
        s <- input$season_period
        num_seasons <- floor(n_obs / s)
        if (num_seasons >= 1) {
          mat <- matrix(y[1:(num_seasons * s)], ncol = s, byrow = TRUE)
          season_means <- colMeans(mat)
          overall_mean <- mean(mat)
          indices <- season_means / overall_mean
          fit <- rep(overall_mean, n_obs) * rep(indices, length.out = n_obs)
        }
      }
      
      metrics <- calc_metrics(y, fit)
      df <- data.frame(Period = 1:n_obs, Actual = y, Forecast = round(fit, 2))
      
      list(df = df, metrics = metrics)
    })
    
    # Plot Output
    output$plot <- renderPlot({
      res <- results()
      ggplot(res$df, aes(x = Period)) +
        geom_line(aes(y = Actual, color = "Actual"), size = 1) +
        geom_line(aes(y = Forecast, color = "Forecast"), size = 1, linetype = "dashed") +
        scale_color_manual(values = c("Actual" = "black", "Forecast" = "blue")) +
        theme_minimal() +
        labs(y = "Value", color = "Legend")
    })
    
    # Metrics Table
    output$metrics_table <- renderTable({
      res <- results()
      data.frame(
        Metric = c("MAD", "MSE", "MAPE (%)"),
        Value = c(res$metrics$MAD, res$metrics$MSE, res$metrics$MAPE)
      )
    })
    
    # Forecast Data Table
    output$forecast_table <- renderTable({
      results()$df
    })
    
  })
}