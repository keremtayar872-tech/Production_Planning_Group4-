# R/mod_agg_lp.R
library(shiny)
library(readxl)
library(ggplot2)
library(lpSolve)

agg_lp_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload Data (.xlsx or .csv)", accept = c(".xlsx", ".csv")),
        h5("Expected Columns: Period, Demand, Days"),
        hr(),
        h4("LP Model Options"),
        checkboxInput(ns("allow_ot"), "Allow Overtime", value = TRUE),
        checkboxInput(ns("allow_sub"), "Allow Subcontracting", value = TRUE),
        
        numericInput(ns("max_ot_unit"), "Max Overtime Capacity (units/period):", value = 200, min = 0),
        numericInput(ns("max_sub_unit"), "Max Subcontracting Capacity (units/period):", value = 300, min = 0),
        
        hr(),
        h4("Plant & Initial Data"),
        numericInput(ns("w0"), "Starting Workforce (W0):", value = 10, min = 0),
        numericInput(ns("i0"), "Starting Inventory (I0):", value = 0, min = 0),
        numericInput(ns("k_val"), "K (units/worker/day):", value = 2.5, min = 0.1, step = 0.1),
        
        hr(),
        h4("Unit Costs ($)"),
        numericInput(ns("c_hire"), "Hiring Cost ($/worker):", value = 500),
        numericInput(ns("c_fire"), "Firing Cost ($/worker):", value = 1000),
        numericInput(ns("c_hold"), "Holding Cost ($/unit/period):", value = 10),
        numericInput(ns("c_reg"), "Regular Labor Cost ($/worker/day):", value = 100),
        numericInput(ns("c_ot"), "Overtime Cost ($/unit):", value = 25),
        numericInput(ns("c_sub"), "Subcontracting Cost ($/unit):", value = 40)
      ),
      
      mainPanel(
        h4("Optimal LP Aggregate Plan"),
        tableOutput(ns("plan_table")),
        hr(),
        h4("Cumulative Demand vs Optimal Production Plot"),
        plotOutput(ns("cum_plot")),
        hr(),
        h4("Optimal Cost Breakdown"),
        tableOutput(ns("cost_table"))
      )
    )
  )
}

agg_lp_server <- function(id) {
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
    
    lp_solution <- reactive({
      req(raw_data())
      df <- raw_data()
      req("Period" %in% names(df), "Demand" %in% names(df), "Days" %in% names(df))
      
      n <- nrow(df)
      K <- input$k_val
      W0 <- input$w0
      I0 <- input$i0
      
      # Decision variables per period t:
      # W_t, H_t, F_t, O_t, S_t, I_t  (Total 6*n variables)
      num_vars <- 6 * n
      
      # Objective coefficients:
      # Cost per variable: W_t*(c_reg*Days_t), H_t*c_hire, F_t*c_fire, O_t*c_ot, S_t*c_sub, I_t*c_hold
      obj <- numeric(num_vars)
      for (t in 1:n) {
        idx <- (t - 1) * 6
        obj[idx + 1] <- input$c_reg * df$Days[t] # W_t
        obj[idx + 2] <- input$c_hire              # H_t
        obj[idx + 3] <- input$c_fire              # F_t
        obj[idx + 4] <- if (input$allow_ot) input$c_ot else 1e8  # O_t
        obj[idx + 5] <- if (input$allow_sub) input$c_sub else 1e8 # S_t
        obj[idx + 6] <- input$c_hold              # I_t
      }
      
      # Constraints:
      # 1. Workforce balance: W_t - W_{t-1} - H_t + F_t = 0
      # 2. Inventory balance: I_{t-1} + W_t*K*Days_t + O_t + S_t - I_t = Demand_t
      # 3. Capacity constraints for OT and Sub
      
      num_const <- 2 * n + (if (input$allow_ot) n else 0) + (if (input$allow_sub) n else 0)
      mat <- matrix(0, nrow = num_const, ncol = num_vars)
      dir <- character(num_const)
      rhs <- numeric(num_const)
      
      row_idx <- 1
      
      # Workforce balance
      for (t in 1:n) {
        idx <- (t - 1) * 6
        mat[row_idx, idx + 1] <- 1  # W_t
        mat[row_idx, idx + 2] <- -1 # H_t
        mat[row_idx, idx + 3] <- 1  # F_t
        if (t > 1) {
          prev_idx <- (t - 2) * 6
          mat[row_idx, prev_idx + 1] <- -1 # -W_{t-1}
          rhs[row_idx] <- 0
        } else {
          rhs[row_idx] <- W0
        }
        dir[row_idx] <- "="
        row_idx <- row_idx + 1
      }
      
      # Inventory balance
      for (t in 1:n) {
        idx <- (t - 1) * 6
        mat[row_idx, idx + 1] <- K * df$Days[t] # Regular production W_t * K * Days_t
        mat[row_idx, idx + 4] <- 1              # O_t
        mat[row_idx, idx + 5] <- 1              # S_t
        mat[row_idx, idx + 6] <- -1             # -I_t
        
        if (t > 1) {
          prev_idx <- (t - 2) * 6
          mat[row_idx, prev_idx + 6] <- 1       # +I_{t-1}
          rhs[row_idx] <- df$Demand[t]
        } else {
          rhs[row_idx] <- df$Demand[t] - I0
        }
        dir[row_idx] <- "="
        row_idx <- row_idx + 1
      }
      
      # OT Max Limit
      if (input$allow_ot) {
        for (t in 1:n) {
          idx <- (t - 1) * 6
          mat[row_idx, idx + 4] <- 1 # O_t
          dir[row_idx] <- "<="
          rhs[row_idx] <- input$max_ot_unit
          row_idx <- row_idx + 1
        }
      }
      
      # Sub Max Limit
      if (input$allow_sub) {
        for (t in 1:n) {
          idx <- (t - 1) * 6
          mat[row_idx, idx + 5] <- 1 # S_t
          dir[row_idx] <- "<="
          rhs[row_idx] <- input$max_sub_unit
          row_idx <- row_idx + 1
        }
      }
      
      sol <- lp("min", obj, mat, dir, rhs)
      
      res_vars <- sol$solution
      res_df <- df
      res_df$Workers <- 0
      res_df$Hired <- 0
      res_df$Fired <- 0
      res_df$Regular_Prod <- 0
      res_df$Overtime <- 0
      res_df$Subcontracting <- 0
      res_df$Inventory <- 0
      
      for (t in 1:n) {
        idx <- (t - 1) * 6
        res_df$Workers[t] <- round(res_vars[idx + 1], 2)
        res_df$Hired[t]   <- round(res_vars[idx + 2], 2)
        res_df$Fired[t]   <- round(res_vars[idx + 3], 2)
        res_df$Regular_Prod[t] <- round(res_df$Workers[t] * K * df$Days[t], 2)
        res_df$Overtime[t] <- round(res_vars[idx + 4], 2)
        res_df$Subcontracting[t] <- round(res_vars[idx + 5], 2)
        res_df$Inventory[t] <- round(res_vars[idx + 6], 2)
      }
      
      costs <- list(
        hire = input$c_hire, fire = input$c_fire, hold = input$c_hold,
        regular_day = input$c_reg, overtime = input$c_ot, subcontract = input$c_sub
      )
      
      cost_df <- calc_agg_costs(res_df, costs)
      
      list(df = res_df, cost_df = cost_df)
    })
    
    output$plan_table <- renderTable({
      lp_solution()$df
    }, digits = 2)
    
    output$cost_table <- renderTable({
      lp_solution()$cost_df
    }, digits = 2)
    
    output$cum_plot <- renderPlot({
      res <- lp_solution()
      df <- res$df
      
      cum_demand <- cumsum(df$Demand)
      cum_prod <- cumsum(df$Regular_Prod + df$Overtime + df$Subcontracting)
      
      plot_df <- data.frame(
        Period = factor(df$Period, levels = df$Period),
        Cum_Demand = cum_demand,
        Cum_Prod = cum_prod
      )
      
      ggplot(plot_df, aes(x = Period, group = 1)) +
        geom_line(aes(y = Cum_Demand, color = "Cumulative Demand"), size = 1.2) +
        geom_line(aes(y = Cum_Prod, color = "Optimal Cum. Production"), size = 1, linetype = "dashed") +
        scale_color_manual(values = c("Cumulative Demand" = "black", "Optimal Cum. Production" = "blue")) +
        theme_minimal() +
        labs(y = "Units", color = "Legend", title = "LP Model: Cumulative Demand vs Total Production")
    })
    
  })
}