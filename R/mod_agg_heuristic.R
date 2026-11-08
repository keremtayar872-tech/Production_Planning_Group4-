# R/mod_agg_heuristic.R
library(shiny)
library(readxl)
library(ggplot2)

agg_heuristic_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload Data (.xlsx or .csv)", accept = c(".xlsx", ".csv")),
        h5("Expected Columns: Period, Demand, Days"),
        hr(),
        
        selectInput(ns("plan_type"), "Select Heuristic Plan", choices = c(
          "Zero-Inventory (Chase Plan)" = "chase",
          "Constant-Workforce (Level Plan)" = "level",
          "Mixed Plan (Custom Workforce)" = "mixed"
        )),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'mixed'", ns("plan_type")),
          textInput(ns("mixed_wf"), "Workforce per Period (comma separated):", value = "10, 12, 12, 15")
        ),
        
        hr(),
        h4("Plant & Initial Data"),
        numericInput(ns("w0"), "Starting Workforce (W0):", value = 10, min = 0),
        numericInput(ns("i0"), "Starting Inventory (I0):", value = 0, min = 0),
        numericInput(ns("target_in"), "Target Ending Inventory (IN):", value = 0, min = 0),
        
        hr(),
        h4("Productivity (K)"),
        radioButtons(ns("k_mode"), "Productivity Input Mode:", choices = c(
          "Direct Entry (K)" = "direct",
          "Compute from Historical Data" = "compute"
        )),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'direct'", ns("k_mode")),
          numericInput(ns("k_direct"), "K (units/worker/day):", value = 2.5, min = 0.1, step = 0.1)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'compute'", ns("k_mode")),
          numericInput(ns("hist_prod"), "Past Units Produced:", value = 5000),
          numericInput(ns("hist_workers"), "Past Workers:", value = 10),
          numericInput(ns("hist_days"), "Past Working Days:", value = 200)
        ),
        
        hr(),
        h4("Unit Costs ($)"),
        numericInput(ns("c_hire"), "Hiring Cost ($/worker):", value = 500),
        numericInput(ns("c_fire"), "Firing Cost ($/worker):", value = 1000),
        numericInput(ns("c_hold"), "Holding Cost ($/unit/period):", value = 10),
        numericInput(ns("c_reg"), "Regular Labor Cost ($/worker/day):", value = 100)
      ),
      
      mainPanel(
        h4("Cumulative Production vs. Cumulative Net Demand Plot"),
        plotOutput(ns("cum_plot")),
        hr(),
        h4("Plan Comparison: Fractional vs. Whole Workers"),
        fluidRow(
          column(6, h5("Original (Exact/Fractional Plan)"), tableOutput(ns("table_orig"))),
          column(6, h5("Whole Workers Plan (Ceiling/Rounded Up)"), tableOutput(ns("table_whole")))
        ),
        hr(),
        h4("Cost Breakdown Comparison"),
        tableOutput(ns("cost_table"))
      )
    )
  )
}

agg_heuristic_server <- function(id) {
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
    
    K_val <- reactive({
      if (input$k_mode == "direct") {
        return(input$k_direct)
      } else {
        req(input$hist_workers > 0, input$hist_days > 0)
        return(input$hist_prod / (input$hist_workers * input$hist_days))
      }
    })
    
    costs <- reactive({
      list(hire = input$c_hire, fire = input$c_fire, hold = input$c_hold, regular_day = input$c_reg)
    })
    
    plan_results <- reactive({
      req(raw_data())
      df <- raw_data()
      req("Period" %in% names(df), "Demand" %in% names(df), "Days" %in% names(df))
      
      K <- K_val()
      c_list <- costs()
      
      if (input$plan_type == "chase") {
        orig <- run_chase_plan(df, input$w0, input$i0, K, round_up = FALSE)
        whole <- run_chase_plan(df, input$w0, input$i0, K, round_up = TRUE)
      } else if (input$plan_type == "level") {
        orig <- run_level_plan(df, input$w0, input$i0, input$target_in, K, round_up = FALSE)
        whole <- run_level_plan(df, input$w0, input$i0, input$target_in, K, round_up = TRUE)
      } else {
        wf_vec <- as.numeric(unlist(strsplit(input$mixed_wf, ",")))
        orig <- run_mixed_plan(df, input$w0, input$i0, K, wf_vec, round_up = FALSE)
        whole <- run_mixed_plan(df, input$w0, input$i0, K, wf_vec, round_up = TRUE)
      }
      
      cost_orig <- calc_agg_costs(orig, c_list)
      cost_whole <- calc_agg_costs(whole, c_list)
      
      cost_comp <- data.frame(
        Cost_Component = cost_orig$Cost_Component,
        Original_Plan_Cost = cost_orig$Amount,
        Whole_Workers_Plan_Cost = cost_whole$Amount
      )
      
      list(orig = orig, whole = whole, cost_comp = cost_comp)
    })
    
    output$cum_plot <- renderPlot({
      res <- plan_results()
      df <- res$orig
      
      cum_demand <- cumsum(df$Demand)
      cum_prod_orig <- cumsum(df$Production)
      cum_prod_whole <- cumsum(res$whole$Production)
      
      plot_df <- data.frame(
        Period = factor(df$Period, levels = df$Period),
        Cum_Demand = cum_demand,
        Cum_Prod_Orig = cum_prod_orig,
        Cum_Prod_Whole = cum_prod_whole
      )
      
      ggplot(plot_df, aes(x = Period, group = 1)) +
        geom_line(aes(y = Cum_Demand, color = "Cumulative Demand"), size = 1.2) +
        geom_line(aes(y = Cum_Prod_Orig, color = "Cum. Prod (Original)"), size = 1, linetype = "dashed") +
        geom_line(aes(y = Cum_Prod_Whole, color = "Cum. Prod (Whole Workers)"), size = 1, linetype = "dotted") +
        scale_color_manual(values = c("Cumulative Demand" = "black", "Cum. Prod (Original)" = "blue", "Cum. Prod (Whole Workers)" = "red")) +
        theme_minimal() +
        labs(y = "Units", color = "Legend", title = "Cumulative Demand vs. Production")
    })
    
    output$table_orig <- renderTable({
      res <- plan_results()
      df <- res$orig
      df[, c("Period", "Demand", "Days", "Workers", "Hired", "Fired", "Production", "Inventory")]
    }, digits = 2)
    
    output$table_whole <- renderTable({
      res <- plan_results()
      df <- res$whole
      df[, c("Period", "Demand", "Days", "Workers", "Hired", "Fired", "Production", "Inventory")]
    }, digits = 2)
    
    output$cost_table <- renderTable({
      plan_results()$cost_comp
    }, digits = 2)
    
  })
}