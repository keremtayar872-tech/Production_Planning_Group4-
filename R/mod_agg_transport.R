# R/mod_agg_transport.R
library(shiny)
library(readxl)
library(ggplot2)
library(lpSolve)

agg_transport_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload Data (.xlsx or .csv)", accept = c(".xlsx", ".csv")),
        h5("Expected Columns: Period, Demand, Days"),
        hr(),
        
        selectInput(ns("solution_method"), "Select Solution Method", choices = c(
          "North-West Corner Solution" = "nwc",
          "Optimal Solution (Cheapest Cell / LP)" = "optimal"
        )),
        
        hr(),
        h4("Capacities per Period"),
        numericInput(ns("i0"), "Starting Inventory (I0):", value = 100, min = 0),
        numericInput(ns("cap_reg"), "Regular Capacity per Period (units):", value = 800, min = 0),
        numericInput(ns("cap_ot"), "Overtime Capacity per Period (units):", value = 200, min = 0),
        numericInput(ns("cap_sub"), "Subcontracting Capacity per Period (units):", value = 150, min = 0),
        
        hr(),
        h4("Unit Costs ($/unit)"),
        numericInput(ns("c_reg"), "Regular Unit Cost ($):", value = 50),
        numericInput(ns("c_ot"), "Overtime Unit Cost ($):", value = 75),
        numericInput(ns("c_sub"), "Subcontracting Unit Cost ($):", value = 90),
        numericInput(ns("c_hold"), "Holding Cost ($/unit/period):", value = 5)
      ),
      
      mainPanel(
        h4("Transportation Allocation Table (Units)"),
        tableOutput(ns("trans_table")),
        hr(),
        h4("Aggregate Production Plan Summary"),
        tableOutput(ns("plan_summary")),
        hr(),
        h4("Cumulative Demand vs Total Production Plot"),
        plotOutput(ns("cum_plot")),
        hr(),
        h4("Cost Breakdown"),
        tableOutput(ns("cost_table"))
      )
    )
  )
}

agg_transport_server <- function(id) {
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
    
    transport_model <- reactive({
      req(raw_data())
      df <- raw_data()
      req("Period" %in% names(df), "Demand" %in% names(df))
      
      n <- nrow(df)
      demands <- df$Demand
      
      # Build Supply vector and labels
      # Sources: I0, P1_RT, P1_OT, P1_Sub, P2_RT, ...
      supply_labels <- "Initial Inventory (I0)"
      supply_vec <- input$i0
      supply_base_cost <- 0
      
      for (t in 1:n) {
        p_name <- paste0("Period_", df$Period[t])
        supply_labels <- c(supply_labels, paste0(p_name, "_Regular"), paste0(p_name, "_Overtime"), paste0(p_name, "_Subcontract"))
        supply_vec <- c(supply_vec, input$cap_reg, input$cap_ot, input$cap_sub)
        supply_base_cost <- c(supply_base_cost, input$c_reg, input$c_ot, input$c_sub)
      }
      
      total_supply <- sum(supply_vec)
      total_demand <- sum(demands)
      
      demand_vec <- demands
      demand_labels <- paste0("Demand_P", df$Period)
      
      has_dummy <- FALSE
      if (total_supply > total_demand) {
        has_dummy <- TRUE
        demand_vec <- c(demand_vec, total_supply - total_demand)
        demand_labels <- c(demand_labels, "Unused Capacity (Dummy)")
      }
      
      m <- length(supply_vec)
      num_demands <- length(demand_vec)
      
      # Build Cost Matrix (m x num_demands)
      cost_mat <- matrix(0, nrow = m, ncol = num_demands)
      
      # Initial Inventory row
      for (k in 1:n) {
        cost_mat[1, k] <- (k - 1) * input$c_hold
      }
      if (has_dummy) cost_mat[1, num_demands] <- 0
      
      # Period sources rows
      for (t in 1:n) {
        base_t_idx <- 1 + (t - 1) * 3
        for (type in 1:3) { # 1: Reg, 2: OT, 3: Sub
          r_idx <- base_t_idx + type
          unit_cost <- supply_base_cost[r_idx]
          
          for (k in 1:n) {
            if (k < t) {
              cost_mat[r_idx, k] <- 1e7 # Backorders not allowed
            } else {
              cost_mat[r_idx, k] <- unit_cost + (k - t) * input$c_hold
            }
          }
          if (has_dummy) cost_mat[r_idx, num_demands] <- 0
        }
      }
      
      # Solve Allocation
      alloc <- matrix(0, nrow = m, ncol = num_demands)
      
      if (input$solution_method == "nwc") {
        # North-West Corner Algorithm
        s <- supply_vec; d <- demand_vec
        i <- 1; j <- 1
        while (i <= m && j <= num_demands) {
          x <- min(s[i], d[j])
          alloc[i, j] <- x
          s[i] <- s[i] - x
          d[j] <- d[j] - x
          if (s[i] == 0) i <- i + 1
          if (j <= num_demands && d[j] == 0) j <- j + 1
        }
      } else {
        # Optimal / Cheapest Cell LP Transport
        res <- lpSolve::lp.transport(
          cost.mat = cost_mat,
          direction = "min",
          row.signs = rep("<=", m),
          row.rhs = supply_vec,
          col.signs = rep(">=", num_demands),
          col.rhs = demand_vec
        )
        alloc <- res$solution
      }
      
      # Format Allocation Table
      alloc_df <- as.data.frame(alloc)
      colnames(alloc_df) <- demand_labels
      rownames(alloc_df) <- supply_labels
      alloc_df$Total_Supply_Used <- rowSums(alloc)
      alloc_df$Capacity <- supply_vec
      
      # Summarize Plan per Period
      plan_df <- data.frame(
        Period = df$Period,
        Demand = df$Demand,
        Regular_Prod = 0,
        Overtime_Prod = 0,
        Subcontract_Prod = 0,
        Inventory = 0,
        Period_Cost = 0
      )
      
      curr_inv <- input$i0
      for (t in 1:n) {
        base_t_idx <- 1 + (t - 1) * 3
        reg_used <- sum(alloc[base_t_idx + 1, 1:n])
        ot_used  <- sum(alloc[base_t_idx + 2, 1:n])
        sub_used <- sum(alloc[base_t_idx + 3, 1:n])
        
        tot_prod <- reg_used + ot_used + sub_used
        curr_inv <- curr_inv + tot_prod - df$Demand[t]
        
        p_cost <- (reg_used * input$c_reg) + (ot_used * input$c_ot) + (sub_used * input$c_sub) + (max(0, curr_inv) * input$c_hold)
        
        plan_df$Regular_Prod[t] <- reg_used
        plan_df$Overtime_Prod[t] <- ot_used
        plan_df$Subcontract_Prod[t] <- sub_used
        plan_df$Inventory[t] <- curr_inv
        plan_df$Period_Cost[t] <- p_cost
      }
      
      # Total Cost Calculation
      total_cost <- sum(alloc * cost_mat * (cost_mat < 1e6))
      
      cost_summary <- data.frame(
        Cost_Component = c("Regular Production Cost", "Overtime Cost", "Subcontracting Cost", "Holding Cost", "TOTAL TRANSPORTATION COST"),
        Amount = c(
          sum(plan_df$Regular_Prod) * input$c_reg,
          sum(plan_df$Overtime_Prod) * input$c_ot,
          sum(plan_df$Subcontract_Prod) * input$c_sub,
          sum(pmax(0, plan_df$Inventory)) * input$c_hold,
          round(total_cost, 2)
        )
      )
      
      list(alloc_df = alloc_df, plan_df = plan_df, cost_summary = cost_summary)
    })
    
    output$trans_table <- renderTable({
      transport_model()$alloc_df
    }, rownames = TRUE, digits = 0)
    
    output$plan_summary <- renderTable({
      transport_model()$plan_df
    }, digits = 2)
    
    output$cost_table <- renderTable({
      transport_model()$cost_summary
    }, digits = 2)
    
    output$cum_plot <- renderPlot({
      res <- transport_model()
      df <- res$plan_df
      
      cum_demand <- cumsum(df$Demand)
      cum_prod <- cumsum(df$Regular_Prod + df$Overtime_Prod + df$Subcontract_Prod)
      
      plot_df <- data.frame(
        Period = factor(df$Period, levels = df$Period),
        Cum_Demand = cum_demand,
        Cum_Prod = cum_prod
      )
      
      ggplot(plot_df, aes(x = Period, group = 1)) +
        geom_line(aes(y = Cum_Demand, color = "Cumulative Demand"), size = 1.2) +
        geom_line(aes(y = Cum_Prod, color = "Transportation Cum. Production"), size = 1, linetype = "dashed") +
        scale_color_manual(values = c("Cumulative Demand" = "black", "Transportation Cum. Production" = "blue")) +
        theme_minimal() +
        labs(y = "Units", color = "Legend", title = "Transportation Method: Cumulative Demand vs Production")
    })
    
  })
}