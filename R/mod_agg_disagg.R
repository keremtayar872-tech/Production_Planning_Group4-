# R/mod_agg_disagg.R
library(shiny)
library(readxl)
library(ggplot2)

agg_disagg_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload Product Family Data (.xlsx or .csv)", accept = c(".xlsx", ".csv")),
        h5("Expected Columns: Product_Family, Demand"),
        hr(),
        numericInput(ns("total_prod"), "Total Aggregate Production Volume:", value = 5000, min = 1),
        selectInput(ns("rule_type"), "Disaggregation Rule:", choices = c(
          "Square-Root Rule (w_i ∝ √D_i)" = "sqrt",
          "Proportional to Demand (w_i ∝ D_i)" = "prop"
        )),
        hr(),
        checkboxInput(ns("round_units"), "Round Allocated Units to Whole Numbers", value = TRUE)
      ),
      
      mainPanel(
        h4("Disaggregation Allocation Table"),
        tableOutput(ns("disagg_table")),
        hr(),
        h4("Product Family Production Share Plot"),
        plotOutput(ns("share_plot")),
        hr(),
        h4("Allocation Summary"),
        tableOutput(ns("summary_table"))
      )
    )
  )
}

agg_disagg_server <- function(id) {
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
    
    disagg_results <- reactive({
      req(raw_data())
      df <- raw_data()
      req("Product_Family" %in% names(df), "Demand" %in% names(df))
      
      total_p <- input$total_prod
      
      if (input$rule_type == "sqrt") {
        sqrt_d <- sqrt(df$Demand)
        weights <- sqrt_d / sum(sqrt_d)
      } else {
        weights <- df$Demand / sum(df$Demand)
      }
      
      allocated_prod <- total_p * weights
      if (input$round_units) {
        allocated_prod <- round(allocated_prod)
      }
      
      df_res <- data.frame(
        Product_Family = df$Product_Family,
        Demand = df$Demand,
        Sqrt_Demand = round(sqrt(df$Demand), 2),
        Weight_Pct = round(weights * 100, 2),
        Allocated_Production = round(allocated_prod, 2)
      )
      
      summary_df <- data.frame(
        Metric = c("Total Aggregate Production", "Total Product Demand", "Number of Product Families"),
        Value = c(total_p, sum(df$Demand), nrow(df))
      )
      
      list(df = df_res, summary = summary_df)
    })
    
    output$disagg_table <- renderTable({
      disagg_results()$df
    }, digits = 2)
    
    output$summary_table <- renderTable({
      disagg_results()$summary
    }, digits = 2)
    
    output$share_plot <- renderPlot({
      res <- disagg_results()
      df <- res$df
      
      ggplot(df, aes(x = Product_Family, y = Allocated_Production, fill = Product_Family)) +
        geom_bar(stat = "identity", width = 0.6) +
        geom_text(aes(label = Allocated_Production), vjust = -0.5, size = 4) +
        theme_minimal() +
        labs(x = "Product Family", y = "Allocated Production (Units)", title = "Disaggregated Production Volume by Product Family") +
        theme(legend.position = "none")
    })
    
  })
}