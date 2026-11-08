# app.R
library(shiny)
library(bslib)
library(readxl)
library(ggplot2)
library(forecast)
library(lpSolve)

source("R/learnin_curve.R")       # Ödev 1
source("R/utils_forecasting.R")  # Ödev 2
source("R/mod_stationary.R")
source("R/mod_trend.R")
source("R/mod_seasonal.R")
source("R/mod_box_jenkins.R")

source("R/utils_aggregate.R")    # Ödev 3: Toplu Planlama
source("R/mod_agg_heuristic.R")  # Ödev 3: Heuristic Plans
source("R/mod_agg_lp.R")         # Ödev 3: LP Model
source("R/mod_agg_transport.R")  # Ödev 3: Transportation Method
source("R/mod_agg_disagg.R")     # Ödev 3: Disaggregation (Square-Root Rule)

ui <- navbarPage(
  title = "Production Planning & Control",
  theme = bs_theme(bootswatch = "flatly"),
  
  tabPanel("Learning Curve", learnin_curve_ui("lc_1")),
  
  navbarMenu("Forecasting",
             tabPanel("Stationary Series", stationary_ui("stat_1")),
             tabPanel("Trend Series (Holt)", trend_ui("trend_1")),
             tabPanel("Seasonal Series (Winters)", seasonal_ui("seas_1")),
             tabPanel("Box-Jenkins (ARIMA)", box_jenkins_ui("bj_1"))
  ),
  
  navbarMenu("Aggregate Planning",
             tabPanel("Heuristic Plans", agg_heuristic_ui("agg_heur_1")),
             tabPanel("Linear Programming (LP)", agg_lp_ui("agg_lp_1")),
             tabPanel("Transportation Method", agg_transport_ui("agg_trans_1")),
             tabPanel("Disaggregation (Square-Root Rule)", agg_disagg_ui("agg_disagg_1"))
  )
)

server <- function(input, output, session) {
  learnin_curve_server("lc_1")
  stationary_server("stat_1")
  trend_server("trend_1")
  seasonal_server("seas_1")
  box_jenkins_server("bj_1")
  
  agg_heuristic_server("agg_heur_1")
  agg_lp_server("agg_lp_1")
  agg_transport_server("agg_trans_1")
  agg_disagg_server("agg_disagg_1")
}

shinyApp(ui, server)