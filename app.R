# app.R
library(shiny)
library(bslib)
library(readxl)
library(ggplot2)
library(forecast)

source("R/learnin_curve.R")       # Ödev 1: Learning Curve
source("R/utils_forecasting.R")  # Ödev 2: Yardımcı Fonksiyonlar
source("R/mod_stationary.R")     # Ödev 2: Stationary Series
source("R/mod_trend.R")          # Ödev 2: Trend Series (Holt)
source("R/mod_seasonal.R")       # Ödev 2: Seasonal Series (Winters)
source("R/mod_box_jenkins.R")    # Ödev 2: Box-Jenkins (ARIMA)

ui <- navbarPage(
  title = "Production Planning & Control",
  theme = bs_theme(bootswatch = "flatly"),
  
  tabPanel("Learning Curve", learnin_curve_ui("lc_1")),
  
  navbarMenu("Forecasting",
             tabPanel("Stationary Series", stationary_ui("stat_1")),
             tabPanel("Trend Series (Holt)", trend_ui("trend_1")),
             tabPanel("Seasonal Series (Winters)", seasonal_ui("seas_1")),
             tabPanel("Box-Jenkins (ARIMA)", box_jenkins_ui("bj_1"))
  )
)

server <- function(input, output, session) {
  learnin_curve_server("lc_1")
  stationary_server("stat_1")
  trend_server("trend_1")
  seasonal_server("seas_1")
  box_jenkins_server("bj_1")
}

shinyApp(ui, server)