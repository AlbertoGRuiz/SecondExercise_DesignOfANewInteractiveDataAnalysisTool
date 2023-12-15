# En la aplicación combinada
if (!require("shiny")) install.packages("shiny")
if (!require("shinydashboard")) install.packages("shinydashboard")
library(shiny)
library(shinydashboard)
# Incluye los archivos de las aplicaciones Shiny

if(!exists("gas_station_data_loaded")){
  source("GasStation_dataset_load.R")
}
source("app_choropleth_map.R")
source("app_grouped_bar.R")
source("app_line_chart.R")

# Define la interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(title = "Fuel prices in Spain"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Price per province", tabName = "app1", icon = icon("map")),
      menuItem("Price over the time", tabName = "app2", icon = icon("line-chart")),
      menuItem("Price per road type", tabName = "app3", icon = icon("bar-chart"))
    )
  ),
    dashboardBody(
        tags$style(type = "text/css", "#map {height: calc(100vh - 160px) !important;}"),
        tags$style(type = "text/css", "#barChart {height: calc(100vh - 160px) !important;}"),
        tags$style(type = "text/css", "#lineChart {height: calc(100vh - 160px) !important;}"),
        tabItems(
        tabItem(tabName = "app1",
                app1$ui
        ),
        tabItem(tabName = "app2",
                app2$ui
        ),
        tabItem(tabName = "app3",
                app3$ui
        )
        )
    )
)

# Define el servidor
server <- function(input, output) {
  # Llama a las funciones del servidor de cada aplicación
  app1$server(input, output)
  app2$server(input, output)
  app3$server(input, output)
}

# Ejecuta la aplicación
shinyApp(ui = ui, server = server)


rm("gas_station_data_loaded")