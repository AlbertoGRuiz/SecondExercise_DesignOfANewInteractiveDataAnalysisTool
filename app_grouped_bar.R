# Instala y carga los paquetes necesarios
if (!require("shiny")) install.packages("shiny")
if (!require("readr")) install.packages("readr")
if (!require("leaflet")) install.packages("leaflet")
if (!require("mapSpain")) install.packages("mapSpain", dependencies = TRUE)
if (!require("stringdist")) install.packages("stringdist")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")

library(shiny)
library(readr)
library(leaflet)
library(mapSpain)
library(stringdist)
library(dplyr)
library(ggplot2)

# Carga el conjunto de datos
source("GasStation_dataset_load.R")
data$LocationType <- ifelse(grepl("AUTOPISTA|AUTOVIA", data$Dirección), "Autovía", "Urbano")
data_autovia <- data %>% filter(LocationType == "Autovía")
mean_95_E5_auto <- mean(data_autovia$`Precio gasolina 95 E5`, na.rm = TRUE)
mean_98_E5_auto <- mean(data_autovia$`Precio gasolina 98 E5`, na.rm = TRUE)
mean_gasoleo_A_auto <- mean(data_autovia$`Precio gasóleo A`, na.rm = TRUE)
mean_gasoleo_Premium_auto <- mean(data_autovia$`Precio gasóleo Premium`, na.rm = TRUE)
rm(data_autovia)

data_urbano <- data %>% filter(LocationType == "Urbano")
mean_95_E5_urban <- mean(data_urbano$`Precio gasolina 95 E5`, na.rm = TRUE)
mean_98_E5_urban <- mean(data_urbano$`Precio gasolina 98 E5`, na.rm = TRUE)
mean_gasoleo_A_urban <- mean(data_urbano$`Precio gasóleo A`, na.rm = TRUE)
mean_gasoleo_Premium_urban <- mean(data_urbano$`Precio gasóleo Premium`, na.rm = TRUE)
rm(data_urbano)

max <- max(mean_95_E5_auto, mean_98_E5_auto, mean_gasoleo_A_auto, mean_gasoleo_Premium_auto, mean_95_E5_urban, mean_98_E5_urban, mean_gasoleo_A_urban, mean_gasoleo_Premium_urban) + 0.01
max <- round(max,2)

# Define la interfaz de usuario
ui <- fluidPage(
  titlePanel("Price Highway vs Urban"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("yrange", "Price range:", min = 0, max = max, value = c(0,max))
    ),
    mainPanel(
      plotOutput("barChart", height = "848")
    )
  )
)

# Define el servidor
server <- function(input, output) {
  # Crea un data frame con las medias calculadas
  data_summary <- data.frame(
    LocationType = rep(c("Highway", "Urban"), each = 4),
    FuelType = rep(c("95 E5", "98 E5", "Gasóleo A", "Gasóleo Premium"), 2),
    MeanPrice = c(mean_95_E5_auto, mean_98_E5_auto, mean_gasoleo_A_auto, mean_gasoleo_Premium_auto, 
                  mean_95_E5_urban, mean_98_E5_urban, mean_gasoleo_A_urban, mean_gasoleo_Premium_urban)
  )
  
  # Crea el gráfico de barras agrupado
  output$barChart <- renderPlot({
    ggplot(data_summary, aes(x = LocationType , y = MeanPrice, fill = FuelType)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(x = "Location Type", y = "Mean Price", fill = "Fuel Type") +
      coord_cartesian(ylim = input$yrange)
  })
}

# Crea la aplicación Shiny
shinyApp(ui = ui, server = server)