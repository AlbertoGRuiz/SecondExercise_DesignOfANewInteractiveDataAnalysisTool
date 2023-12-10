


if (!require("shiny")) install.packages("shiny")
if (!require("readr")) install.packages("readr")
if (!require("leaflet")) install.packages("leaflet")
if (!require("mapSpain")) install.packages("mapSpain", dependencies = TRUE)
if (!require("stringdist")) install.packages("stringdist")
if (!require("dplyr")) install.packages("dplyr")
if (!require("plotly")) install.packages("plotly")

library(shiny)
library(readr)
library(leaflet)
library(mapSpain)
library(stringdist)
library(dplyr)
library(plotly)

ruta_del_archivo1 <- "C:/Users/diego/OneDrive/Escritorio/Master Data Science/DataVisualization/Proyecto/DataVis/CSV_Log/preciosEESS_es_19_Sept.csv"
datosSept <- read.csv(ruta_del_archivo1)

ruta_del_archivo2 <- "C:/Users/diego/OneDrive/Escritorio/Master Data Science/DataVisualization/Proyecto/DataVis/CSV_Log/preciosEESS_es_19_Oct.csv"
datosOct <- read.csv(ruta_del_archivo2)


print(datosSept)

# ... (Otras partes de tu código shiny)

ui <- fluidPage(
  # ... (Otras partes de tu interfaz de usuario)
  
  # Agrega un gráfico de puntos para mostrar los valores de septiembre y octubre
  plotlyOutput("grafico_puntos")
)

server <- function(input, output) {
  # ... (Otras partes de tu servidor Shiny)
  
  # Calcula las medias por tipo de combustible y mes para octubre
  medias_por_tipo_oct <- reactive({
    mean_95_E5_auto <- mean(datosOct$`Precio.gasolina.95.E5`, na.rm = TRUE)
    mean_98_E5_auto <- mean(datosOct$`Precio.gasolina.98.E5`, na.rm = TRUE)
    mean_gasoleo_A_auto <- mean(datosOct$`Precio.gasóleo.A`, na.rm = TRUE)
    mean_gasoleo_Premium_auto <- mean(datosOct$`Precio.gasóleo.Premium`, na.rm = TRUE)
    
    data.frame(
      Mes = factor("Octubre", levels = c("Septiembre", "Octubre")),
      Combustible = c("Gasolina 95 E5", "Gasolina 98 E5", "Gasóleo A", "Gasóleo Premium"),
      Media = c(mean_95_E5_auto, mean_98_E5_auto, mean_gasoleo_A_auto, mean_gasoleo_Premium_auto)
    )
  })
  
  
  
  # Calcula las medias por tipo de combustible y mes para septiembre
  medias_por_tipo_sept <- reactive({
    mean_95_E5_auto <- mean(datosSept$`Precio.gasolina.95.E5`, na.rm = TRUE)
    mean_98_E5_auto <- mean(datosSept$`Precio.gasolina.98.E5`, na.rm = TRUE)
    mean_gasoleo_A_auto <- mean(datosSept$`Precio.gasóleo.A`, na.rm = TRUE)
    mean_gasoleo_Premium_auto <- mean(datosSept$`Precio.gasóleo.Premium`, na.rm = TRUE)
    
    data.frame(
      Mes = factor("Septiembre", levels = c("Septiembre", "Octubre")),
      Combustible = c("Gasolina 95 E5", "Gasolina 98 E5", "Gasóleo A", "Gasóleo Premium"),
      Media = c(mean_95_E5_auto, mean_98_E5_auto, mean_gasoleo_A_auto, mean_gasoleo_Premium_auto)
    )
  })
  
  # Combina ambos data frames en uno solo
  medias_combinadas <- reactive({
    rbind(medias_por_tipo_sept(), medias_por_tipo_oct())
  })
  
  # Envía el gráfico de puntos al UI
  output$grafico_puntos <- renderPlotly({
    medias_combinadas_data <- medias_combinadas()
    
    plot_ly(data = medias_combinadas_data, x = ~Mes, y = ~Media, color = ~Combustible, type = 'scatter', mode = 'lines+markers', name = ~Combustible) %>%
      layout(title = "Valores Medios de Combustibles",
             xaxis = list(title = "Mes"),
             yaxis = list(title = "Precio por Litro (€)"),
             showlegend = TRUE)
  })
}
  

shinyApp(ui, server)

