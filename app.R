# Instala y carga los paquetes necesarios
if (!require("shiny")) install.packages("shiny")
if (!require("readr")) install.packages("readr")
if (!require("leaflet")) install.packages("leaflet")
library(shiny)
library(readr)
library(leaflet)

# Carga el conjunto de datos
source("GasStation_dataset_load.R")

# Define la interfaz de usuario
ui <- fluidPage(
  titlePanel("Mapa de gasolineras en España"),
  selectInput("carburante", "Selecciona un tipo de carburante:", 
              choices = c("Precio gasolina 95 E5", "Precio gasolina 98 E5", "Precio gasóleo A", "Precio gasóleo Premium")),
  leafletOutput("map", height = "84vh") # Establece la altura al 100% de la altura de la ventana
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    # Crea un mapa centrado en España
    m <- leaflet() %>%
      setView(lng = -6.94922, lat = 36.463667, zoom = 5.3) %>%
      addProviderTiles(providers$OpenStreetMap)
    
    # Genera una paleta de colores basada en la elección del usuario
    pal <- colorNumeric(palette = "RdYlGn", domain = data[[input$carburante]][data[[input$carburante]] > 0], reverse = TRUE)
    
    # Añade marcadores para cada gasolinera
    m <- addCircleMarkers(
      map = m,
      data = data,
      lng = ~Longitud, lat = ~Latitud,
      color = pal(data[[input$carburante]]), # Usa la paleta de colores para los marcadores
      popup = ~paste0("<strong>Dirección:</strong> ", Dirección, "<br>",
                      "<strong>Horario:</strong> ", Horario, "<br>",
                      "<strong>",input$carburante,":</strong> ", data[[input$carburante]], "<br>",
                      "<strong>Fecha últ.Actualización;</strong> ", `Toma de datos`)
    )
    # Genera una paleta de colores basada en la elección del usuario
    data_no_na <- data[[input$carburante]][!is.na(data[[input$carburante]]) & data[[input$carburante]] > 0]
    pal_no_NA <- colorNumeric(palette = "RdYlGn", domain = data_no_na, reverse = TRUE)
    
    # Añade la leyenda al mapa
    m <- addLegend(map = m, pal = pal_no_NA, values = data_no_na, title = "Precios de carburante", opacity = 1)
    
    # Añade una segunda leyenda para los valores NA
    m <- addLegend(map = m, colors = "grey", labels = "NA", title = "Valores no disponibles", opacity = 1, position = "bottomright")
    
    return(m)
  })
}

# Ejecuta la aplicación
shinyApp(ui = ui, server = server)
