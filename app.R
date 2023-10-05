# Instala y carga los paquetes necesarios
if (!require("shiny")) install.packages("shiny")
if (!require("readr")) install.packages("readr")
if (!require("leaflet")) install.packages("leaflet")
if (!require("mapSpain")) install.packages("mapSpain", dependencies = TRUE)
if (!require("stringdist")) install.packages("stringdist")
if (!require("dplyr")) install.packages("dplyr")
library(shiny)
library(readr)
library(leaflet)
library(mapSpain)
library(stringdist)
library(dplyr)

# Carga el conjunto de datos
source("GasStation_dataset_load.R")

# Define la interfaz de usuario
ui <- fluidPage(
  titlePanel("Mapa de precios medios de gasolineras en España por provincia"),
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
    
    # Calcula la media del precio del carburante por provincia
    data_by_province  <- aggregate(data[[input$carburante]], by = list(ine.prov.name = data$Provincia), FUN = mean, na.rm = TRUE)
    data_by_province <- rename(data_by_province, medium_cost = x )
    # Homogeneiza los nombres de las provincias
    data_by_province$ine.prov.name <- tools::toTitleCase(tolower(data_by_province$ine.prov.name))
    # Elimina los paréntesis
    data_by_province$ine.prov.name <- gsub(" \\(", ", ", data_by_province$ine.prov.name)
    data_by_province$ine.prov.name <- gsub("\\)", "", data_by_province$ine.prov.name)

    # Combina los límites de las provincias con los datos por provincia
    provincias <- mapSpain::esp_get_prov()
    
    # Para cada provincia en data_by_province, encuentra las provincias más similares en provincias
    for (i in seq_along(data_by_province$ine.prov.name)) {
      # Encuentra las provincias más similares con una distancia máxima de 2
      indices_similares <- agrep(data_by_province$ine.prov.name[i], provincias$ine.prov.name, max.distance = 0.1)
      
      
      # Si hay coincidencias, reemplaza el nombre de la provincia con el de la primera coincidencia
      if (length(indices_similares) > 0) {
        data_by_province$ine.prov.name[i] <- provincias$ine.prov.name[indices_similares[1]]
      }
    }
    
    provincias <- dplyr::left_join(provincias, data_by_province, by = "ine.prov.name")
    
    # Genera una paleta de colores basada en las medias calculadas
    pal <- colorNumeric(palette = "RdYlGn", domain = provincias$x, reverse = TRUE)
    
    # Añade polígonos para cada provincia
    m <- addPolygons(
      map = m,
      data = provincias,
      fillColor = ~pal(medium_cost), # Usa la paleta de colores para los polígonos
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = ~paste0("<strong>Provincia:</strong> ", ine.prov.name, "<br>",
                      "<strong>Precio medio ",input$carburante,":</strong> ", round(medium_cost, 3))
    )
    
    # Genera una paleta de colores basada en las medias calculadas
    pal_no_NA <- colorNumeric(palette = "RdYlGn", domain = provincias$medium_cost[!is.na(provincias$medium_cost)], reverse = TRUE)
    
    # Añade la leyenda al mapa
    m <- addLegend(map = m, pal = pal_no_NA, values = provincias$medium_cost[!is.na(provincias$medium_cost)], title = "Precios medios de carburante", opacity = 1)
    
    return(m)
  })
}

# Ejecuta la aplicación
shinyApp(ui = ui, server = server)
