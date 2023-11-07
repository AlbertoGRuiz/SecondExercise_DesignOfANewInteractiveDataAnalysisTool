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
  sidebarLayout(
    sidebarPanel(
      selectInput("carburante", "Selecciona un tipo de carburante:", 
                  choices = c("Precio gasolina 95 E5", "Precio gasolina 98 E5", "Precio gasóleo A", "Precio gasóleo Premium"))
    ),
    mainPanel(
      leafletOutput("map", height = "93vh") # Establece la altura al 100% de la altura de la ventana
    )
  )
)


server <- function(input, output) {
  output$map <- renderLeaflet({
    # Crea un mapa centrado en España
    m <- leaflet() %>%
      setView(lng = -3.70256, lat = 40.4165, zoom = 6) %>%
      addProviderTiles(providers$OpenStreetMap)
    
    # Calcula la media del precio del carburante por provincia
    data_by_province  <- aggregate(data[[input$carburante]], by = list(ine.prov.name = data$Provincia), FUN = mean, na.rm = TRUE)
    data_by_province <- rename(data_by_province, medium_cost = x )
    
    # Homogeneiza los nombres de las provincias
    data_by_province$ine.prov.name <- tools::toTitleCase(tolower(data_by_province$ine.prov.name))
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
    
    #Unifica los valores de las coordenadas del mapa y el precio medio por provincia usando la columna nombre de provincia
    provincias <- dplyr::left_join(provincias, data_by_province, by = "ine.prov.name")
    
    # Crea una nueva variable categórica
    mean_value <- mean(provincias$medium_cost, na.rm = TRUE)
    min_value <- min(provincias$medium_cost, na.rm = TRUE)
    print(mean_value) #low               #Mean-low         #Mean-high         #high            #ext-high
    breaks <- c(-Inf, mean_value-0.050*2,mean_value-0.015, mean_value, mean_value+0.015, mean_value+0.050*2, Inf)
    
    labels <- c("Bajo-➤Extremadamente",
                "Bajo-➤Intermedio", 
                "Bajo-➤Medio",
                "Alto-➤Medio",
                "Alto-➤Intermedio",
                "Alto-➤Extremadamente")
    # Asigna las etiquetas a la variable cost_category
    provincias$cost_category <- cut(provincias$medium_cost, breaks = breaks, labels = labels, include.lowest = TRUE, include.highest = TRUE)
    provincias$legend <- cut(provincias$medium_cost, breaks = breaks, labels = rev(labels), include.lowest = TRUE, include.highest = TRUE)
    
    
    # Define la paleta de colores
    pal <- colorFactor(palette = c("#1a9850","#91cf60","#d9ef8b", "#fee08b", "#fc8d59", "#d73027"), domain = provincias$cost_category)

    # Añade polígonos para cada provincia
    m <- addPolygons(
      map = m,
      data = provincias,
      fillColor = ~pal(cost_category), # Usa la paleta de colores para los polígonos
      weight = 2,
      opacity = 1,
      color = "black",
      dashArray = "3",
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = ~paste0("Provincia: ", ine.prov.name, " | ",
                      "Precio medio ",sub("Precio","",input$carburante),": ", round(medium_cost, 3))
    )

    # Añade la leyenda al mapa con etiquetas personalizadas
    m <- addLegend(map = m, pal = pal, values = (provincias[!is.na(provincias$legend),]$legend), 
                  title = paste0("Precio medio de ",sub("Precio","",input$carburante)), opacity = 1)
    return(m)
  })
}

# Ejecuta la aplicación
shinyApp(ui = ui, server = server)
