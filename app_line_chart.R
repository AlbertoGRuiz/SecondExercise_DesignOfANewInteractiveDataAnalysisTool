


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


if(!exists("gas_station_data_loaded")){
  source("GasStation_dataset_load.R")
}

ruta_del_archivo1 <- "./CSV_Log/preciosEESS_es_19_Sept.csv"
datosSept <- read.csv(ruta_del_archivo1)

ruta_del_archivo2 <- "./CSV_Log/preciosEESS_es_19_Oct.csv"
datosOct <- read.csv(ruta_del_archivo2)

ruta_del_archivo3 <- "./CSV_Log/preciosEESS_es_19_Nov.csv"
datosNov <- read.csv(ruta_del_archivo3)

ruta_del_archivo4 <- "./CSV_Log/preciosEESS_es_19_Dic.csv"
datosDic <- read.csv(ruta_del_archivo4)



# Define la interfaz de usuario
ui <- fluidPage(
  titlePanel("Price over the time"),
  sidebarLayout(
    sidebarPanel(
      selectInput("start_month", "Start month:", choices = c("September", "October", "November", "December"), selected = "September"),
      uiOutput("end_month_ui"),  # Aquí se renderizará el selector de final
    ),
    mainPanel(
      tags$head(
        tags$style(
          "#lineChart {
            border-radius: 15px;
            height: 80vh;
          }"
        )
      ),
      plotlyOutput("lineChart")
    )
  )
)

server <- function(input, output) {
  output$end_month_ui <- renderUI({
    req(input$start_month)  # Asegúrate de que start_month está disponible
    months <- c("September", "October", "November", "December")
    start_index <- match(input$start_month, months)
    end_index <- if (!is.null(input$end_month)) match(input$end_month, months) else length(months)
    choices <- months[start_index:length(months)]
    if (start_index > end_index) {
      selectInput("end_month", "End month:", choices = choices, selected = "December")
    } else {
      selectInput("end_month", "End month:", choices = choices, selected = ifelse(is.null(input$end_month), "December", input$end_month))
    }
  })
  
  # Calcula las medias por tipo de combustible y mes para octubre
  medias_por_tipo_oct <- reactive({
    mean_95_E5_auto <- mean(datosOct$`Precio.gasolina.95.E5`, na.rm = TRUE)
    mean_98_E5_auto <- mean(datosOct$`Precio.gasolina.98.E5`, na.rm = TRUE)
    mean_gasoleo_A_auto <- mean(datosOct$`Precio.gasóleo.A`, na.rm = TRUE)
    mean_gasoleo_Premium_auto <- mean(datosOct$`Precio.gasóleo.Premium`, na.rm = TRUE)
    
    data.frame(
      Mes = factor("October", levels = c("September", "October", "November", "December")),
      Combustible = c("Gasoline 95 E5", "Gasoline 98 E5", "Diesel A", "Diesel A Premium"),
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
      Mes = factor("September", levels = c("September", "October", "November", "December")),
      Combustible = c("Gasoline 95 E5", "Gasoline 98 E5", "Diesel A", "Diesel A Premium"),
      Media = c(mean_95_E5_auto, mean_98_E5_auto, mean_gasoleo_A_auto, mean_gasoleo_Premium_auto)
    )
  })
  
  # Calcula las medias por tipo de combustible y mes para noviembre
  medias_por_tipo_nov <- reactive({
    mean_95_E5_auto <- mean(datosNov$`Precio.gasolina.95.E5`, na.rm = TRUE)
    mean_98_E5_auto <- mean(datosNov$`Precio.gasolina.98.E5`, na.rm = TRUE)
    mean_gasoleo_A_auto <- mean(datosNov$`Precio.gasóleo.A`, na.rm = TRUE)
    mean_gasoleo_Premium_auto <- mean(datosNov$`Precio.gasóleo.Premium`, na.rm = TRUE)
    
    data.frame(
      Mes = factor("November", levels = c("September", "October", "November", "December")),
      Combustible = c("Gasoline 95 E5", "Gasoline 98 E5", "Diesel A", "Diesel A Premium"),
      Media = c(mean_95_E5_auto, mean_98_E5_auto, mean_gasoleo_A_auto, mean_gasoleo_Premium_auto)
    )
  })
  
  # Calcula las medias por tipo de combustible y mes para diciembre
  medias_por_tipo_dic <- reactive({
    mean_95_E5_auto <- mean(datosDic$`Precio.gasolina.95.E5`, na.rm = TRUE)
    mean_98_E5_auto <- mean(datosDic$`Precio.gasolina.98.E5`, na.rm = TRUE)
    mean_gasoleo_A_auto <- mean(datosDic$`Precio.gasóleo.A`, na.rm = TRUE)
    mean_gasoleo_Premium_auto <- mean(datosDic$`Precio.gasóleo.Premium`, na.rm = TRUE)
    
    data.frame(
      Mes = factor("December", levels = c("September", "October", "November", "December")),
      Combustible = c("Gasoline 95 E5", "Gasoline 98 E5", "Diesel A", "Diesel A Premium"),
      Media = c(mean_95_E5_auto, mean_98_E5_auto, mean_gasoleo_A_auto, mean_gasoleo_Premium_auto)
    )
  })
  
  # Combina todos los data frames en uno solo
  medias_combinadas <- reactive({
    req(input$start_month, input$end_month)  # Asegúrate de que start_month y end_month están disponibles
    datos <- list("September" = medias_por_tipo_sept(), "October" = medias_por_tipo_oct(), "November" = medias_por_tipo_nov(), "December" = medias_por_tipo_dic())
    meses <- match(c(input$start_month, input$end_month), c("September", "October", "November", "December"))
    if (any(is.na(meses))) {
      return(NULL)  # Devuelve NULL si start_month o end_month no son válidos
    }
    do.call(rbind, datos[meses[1]:meses[2]])
  })
  
  # Envía el gráfico de puntos al UI
  output$lineChart <- renderPlotly({
    medias_combinadas_data <- medias_combinadas()
    
    plot_ly(data = medias_combinadas_data, x = ~Mes, y = ~Media, color = ~Combustible, type = 'scatter', mode = 'lines+markers', name = ~Combustible) %>%
      layout(
             xaxis = list(title = "Moth"),
             yaxis = list(title = "Price per liter (€)"),
             font = list(size = 13),
             showlegend = TRUE,
             legend = list(title = list(text = '<b> Fuel Type </b>')))  # Añade un título a la leyenda
  })
}
  

shinyApp(ui, server)

app2 <- list(ui=ui,server = server)
