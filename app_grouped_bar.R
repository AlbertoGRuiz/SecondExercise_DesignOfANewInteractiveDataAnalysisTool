# Instala y carga los paquetes necesarios
if (!require("shiny")) install.packages("shiny")
if (!require("plotly")) install.packages("plotly")

library(shiny)
library(plotly)

# Carga el conjunto de datos
if(!exists("gas_station_data_loaded")){
  source("GasStation_dataset_load.R")
}

data$Location_Type <- ifelse(grepl("AUTOPISTA|AUTOVIA", data$Dirección), "Autovía", "Urbano")
data_autovia <- data %>% filter(Location_Type == "Autovía")
mean_95_E5_auto <- mean(data_autovia$`Precio gasolina 95 E5`, na.rm = TRUE)
mean_98_E5_auto <- mean(data_autovia$`Precio gasolina 98 E5`, na.rm = TRUE)
mean_gasoleo_A_auto <- mean(data_autovia$`Precio gasóleo A`, na.rm = TRUE)
mean_gasoleo_Premium_auto <- mean(data_autovia$`Precio gasóleo Premium`, na.rm = TRUE)
rm(data_autovia)

data_urbano <- data %>% filter(Location_Type == "Urbano")
mean_95_E5_urban <- mean(data_urbano$`Precio gasolina 95 E5`, na.rm = TRUE)
mean_98_E5_urban <- mean(data_urbano$`Precio gasolina 98 E5`, na.rm = TRUE)
mean_gasoleo_A_urban <- mean(data_urbano$`Precio gasóleo A`, na.rm = TRUE)
mean_gasoleo_Premium_urban <- mean(data_urbano$`Precio gasóleo Premium`, na.rm = TRUE)
rm(data_urbano)

max <- max(mean_95_E5_auto, mean_98_E5_auto, mean_gasoleo_A_auto, mean_gasoleo_Premium_auto, mean_95_E5_urban, mean_98_E5_urban, mean_gasoleo_A_urban, mean_gasoleo_Premium_urban) + 0.01
max <- round(max,2)

# Define la interfaz de usuario
ui <- fluidPage(
  titlePanel("Price per road type"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("yrange", "Price range:", min = 0, max = max, value = c(0,max))
    ),
    mainPanel(
      tags$head(
        tags$style(
          "#barChart {
            border-radius: 15px;
            height: 80vh;
          }"
        )
      ),
      plotlyOutput("barChart")
    )
  )
)

# Define el servidor
server <- function(input, output) {
  # Crea un data frame con las medias calculadas
  data_summary <- data.frame(
    Location_Type = rep(c("Highway", "Urban"), each = 4),
    Fuel_Type = rep(c("Gasoline 95 E5", "Gasoline 98 E5", "Diesel A", "Diesel A Premium"), 2),
    Mean_Price = c(mean_95_E5_auto, mean_98_E5_auto, mean_gasoleo_A_auto, mean_gasoleo_Premium_auto, 
                  mean_95_E5_urban, mean_98_E5_urban, mean_gasoleo_A_urban, mean_gasoleo_Premium_urban)
  )
  
  # Crea el gráfico de barras agrupado
  output$barChart <- renderPlotly({
    ggplot(data_summary, aes(x = Location_Type , y = Mean_Price, fill = Fuel_Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal(base_size  = 12) +
      labs(x = "Location Type", y = "Price per liter (€)", fill = "Fuel Type") +  # Cambia el título de la leyenda
      coord_cartesian(ylim = input$yrange) +
      theme(legend.title = element_text(face = "bold", family = "Open Sans Black"))  # Hace que el título de la leyenda sea negrita
  })
}

# Crea la aplicación Shiny
shinyApp(ui = ui, server = server)

app3 <- list(ui=ui,server=server)