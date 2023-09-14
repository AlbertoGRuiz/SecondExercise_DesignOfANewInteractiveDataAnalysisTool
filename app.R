# Load necessary libraries
library(shiny)
library(ggplot2)

# Define user interface
ui <- fluidPage(
  titlePanel("Pollutant Evolution"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("pollutant", "Select pollutants:", choices = unique(plotable_dataset$pollutant))
    ),
    mainPanel(
      plotOutput("linePlot")
    )
  )
)

# Define server
server <- function(input, output) {
  output$linePlot <- renderPlot({
    # Filter data for selected pollutants and exclude rows with NaN values
    filtered_data <- plotable_dataset[plotable_dataset$pollutant %in% input$pollutant & !is.na(plotable_dataset$value),]
    
    # Calculate min and max year
    min_year <- min(filtered_data$year)
    max_year <- max(filtered_data$year)
    
    # Create line plot
    ggplot(filtered_data, aes(x = year, y = value, color = pollutant)) +
      geom_line() +
      labs(x = "Year", y = "Value", title = paste("Evolution of pollutants from", min_year, "to", max_year))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
