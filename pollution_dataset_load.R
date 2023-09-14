# Load necessary libraries
library(dplyr)
library(readr)
library(lubridate)

# Read the first file as a reference for the columns
reference_columns <- read_csv("pollution_dataset/csvs_per_year/csvs_per_year/madrid_2001.csv") %>% names()

# Initialize the combined DataFrame
data <- read_csv("pollution_dataset/csvs_per_year/csvs_per_year/madrid_2001.csv")

# Read and combine data from 2002 to 2018
for (year in 2002:2018) {
  # Create the path to the CSV file for the current year
  file_path <- paste0("pollution_dataset/csvs_per_year/csvs_per_year/madrid_", year, ".csv")
  
  # Read the CSV file into a dataframe
  year_data <- read_csv(file_path)
  
  # Get the additional columns in year_data
  extra_columns <- setdiff(names(year_data), reference_columns)
  
  # Discard the additional columns
  year_data <- year_data[, !names(year_data) %in% extra_columns]
  
  # Join the current year's data with the existing data
  data <- bind_rows(data, year_data)
}

# Convert the date and time column to date format and extract the month and year
data$date_time <- as.POSIXct(data$date, format = "%Y-%m-%d %H:%M:%S")
data$year <- year(data$date_time)
data$month <- month(data$date_time)

# Calculate the monthly average for each pollutant
monthly_average_data <- data %>%
  group_by(year, month) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

annual_average_data <- monthly_average_data %>%
  group_by(year) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Get the names of the columns that represent the pollutants
pollutant_names <- setdiff(names(annual_average_data), c("year", "month", "station"))

# Restructure only the pollutant columns
plotable_dataset <- gather(annual_average_data, pollutant, value, pollutant_names, -year)

# View the first records of the monthly average dataframe
rm(data,year_data,year,extra_columns,reference_columns,file_path,monthly_average_data,pollutant_names,annual_average_data)
head(plotable_dataset)
