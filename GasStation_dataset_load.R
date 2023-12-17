# Instala y carga los paquetes necesarios
if (!require("readr")) install.packages("readr")
if (!require("readxl")) install.packages("readxl")
if (!require("writexl")) install.packages("writexl")
library(readxl)
library(writexl)
library(readr)

# Obtiene una lista de todos los archivos en la carpeta "Excel_Log" que tienen el prefijo "preciosEESS_es.xls"
archivos <- list.files(path = "./Excel_Log", pattern = "^preciosEESS_es", full.names = TRUE)

# Itera sobre la lista de archivos
for (archivo in archivos) {
  # Lee el archivo
  datos <- read_excel(archivo, skip=3)
  
  # Aplica las transformaciones a los datos
  datos$Longitud <- gsub(",", ".", datos$Longitud)
  datos$Latitud <- gsub(",", ".", datos$Latitud)
  
  
  datos[,grep("Precio", names(datos), value = TRUE)] <- lapply(datos[,grep("Precio", names(datos), value = TRUE)], function(x){
    gsub(",", ".", x)
  })
  
  # Obtiene el nombre del archivo sin la extensión
  nombre_sin_extension <- sub("\\.xls$", "", basename(archivo))
  
  # Escribe el data frame como un archivo CSV con el mismo nombre
  write.csv(datos, file = paste0("./CSV_Log/", nombre_sin_extension, ".csv"), row.names = FALSE)
  rm(datos)
}
data <- read_csv("CSV_Log/preciosEESS_es.csv")

gas_station_data_loaded <- TRUE

  






