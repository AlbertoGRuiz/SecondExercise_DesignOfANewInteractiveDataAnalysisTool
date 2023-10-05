# if (!require("readr")) install.packages("readr")
# library(readr)
# # Read the first file as a reference for the columns
# data <- read_csv("Gasolineras_de_EspaB1a.csv")
# 
# # Modifica el objeto data
# data$Precio_gas <- apply(data[,grep("Precio_g_", names(data))], 1, function(x) {
#   if(all(is.na(x) | x == 0)) {
#     return(0)
#   } else {
#     return(x[!is.na(x) & x > 0 & x < 5][1])
#   }
# })

# Carga las bibliotecas necesarias
if (!require("readxl")) install.packages("readxl")
if (!require("readr")) install.packages("readr")
library(readxl)
library(readr)


# Lee el archivo XLS
datos <- read_excel("preciosEESS_es.xls", skip = 3)
datos$Longitud <- gsub(",", ".", datos$Longitud)
datos$Latitud <- gsub(",", ".", datos$Latitud)

# Reemplazar las comas por puntos en estas columnas
datos[,grep("Precio", names(datos), value = TRUE)] <- lapply(datos[,grep("Precio", names(datos), value = TRUE)], function(x){
  gsub(",", ".", x)
})






# Escribe el archivo CSV
write_csv(datos, "preciosEESS_es.csv")


data <- read_csv("preciosEESS_es.csv")

  






