###############################################################################
################ INCIDENCIA DE COVID-19 EN LAS REGIONES DE MEXICO #############
###############################################################################

#Paso 1 CArga de librerías
library(tidyverse)
library(data.table)
library(raster)
library(stringi)
library(leaflet)

#Paso 3: Limpieza de la infomación de los mapas
MX <- getData("GADM", country = "MX", level = 1)
# MX@data$NAME_1 <- toupper(MX@data$NAME_1)
# MX@data$NAME_1 <- stri_trans_general(MX@data$NAME_1, "Latin-ASCII")
MX@data$NAME_1[which(MX@data$NAME_1%in% c("Distrito Federal"))]<- "Ciudad de México"



palnumeric <- colorQuantile(domain = mortalidad2020_$tasa_mortalidad,n = 5,
                            palette = c("#f7b267","#f79d65","#f4845f", "#f27059", "#f25c54","#f42b03","#e70e02"))


#Paso 5: Creando el mapa
leaflet() %>%
  addProviderTiles(provider = "CartoDB.PositronNoLabels") %>%
  addPolygons(data = MX , fillColor  = ~palnumeric(mortalidad2020_$tasa_mortalidad), color = "black", weight = 1,
              label = MX$NAME_1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5) %>%
  addLegend(data = MX, position = "topright", pal = palnumeric, values = ~mortalidad2020_$tasa_mortalidad,
            title = "Mortalidad por COVID-19")



palnumeric <- colorQuantile(domain = mortalidad2021_$tasa_mortalidad,n = 5,
                            palette = c("#f7b267","#f79d65","#f4845f", "#f27059", "#f25c54","#f42b03","#e70e02"))


#Paso 5: Creando el mapa
leaflet() %>%
  addProviderTiles(provider = "CartoDB.PositronNoLabels") %>%
  addPolygons(data = MX , fillColor  = ~palnumeric(mortalidad2021_$tasa_mortalidad), color = "black", weight = 1,
              label = MX$NAME_1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5) %>%
  addLegend(data = MX, position = "topright", pal = palnumeric, values = ~mortalidad2021_$tasa_mortalidad,
            title = "Mortalidad por COVID-19")
