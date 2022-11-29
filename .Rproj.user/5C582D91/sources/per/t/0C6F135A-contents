
##### MORTALIDAD DE COVID-19 EN LAS REGIONES DE MEXICO #####

#Paso 1 Carga de librerías
library(tidyverse)
library(data.table)
library(raster)
library(stringi)
library(leaflet)

mortalidad2020 <- mortalidad_aj %>% 
  filter(AÑO == 2020)

mortalidad2021 <- mortalidad_aj %>% 
  filter(AÑO == 2021)

#Paso 3: Limpieza de la infomación de los mapas
MX <- getData("GADM", country = "MX", level = 1)
MX@data$NAME_1 <- toupper(MX@data$NAME_1)
# MX@data$NAME_1 <- stri_trans_general(MX@data$NAME_1, "Latin-ASCII")
MX@data$NAME_1[which(MX@data$NAME_1%in% c("Distrito Federal"))]<- "Ciudad de México"

### CAmbiar incidencia_Entidad por el de mortalidad
MX@data <- left_join(MX@data, mortalidad2020, by = c("NAME_1"= "NOM_ENT"))


#Valores numéricos
color_num <- colorNumeric(palette = "RdBu",
                          domain = mortalidad2020$TASA_AJUSTADA, reverse = T)


leaflet() %>%
  addProviderTiles(provider = "CartoDB.PositronNoLabels") %>%
  addPolygons(data = MX ,
              fillColor  = ~color_num(mortalidad2020$TASA_AJUSTADA),
              color = "black",
              weight = 1,
              label = paste0(MX$NAME_1, ": ", round(mortalidad2020$TASA_AJUSTADA,0) ),
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5) %>%
  addLegend(data = MX, position = "topright", pal = color_num, values = ~mortalidad2020$TASA_AJUSTADA,
            bins = seq(0,400, by =50), #Aqui modificar el valor mínimo al maximo por x
            title = "Mortalidad por COVID-19 durante 2020")

