# Cargamos librerias
library(tidyverse)
library(data.table)
library(aweek)
library(raster)
library(stringi)
library(leaflet)

# Cargar base de tasa de mortalidad por municipio por COVID-19 para 2020 y 2021
mort_mun <- fread("Bases/mort_mun.csv", encoding = "Latin-1")

#### MAPA DE MORTALIDAD POR MUNICIPIO DE LA ZMVM DURANTE 2020 ####

# Cargamos base de mortalidad por municipio filtrada para 2020 
mort_mun <- fread("Bases/mort_mun.csv",
                  encoding = "Latin-1") %>% 
  filter(AÑO ==  2020)


# Limpieza de la infomación de los mapas
MX <- getData("GADM", country = "MX", level = 2)
MX@data$NAME_1 <- toupper(MX@data$NAME_1)
MX@data$NAME_2 <- toupper(MX@data$NAME_2)
MX@data$NAME_1 <- stri_trans_general(MX@data$NAME_1, "Latin-ASCII")

# Renombramos

MX <- MX[MX@data$NAME_1 %in% c("DISTRITO FEDERAL", "MEXICO"), ]

MX@data$NAME_1[which(MX@data$NAME_1%in% c("DISTRITO FEDERAL"))]<- "CIUDAD DE MEXICO"
MX@data$NAME_2[which(MX@data$NAME_2%in% c("MAGDALENA CONTRERAS"))]<- "LA MAGDALENA CONTRERAS"
MX@data$NAME_2[which(MX@data$NAME_2%in% c("JALATLACO"))]<- "XALATLACO"
MX@data$NAME_2[which(MX@data$NAME_2%in% c("ACAMBAY"))]<- "ACAMBAY DE RUIZ CASTANEDA"
MX@data$NAME_2[which(MX@data$NAME_2%in% c("ZINACATEPEC"))]<- "ZINACANTEPEC"
MX@data$NAME_2[which(MX@data$NAME_2%in% c("SAN MARTÍN DE LAS PIRÁÁMIDES"))]<- "SAN MARTIN DE LAS PIRAMIDES"
MX@data$NAME_2[which(MX@data$NAME_2%in% c("TLALNEPANTLA"))]<- "TLALNEPANTLA DE BAZ"


MX@data$NAME_2 <- stri_trans_general(MX@data$NAME_2, "Latin-ASCII")
MX <- MX[MX@data$NAME_2 %in% mort_mun$MUNICIPIO_RES, ]

MX@data <- left_join(MX@data, mort_mun, by = c("NAME_1"= "ENTIDAD_RES",
                                                         "NAME_2"= "MUNICIPIO_RES"))
##########

#Valores numéricos
color_num <- colorNumeric(palette = "RdBu",
                          domain = mort_mun$TASA_AJUSTADA, reverse = T)
leaflet() %>%
  addProviderTiles(provider = "CartoDB.PositronNoLabels") %>%
  addPolygons(data = MX ,
              fillColor  = ~color_num(TASA_AJUSTADA),
              color = "black",
              weight = 1,
              label = paste0(MX$NAME_2, ": ", round(MX$TASA_AJUSTADA,0) ),
              smoothFactor = 0.5,
              opacity = -5,
              stroke=TRUE,
              fillOpacity = 0.7) %>%
  addLegend(data = MX, position = "topright", pal = color_num, values = ~TASA_AJUSTADA,
            bins = seq(0,300, by =50),
            title = "Tasa Ajustada Mortalidad por COVID-19")

#POr Bins
color_bins <- colorBin(palette = c("RdBu"),
                       domain = mort_mun$TASA_AJUSTADA,bins = 5, reverse = T)

leaflet() %>%
  addProviderTiles(provider = "CartoDB.PositronNoLabels") %>%
  addPolygons(data = MX ,
              fillColor = ~color_bins(TASA_AJUSTADA) ,
              color = "black",
              weight = 1,
              label = paste0(MX$NAME_2, ": ", round(MX$TASA_AJUSTADA,0) ),

              opacity = -9,

              stroke=TRUE,
              fillOpacity = 0.9,
              smoothFactor = 0.3

              ) %>%

  addLegend(data = MX, position = "topright", pal  = color_bins,
            values = ~TASA_AJUSTADA,
            title = "Mortalidad por COVID-19")


##########

# Generamos las zonas

Noroeste <- mort_mun %>%
  filter(ZONA == "NOROESTE")

Suroeste <- mort_mun %>%
  filter(ZONA == "SUROESTE")

Centro <- mort_mun %>%
  filter(ZONA == "CENTRO")

Noreste <- mort_mun %>%
  filter(ZONA == "NORESTE")

Sureste <- mort_mun %>%
  filter(ZONA == "SURESTE")

MX_NOROESTE <- MX[MX@data$NAME_2 %in% Noroeste$MUNICIPIO_RES, ]
MX_SUROESTE <- MX[MX@data$NAME_2 %in% Suroeste$MUNICIPIO_RES,]
MX_CENTRO <- MX[MX@data$NAME_2 %in% Centro$MUNICIPIO_RES,]
MX_NORESTE <- MX[MX@data$NAME_2 %in% Noreste$MUNICIPIO_RES,]
MX_SURESTE <- MX[MX@data$NAME_2 %in% Sureste$MUNICIPIO_RES,]


#Valores numéricos
color_num_Noroeste <- colorNumeric(palette = "Reds",
                          domain = (mort_mun %>% filter(ZONA == "NOROESTE"))$TASA_AJUSTADA, reverse = F)
color_num_Suroeste <- colorNumeric(palette = "Blues",
                          domain = (mort_mun %>% filter(ZONA == "SUROESTE"))$TASA_AJUSTADA, reverse = F)
color_num_Centro <- colorNumeric(palette = "Greens",
                                   domain = (mort_mun %>% filter(ZONA == "CENTRO"))$TASA_AJUSTADA, reverse = F)

color_num_Noreste <- colorNumeric(palette = "Purples",
                                 domain = (mort_mun %>% filter(ZONA == "NORESTE"))$TASA_AJUSTADA, reverse = F)

color_num_Sureste <- colorNumeric(palette = "Oranges",
                                  domain = (mort_mun %>% filter(ZONA == "SURESTE"))$TASA_AJUSTADA, reverse = F)

#Bins
color_num_Noroeste <- colorBin(palette = c("Reds"),
                       domain = (mort_mun %>% filter(ZONA == "NOROESTE"))$TASA_AJUSTADA,bins = 5, reverse = F)

color_num_Suroeste <- colorBin(palette = "Blues",
                                   domain = (mort_mun %>% filter(ZONA == "SUROESTE"))$TASA_AJUSTADA,bins = 5, reverse = F) 

color_num_Centro <- colorBin(palette = "Greens",
                                 domain = (mort_mun %>% filter(ZONA == "CENTRO"))$TASA_AJUSTADA, bins = 5, reverse = F)

color_num_Noreste <- colorBin(palette = "Purples",
                                  domain = (mort_mun %>% filter(ZONA == "NORESTE"))$TASA_AJUSTADA, bins = 5, reverse = F)

color_num_Sureste <- colorBin(palette = "Oranges",
                                  domain = (mort_mun %>% filter(ZONA == "SURESTE"))$TASA_AJUSTADA,bins = 5, reverse = F)

grosor <- 0.5

# Generamos el mapa

leaflet() %>%
  addProviderTiles(provider = "CartoDB.PositronNoLabels") %>%
  addPolygons(data = MX_NOROESTE ,
              fillColor  = ~color_num_Noroeste(TASA_AJUSTADA),
              color = "black",
              weight = 1,
              label = paste0(MX_NOROESTE$NAME_2, ": ", round(MX_NOROESTE$TASA_AJUSTADA,0) ),
              smoothFactor = 0.5,
              opacity = 1,
              stroke=TRUE,
              fillOpacity = 1
              ) %>%

  addPolygons(data = MX_NORESTE ,
              fillColor  = ~color_num_Noreste(TASA_AJUSTADA),
              color = "black",
              weight = 1,
              label = paste0(MX_NORESTE$NAME_2, ": ", round(MX_NORESTE$TASA_AJUSTADA,0) ),
              smoothFactor = 0.5,
              opacity = 1,
              stroke=TRUE,
              fillOpacity = 1
  ) %>%

  addPolygons(data = MX_SUROESTE ,
              fillColor  = ~color_num_Suroeste(TASA_AJUSTADA),
              color = "black",
              weight = 1,
              label = paste0(MX_SUROESTE$NAME_2, ": ", round(MX_SUROESTE$TASA_AJUSTADA,0) ),
              smoothFactor = 0.5,
              opacity =1,
              stroke=TRUE,
              fillOpacity = 1
  ) %>%
    addPolygons(data = MX_SURESTE ,
                fillColor  = ~color_num_Sureste(TASA_AJUSTADA),
                color = "black",
                weight = 1,
                label = paste0(MX_SURESTE$NAME_2, ": ", round(MX_SURESTE$TASA_AJUSTADA,0) ),
                smoothFactor = 0.5,
                opacity =1,
                stroke=TRUE,
                fillOpacity = 1
    ) %>%
  addPolygons(data = MX_CENTRO ,
              fillColor  = ~color_num_Centro(TASA_AJUSTADA),
              color = "black",
              weight = 1,
              label = paste0(MX_CENTRO$NAME_2, ": ", round(MX_CENTRO$TASA_AJUSTADA,0) ),
              smoothFactor = 0.5,
              opacity =1,
              stroke=TRUE,
              fillOpacity = 1
  ) %>%
  addLegend(data = MX_CENTRO, position = "topright", pal  = color_num_Centro,
            values = ~TASA_AJUSTADA,
            title = "Mortalidad por COVID-19 en la zona centro por millón de hab.") %>%

  addLegend(data = MX_SUROESTE, position = "bottomleft", pal  = color_num_Suroeste,
            values = ~TASA_AJUSTADA,
            title = "Mortalidad por COVID-19 en la zona suroeste por millón de hab.") %>%
  addLegend(data = MX_NOROESTE, position = "topleft", pal  = color_num_Noroeste,
            values = ~TASA_AJUSTADA,
            title = "Mortalidad por COVID-19 en la zona noroeste por millón de hab.") %>%
  addLegend(data = MX_NORESTE, position = "topright", pal  = color_num_Noreste,
            values = ~TASA_AJUSTADA,
            title = "Mortalidad por COVID-19 en la zona noreste por millón de hab.") %>%
  addLegend(data = MX_SURESTE, position = "bottomright", pal  = color_num_Sureste,
            values = ~TASA_AJUSTADA,
            title = "Mortalidad por COVID-19 en la zona sureste por millón de hab.") %>%
## Este es para agregar el radio de 5 km de cada estacion
  addCircleMarkers(data = posicion, lat = ~latitud, lng = ~longitud) %>% 
  addCircles(lng = (unique(pm$longitud)), lat = (unique(pm$latitud)),
             radius = 5000, color = "red", fillOpacity = 0.05)
  
## Localizacion de las estaciones de monitoreo  
posicion <- pm [,.(latitud, longitud, cve_estac)] %>% unique()
  

#### MAPA DE MORTALIDAD POR MUNICIPIO DE LA ZMVM DURANTE 2021 ####

# Cargamos base de mortalidad por municipio filtrada para 2021 
mort_mun_2021 <- fread("Bases/mort_mun.csv",
                       encoding = "Latin-1") %>% 
  filter(AÑO ==  2021)


# Limpieza de la infomación de los mapas
MX <- getData("GADM", country = "MX", level = 2)
MX@data$NAME_1 <- toupper(MX@data$NAME_1)
MX@data$NAME_2 <- toupper(MX@data$NAME_2)
MX@data$NAME_1 <- stri_trans_general(MX@data$NAME_1, "Latin-ASCII")

# Renombramos

MX <- MX[MX@data$NAME_1 %in% c("DISTRITO FEDERAL", "MEXICO"), ]

MX@data$NAME_1[which(MX@data$NAME_1%in% c("DISTRITO FEDERAL"))]<- "CIUDAD DE MEXICO"
MX@data$NAME_2[which(MX@data$NAME_2%in% c("MAGDALENA CONTRERAS"))]<- "LA MAGDALENA CONTRERAS"
MX@data$NAME_2[which(MX@data$NAME_2%in% c("JALATLACO"))]<- "XALATLACO"
MX@data$NAME_2[which(MX@data$NAME_2%in% c("ACAMBAY"))]<- "ACAMBAY DE RUIZ CASTANEDA"
MX@data$NAME_2[which(MX@data$NAME_2%in% c("ZINACATEPEC"))]<- "ZINACANTEPEC"
MX@data$NAME_2[which(MX@data$NAME_2%in% c("SAN MARTÍN DE LAS PIRÁÁMIDES"))]<- "SAN MARTIN DE LAS PIRAMIDES"
MX@data$NAME_2[which(MX@data$NAME_2%in% c("TLALNEPANTLA"))]<- "TLALNEPANTLA DE BAZ"


MX@data$NAME_2 <- stri_trans_general(MX@data$NAME_2, "Latin-ASCII")
MX <- MX[MX@data$NAME_2 %in% mort_mun_2021$MUNICIPIO_RES, ]

MX@data <- left_join(MX@data, mort_mun_2021, by = c("NAME_1"= "ENTIDAD_RES",
                                               "NAME_2"= "MUNICIPIO_RES"))
##########

#Valores numéricos
color_num <- colorNumeric(palette = "RdBu",
                          domain = mort_mun$TASA_AJUSTADA, reverse = T)
leaflet() %>%
  addProviderTiles(provider = "CartoDB.PositronNoLabels") %>%
  addPolygons(data = MX ,
              fillColor  = ~color_num(TASA_AJUSTADA),
              color = "black",
              weight = 1,
              label = paste0(MX$NAME_2, ": ", round(MX$TASA_AJUSTADA,0) ),
              smoothFactor = 0.5,
              opacity = -5,
              stroke=TRUE,
              fillOpacity = 0.7) %>%
  addLegend(data = MX, position = "topright", pal = color_num, values = ~TASA_AJUSTADA,
            bins = seq(0,300, by =50),
            title = "Tasa Ajustada Mortalidad por COVID-19")

#POr Bins
color_bins <- colorBin(palette = c("RdBu"),
                       domain = mort_mun$TASA_AJUSTADA,bins = 5, reverse = T)

leaflet() %>%
  addProviderTiles(provider = "CartoDB.PositronNoLabels") %>%
  addPolygons(data = MX ,
              fillColor = ~color_bins(TASA_AJUSTADA) ,
              color = "black",
              weight = 1,
              label = paste0(MX$NAME_2, ": ", round(MX$TASA_AJUSTADA,0) ),
              
              opacity = -9,
              
              stroke=TRUE,
              fillOpacity = 0.9,
              smoothFactor = 0.3
              
  ) %>%
  
  addLegend(data = MX, position = "topright", pal  = color_bins,
            values = ~TASA_AJUSTADA,
            title = "Mortalidad por COVID-19")


##########

# Generamos las zonas

Noroeste <- mort_mun_2021 %>%
  filter(ZONA == "NOROESTE")

Suroeste <- mort_mun_2021 %>%
  filter(ZONA == "SUROESTE")

Centro <- mort_mun_2021 %>%
  filter(ZONA == "CENTRO")

Noreste <- mort_mun_2021 %>%
  filter(ZONA == "NORESTE")

Sureste <- mort_mun_2021 %>%
  filter(ZONA == "SURESTE")

MX_NOROESTE <- MX[MX@data$NAME_2 %in% Noroeste$MUNICIPIO_RES, ]
MX_SUROESTE <- MX[MX@data$NAME_2 %in% Suroeste$MUNICIPIO_RES,]
MX_CENTRO <- MX[MX@data$NAME_2 %in% Centro$MUNICIPIO_RES,]
MX_NORESTE <- MX[MX@data$NAME_2 %in% Noreste$MUNICIPIO_RES,]
MX_SURESTE <- MX[MX@data$NAME_2 %in% Sureste$MUNICIPIO_RES,]


#Valores numéricos
color_num_Noroeste <- colorNumeric(palette = "Reds",
                                   domain = (mort_mun_2021 %>% filter(ZONA == "NOROESTE"))$TASA_AJUSTADA, reverse = F)
color_num_Suroeste <- colorNumeric(palette = "Blues",
                                   domain = (mort_mun_2021 %>% filter(ZONA == "SUROESTE"))$TASA_AJUSTADA, reverse = F)
color_num_Centro <- colorNumeric(palette = "Greens",
                                 domain = (mort_mun_2021 %>% filter(ZONA == "CENTRO"))$TASA_AJUSTADA, reverse = F)

color_num_Noreste <- colorNumeric(palette = "Purples",
                                  domain = (mort_mun_2021 %>% filter(ZONA == "NORESTE"))$TASA_AJUSTADA, reverse = F)

color_num_Sureste <- colorNumeric(palette = "Oranges",
                                  domain = (mort_mun_2021 %>% filter(ZONA == "SURESTE"))$TASA_AJUSTADA, reverse = F)

#Bins
color_num_Noroeste <- colorBin(palette = c("Reds"),
                               domain = (mort_mun_2021 %>% filter(ZONA == "NOROESTE"))$TASA_AJUSTADA,bins = 5, reverse = F)

color_num_Suroeste <- colorBin(palette = "Blues",
                               domain = (mort_mun_2021 %>% filter(ZONA == "SUROESTE"))$TASA_AJUSTADA,bins = 5, reverse = F) 

color_num_Centro <- colorBin(palette = "Greens",
                             domain = (mort_mun_2021 %>% filter(ZONA == "CENTRO"))$TASA_AJUSTADA, bins = 5, reverse = F)

color_num_Noreste <- colorBin(palette = "Purples",
                              domain = (mort_mun_2021 %>% filter(ZONA == "NORESTE"))$TASA_AJUSTADA, bins = 5, reverse = F)

color_num_Sureste <- colorBin(palette = "Oranges",
                              domain = (mort_mun_2021 %>% filter(ZONA == "SURESTE"))$TASA_AJUSTADA,bins = 5, reverse = F)

grosor <- 0.5

# Generamos el mapa

leaflet() %>%
  addProviderTiles(provider = "CartoDB.PositronNoLabels") %>%
  addPolygons(data = MX_NOROESTE ,
              fillColor  = ~color_num_Noroeste(TASA_AJUSTADA),
              color = "black",
              weight = 1,
              label = paste0(MX_NOROESTE$NAME_2, ": ", round(MX_NOROESTE$TASA_AJUSTADA,0) ),
              smoothFactor = 0.5,
              opacity = 1,
              stroke=TRUE,
              fillOpacity = 1
  ) %>%
  
  addPolygons(data = MX_NORESTE ,
              fillColor  = ~color_num_Noreste(TASA_AJUSTADA),
              color = "black",
              weight = 1,
              label = paste0(MX_NORESTE$NAME_2, ": ", round(MX_NORESTE$TASA_AJUSTADA,0) ),
              smoothFactor = 0.5,
              opacity = 1,
              stroke=TRUE,
              fillOpacity = 1
  ) %>%
  
  addPolygons(data = MX_SUROESTE ,
              fillColor  = ~color_num_Suroeste(TASA_AJUSTADA),
              color = "black",
              weight = 1,
              label = paste0(MX_SUROESTE$NAME_2, ": ", round(MX_SUROESTE$TASA_AJUSTADA,0) ),
              smoothFactor = 0.5,
              opacity =1,
              stroke=TRUE,
              fillOpacity = 1
  ) %>%
  addPolygons(data = MX_SURESTE ,
              fillColor  = ~color_num_Sureste(TASA_AJUSTADA),
              color = "black",
              weight = 1,
              label = paste0(MX_SURESTE$NAME_2, ": ", round(MX_SURESTE$TASA_AJUSTADA,0) ),
              smoothFactor = 0.5,
              opacity =1,
              stroke=TRUE,
              fillOpacity = 1
  ) %>%
  addPolygons(data = MX_CENTRO ,
              fillColor  = ~color_num_Centro(TASA_AJUSTADA),
              color = "black",
              weight = 1,
              label = paste0(MX_CENTRO$NAME_2, ": ", round(MX_CENTRO$TASA_AJUSTADA,0) ),
              smoothFactor = 0.5,
              opacity =1,
              stroke=TRUE,
              fillOpacity = 1
  ) %>%
  addLegend(data = MX_CENTRO, position = "topright", pal  = color_num_Centro,
            values = ~TASA_AJUSTADA,
            title = "Mortalidad por COVID-19 en la zona centro por millón de hab.") %>%
  
  addLegend(data = MX_SUROESTE, position = "bottomleft", pal  = color_num_Suroeste,
            values = ~TASA_AJUSTADA,
            title = "Mortalidad por COVID-19 en la zona suroeste por millón de hab.") %>%
  addLegend(data = MX_NOROESTE, position = "topleft", pal  = color_num_Noroeste,
            values = ~TASA_AJUSTADA,
            title = "Mortalidad por COVID-19 en la zona noroeste por millón de hab.") %>%
  addLegend(data = MX_NORESTE, position = "topright", pal  = color_num_Noreste,
            values = ~TASA_AJUSTADA,
            title = "Mortalidad por COVID-19 en la zona noreste por millón de hab.") %>%
  addLegend(data = MX_SURESTE, position = "bottomright", pal  = color_num_Sureste,
            values = ~TASA_AJUSTADA,
            title = "Mortalidad por COVID-19 en la zona sureste por millón de hab.") %>%
  ## Este es para agregar el radio de 5 km de cada estacion
  addCircleMarkers(data = posicion, lat = ~latitud, lng = ~longitud) %>% 
  addCircles(lng = (unique(pm$longitud)), lat = (unique(pm$latitud)),
             radius = 5000, color = "red", fillOpacity = 0.05)

## Localizacion de las estaciones de monitoreo  
posicion <- pm [,.(latitud, longitud, cve_estac)] %>% unique()


##### Lista de municipios dento del radio de 5 km ####

## Sacamos los valores unicos de la variable municipio
unique(mort_mun$MUNICIPIO_RES)

## Hacemos lista de municipios
municipios <- c("ATENCO", "TEXCOCO", "CHICONCUAC", "ECATEPEC DE MORELOS", "COACALCO DE BERRIOZABAL",
                "TLALNEPANTLA DE BAZ", "TULTITLAN", "GUSTAVO A. MADERO", "CUAUTITLAN IZCALLI",
                "NICOLAS ROMERO", "ATIZAPAN DE ZARAGOZA", "NAUCALPAN DE JUAREZ", "AZCAPOTZALCO",
                "CUAUHTEMOC", "NEZAHUALCOYOTL", "VENUSTIANO CARRANZA", "IZTACALCO", "IZTAPALAPA",
                "BENITO JUAREZ", "ALVARO OBREGON", "COYOACAN", "HUIXQUILUCAN", "CUAJIMALPA DE MORELOS",
                "LA MAGDALENA CONTRERAS", "TLALPAN", "XOCHIMILCO", "TLAHUAC", "MILPA ALTA") 



  


  
