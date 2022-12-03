#### CALCULO DE TASAS DE MORTALIDAD CRUDAS Y AJUSTADAS

# Cargamos librerias
library(data.table)
library(tidyverse)

# Creamos vector con variables a utilizar de Datos Abiertos
variables <- c("ID_REGISTRO", "ENTIDAD_UM", "FECHA_DEF", "CLASIFICACION_FINAL")

##### CALCULO DE MORTALIDAD POR ESTADO 2020 Y 2021 ####

# Cargamos la base de COVID-19 Datos Abiertos 2020
COVID2020 <- fread("Bases/Datos abiertos/COVID19MEXICO2020.csv",
                   select = variables) %>% 
  mutate(FECHA_DEF = as.Date(FECHA_DEF,format = "%Y-%m-%d")) %>%
  add_column(., AÑO = 2020) %>% 
  filter(!is.na(FECHA_DEF),
         CLASIFICACION_FINAL %in% c(1,2,3)) %>%  
  group_by(ENTIDAD_UM, AÑO) %>%
  summarise(NUM_DEF = n())

# Cargamos la base de COVID-19 Datos Abiertos 2021
COVID2021 <- fread("Bases/Datos abiertos/COVID19MEXICO2021.csv",
                   select = variables) %>% 
  mutate(FECHA_DEF = as.Date(FECHA_DEF,format = "%Y-%m-%d")) %>%
  add_column(., AÑO = 2021) %>% 
  filter(!is.na(FECHA_DEF),
         CLASIFICACION_FINAL %in% c(1,2,3)) %>% 
  filter(FECHA_DEF <= "2021-12-31") %>% 
  group_by(ENTIDAD_UM, AÑO) %>%
  summarise(NUM_DEF = n())

# Unimos ambas bases para juntar las defunciones de 2020 y 2021
COVID2020_2021 <- rbind(COVID2020, COVID2021) 
  # group_by(ENTIDAD_UM) %>% 
  # summarise(NUM_DEF=n())

# Cargamos bases de CONAPO 1/2
CONAPO1 <- fread("Bases/CONAPO/base_municipios_final_datos_01.csv", encoding = "Latin-1") %>% 
  rename("AÑO" = 7) %>% 
  filter(AÑO %in% c("2020","2021")) %>% 
  group_by(CLAVE_ENT, NOM_ENT, AÑO) %>% 
  summarise(POB=sum(POB))

# CONAPO2 <- fread("Bases/CONAPO/base_municipios_final_datos_02.csv", encoding = "Latin-1") %>%
#   rename("AÑO" = 7) %>% 
#   filter(AÑO %in% c("2020","2021")) %>% 
#   group_by(CLAVE_ENT, NOM_ENT, AÑO) %>% 
#   summarise(POB=sum(POB))
# 
# write.csv(CONAPO2, "Bases/CONAPO/CONAPO2.csv", row.names = F, fileEncoding = "ISO-8859-1")

CONAPO2 <- fread("Bases/CONAPO/CONAPO2.csv", encoding = "Latin-1")

CONAPO2020_2021 <- rbind(CONAPO1, CONAPO2)

# Descargamos base de CONAPO para sacar poblacion total de 2020 y 2021
write.csv(CONAPO2020_2021,"Bases/CONAPO/CONAPO.csv",
          row.names = F,
          fileEncoding = "ISO-8859-1")

# Creamos la base de mortalidad y calculamos la tasa cruda para cada estado
mortalidad <- left_join(COVID2020_2021, CONAPO2020_2021, by = c("ENTIDAD_UM" = "CLAVE_ENT",
                                                                "AÑO" = "AÑO")) %>% 
  mutate(TASA_MORTALIDAD = (NUM_DEF/POB)*10000)

# Descargamos base de mortalidad para realizar el ajuste de tasas
write.csv(mortalidad, "Bases/CONAPO/mortalidad.csv",
          row.names = F,
          fileEncoding = "ISO-8859-1")

# Volvemos a subir las bases con tasas ajustadas
mortalidad_aj <- fread("Bases/CONAPO/mortalidad.csv",
                       encoding = "Latin-1")

##### CALCULO DE MORTALIDAD POR ALCALDIA/MUNICIPIO 2020 Y 2021 ####

# Creamos base de mortalidad por alcaldia/municipio
# Seleccionamos las variables que queremos y agrupamos
# Sumamos el numero de defunciones 
# Creamos las columnas de tasa de mortalidad cruda y ajustada
mort_mun <- def_zmvm %>%
  group_by(ENTIDAD_RES, MUNICIPIO_RES, AÑO,POB_MUN, POB_ENT, ZONA) %>% 
  summarise(NUM_DEF=n()) %>% 
  mutate(TASA_MORTALIDAD = (NUM_DEF/POB_MUN)*100000,
         TASA_AJUSTADA = (NUM_DEF/POB_ENT)*1000000)

# Guardamos la base creada
write.csv(mort_mun, "Bases/mort_mun.csv",
          row.names = F,
          fileEncoding = "ISO-8859-1")

# Cargamos los datos para crear el mapa
MX <- getData("GADM", country = "MX", level = 2)

MX@data$NAME_1[which(MX@data$NAME_1%in% c("Distrito Federal"))]<- "Ciudad de México"
MX@data$NAME_1 <- toupper(MX@data$NAME_1)
MX@data$NAME_1 <- stri_trans_general(MX@data$NAME_1, "Latin-ASCII")
MX@data$NAME_2 <- toupper(MX@data$NAME_2)
MX@data$NAME_2 <- stri_trans_general(MX@data$NAME_2, "Latin-ASCII")

MX@data <- MX@data %>% 
  filter(NAME_1 %in% c("CIUDAD DE MEXICO", "MEXICO"))

MX@data <- left_join(MX@data, mort_mun,
                     by = c("NAME_1" = "ENTIDAD_RES"
                            ,"NAME_2"= "MUNICIPIO_RES")) %>% 
  filter(!is.na(ZONA))

#Valores numéricos
color_num <- colorNumeric(palette = "RdBu",
                          domain = mort_mun$TASA_AJUSTADA, reverse = T)


# Hacemos mapa por municipios
leaflet() %>%
  addProviderTiles(provider = "CartoDB.PositronNoLabels") %>%
  addPolygons(data = MX@data,
              fillColor  = ~color_num(mort_mun$TASA_AJUSTADA),
              color = "black",
              weight = 1,
              label = paste0(MX$NAME_2, ": ", round(mort_mun$TASA_AJUSTADA,0) ),
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5) %>%
  addLegend(data = MX, position = "topright", pal = color_num, values = ~mort_mun$TASA_AJUSTADA,
            bins = seq(0,400, by =50), #Aqui modificar el valor mínimo al maximo por x
            title = "Mortalidad por COVID-19 durante 2020")


