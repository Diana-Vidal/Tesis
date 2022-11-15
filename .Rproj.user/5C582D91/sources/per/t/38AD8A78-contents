#### ANALISIS DESCRIPTIVO: DEFUNCIONES POR COVID-19 ####

#Cargamos librerias
library(data.table)
library(tidyverse)


#Creamos vector con variables a utilizar de Datos Abiertos
variables <- c("ID_REGISTRO", "SEXO", "ENTIDAD_RES", "MUNICIPIO_RES","FECHA_DEF", "EDAD",
               "DIABETES", "EPOC", "ASMA", "HIPERTENSION", "CARDIOVASCULAR", "OBESIDAD", "RENAL_CRONICA",
               "CLASIFICACION_FINAL")

#Cargamos catalogo de municipios que vamos a utilzar para obtener nombres
catalogo_municipios <- fread("Bases/Catalogo_Municipios.csv")


#### 2020 ####

#Cargamos la base de COVID-19 Datos Abiertos 2020
#Modificamos el formato de fecha
#Renombramos las variables
#Creamos variable de grupo de edad
#Filtramos Clasificacion final por confirmados a COVID-19
#Unimos base con el catalogo de municipios
COVID2020 <- fread("Bases/Datos abiertos/COVID19MEXICO2020.csv",
                   select = variables) %>% 
  mutate(FECHA_DEF = as.Date(FECHA_DEF,format = "%Y-%m-%d"),
         SEXO = recode(SEXO,
                       "1" = "MUJER",
                       "2" = "HOMBRE",
                       "3" = "NO ESPECIFICADO"),
         ENTIDAD_RES = recode(ENTIDAD_RES,
                              "1" = "AGUASCALIENTES",
                              "2" = "BAJA CALIFORNIA",
                              "3" = "BAJA CALIFORNIA SUR",
                              "4" = "CAMPECHE",
                              "5" = "COAHUILA",
                              "6" = "COLIMA",
                              "7" = "CHIAPAS",
                              "8" = "CHIHUAHUA",
                              "9" = "CIUDAD DE MEXICO",
                              "10" = "DURANGO",
                              "11" = "GUANAJUATO",
                              "12" = "GUERRERO",
                              "13" = "HIDALGO",
                              "14" = "JALISCO",
                              "15" = "MEXICO",
                              "16" = "MICHOACAN",
                              "17" = "MORELOS",
                              "18" = "NAYARIT",
                              "19" = "NUEVO LEON",
                              "20" = "OAXACA",
                              "21" = "PUEBLA",
                              "22" = "QUERETARO",
                              "23" = "QUINTANA ROO",
                              "24" = "SAN LUIS POTOSI",
                              "25" = "SINALOA",
                              "26" = "SONORA",
                              "27" = "TABASCO",
                              "28" = "TAMAULIPAS",
                              "29" = "TLAXCALA",
                              "30" = "VERACRUZ",
                              "31" = "YUCATAN",
                              "32" = "ZACATECAS",
                              "36" = "ESTADOS UNIDOS MEXICANOS",
                              "97" = "NO APLICA",
                              "98" = "SE IGNORA",
                              "99" = "NO ESPECIFICADO"),
         DIABETES = recode(DIABETES,
                           "1" = "SI", 
                           "2" = "NO",
                           "97" = "NO APLICA",
                           "98" = "SE IGNORA",
                           "99" = "NO ESPECIFICADO"),
         EPOC = recode(EPOC,
                           "1" = "SI", 
                           "2" = "NO",
                           "97" = "NO APLICA",
                           "98" = "SE IGNORA",
                           "99" = "NO ESPECIFICADO"),
         ASMA = recode(ASMA,
                           "1" = "SI", 
                           "2" = "NO",
                           "97" = "NO APLICA",
                           "98" = "SE IGNORA",
                           "99" = "NO ESPECIFICADO"),
         HIPERTENSION = recode(HIPERTENSION,
                           "1" = "SI", 
                           "2" = "NO",
                           "97" = "NO APLICA",
                           "98" = "SE IGNORA",
                           "99" = "NO ESPECIFICADO"),
         CARDIOVASCULAR = recode(CARDIOVASCULAR,
                           "1" = "SI", 
                           "2" = "NO",
                           "97" = "NO APLICA",
                           "98" = "SE IGNORA",
                           "99" = "NO ESPECIFICADO"),
         OBESIDAD = recode(OBESIDAD,
                           "1" = "SI", 
                           "2" = "NO",
                           "97" = "NO APLICA",
                           "98" = "SE IGNORA",
                           "99" = "NO ESPECIFICADO"),
         RENAL_CRONICA = recode(RENAL_CRONICA,
                           "1" = "SI", 
                           "2" = "NO",
                           "97" = "NO APLICA",
                           "98" = "SE IGNORA",
                           "99" = "NO ESPECIFICADO"),
         GRUPO_EDAD = factor(case_when(EDAD >= 0 & EDAD <= 4 ~ '0 - 4',
                                       EDAD >= 5  & EDAD <= 9 ~ '5 - 9',
                                       EDAD >= 10  & EDAD <= 14 ~ '10 - 14',
                                       EDAD >= 15  & EDAD <= 19 ~ '15 - 19',
                                       EDAD >= 20  & EDAD <= 24 ~ '20 - 24',
                                       EDAD >= 25  & EDAD <= 29 ~ '25 - 29',
                                       EDAD >= 30  & EDAD <= 34 ~ '30 - 34',
                                       EDAD >= 35  & EDAD <= 39 ~ '35 - 39',
                                       EDAD >= 40  & EDAD <= 44 ~ '40 - 44',
                                       EDAD >= 45  & EDAD <= 49 ~ '45 - 49',
                                       EDAD >= 50  & EDAD <= 54 ~ '50 - 54',
                                       EDAD >= 55  & EDAD <= 59 ~ '55 - 59',
                                       EDAD >= 60  & EDAD <= 64 ~ '60 - 64',
                                       EDAD >= 65 ~ '65 o más'))) %>%
  add_column(., AÑO = 2020) %>% 
  filter(!is.na(FECHA_DEF),
         CLASIFICACION_FINAL %in% c(1,2,3)) %>% 
  left_join(., catalogo_municipios, by = c("ENTIDAD_RES" = "ENTIDAD", "MUNICIPIO_RES" = "CLAVE_MUNICIPIO")) %>% 
  mutate(SEXO = as.factor(SEXO),
         DIABETES = as.factor(DIABETES),
         EPOC = as.factor(EPOC),
         ASMA = as.factor(ASMA),
         HIPERTENSION = as.factor(HIPERTENSION),
         CARDIOVASCULAR = as.factor(CARDIOVASCULAR),
         OBESIDAD = as.factor(OBESIDAD),
         RENAL_CRONICA = as.factor(RENAL_CRONICA))
  

# Estadisticas descriptivas
summary(COVID2020)

## Categoricas
table(COVID2020$DIABETES)
prop.table(table(COVID2020$DIABETES))

table(COVID2020$EPOC)
prop.table(table(COVID2020$EPOC))

table(COVID2020$ASMA)
prop.table(table(COVID2020$ASMA))

table(COVID2020$HIPERTENSION)
prop.table(table(COVID2020$HIPERTENSION))

table(COVID2020$CARDIOVASCULAR)
prop.table(table(COVID2020$CARDIOVASCULAR))

table(COVID2020$OBESIDAD)
prop.table(table(COVID2020$OBESIDAD))

table(COVID2020$RENAL_CRONICA)
prop.table(table(COVID2020$RENAL_CRONICA))

## Continuas
sd(COVID2020$EDAD)

#### 2021 ####

#Cargamos la base de COVID-19 Datos Abiertos 2020
#Modificamos el formato de fecha
#Renombramos las variables
#Creamos variable de grupo de edad
#Filtramos Clasificacion final por confirmados a COVID-19
#Unimos base con el catalogo de municipios
COVID2021 <- fread("Bases/Datos abiertos/COVID19MEXICO2021.csv",
                   select = variables) %>% 
  mutate(FECHA_DEF = as.Date(FECHA_DEF,format = "%Y-%m-%d"),
         SEXO = recode(SEXO,
                       "1" = "MUJER",
                       "2" = "HOMBRE",
                       "3" = "NO ESPECIFICADO"),
         ENTIDAD_RES = recode(ENTIDAD_RES,
                              "1" = "AGUASCALIENTES",
                              "2" = "BAJA CALIFORNIA",
                              "3" = "BAJA CALIFORNIA SUR",
                              "4" = "CAMPECHE",
                              "5" = "COAHUILA",
                              "6" = "COLIMA",
                              "7" = "CHIAPAS",
                              "8" = "CHIHUAHUA",
                              "9" = "CIUDAD DE MEXICO",
                              "10" = "DURANGO",
                              "11" = "GUANAJUATO",
                              "12" = "GUERRERO",
                              "13" = "HIDALGO",
                              "14" = "JALISCO",
                              "15" = "MEXICO",
                              "16" = "MICHOACAN",
                              "17" = "MORELOS",
                              "18" = "NAYARIT",
                              "19" = "NUEVO LEON",
                              "20" = "OAXACA",
                              "21" = "PUEBLA",
                              "22" = "QUERETARO",
                              "23" = "QUINTANA ROO",
                              "24" = "SAN LUIS POTOSI",
                              "25" = "SINALOA",
                              "26" = "SONORA",
                              "27" = "TABASCO",
                              "28" = "TAMAULIPAS",
                              "29" = "TLAXCALA",
                              "30" = "VERACRUZ",
                              "31" = "YUCATAN",
                              "32" = "ZACATECAS",
                              "36" = "ESTADOS UNIDOS MEXICANOS",
                              "97" = "NO APLICA",
                              "98" = "SE IGNORA",
                              "99" = "NO ESPECIFICADO"),
         DIABETES = recode(DIABETES,
                           "1" = "SI", 
                           "2" = "NO",
                           "97" = "NO APLICA",
                           "98" = "SE IGNORA",
                           "99" = "NO ESPECIFICADO"),
         EPOC = recode(EPOC,
                       "1" = "SI", 
                       "2" = "NO",
                       "97" = "NO APLICA",
                       "98" = "SE IGNORA",
                       "99" = "NO ESPECIFICADO"),
         ASMA = recode(ASMA,
                       "1" = "SI", 
                       "2" = "NO",
                       "97" = "NO APLICA",
                       "98" = "SE IGNORA",
                       "99" = "NO ESPECIFICADO"),
         HIPERTENSION = recode(HIPERTENSION,
                               "1" = "SI", 
                               "2" = "NO",
                               "97" = "NO APLICA",
                               "98" = "SE IGNORA",
                               "99" = "NO ESPECIFICADO"),
         CARDIOVASCULAR = recode(CARDIOVASCULAR,
                                 "1" = "SI", 
                                 "2" = "NO",
                                 "97" = "NO APLICA",
                                 "98" = "SE IGNORA",
                                 "99" = "NO ESPECIFICADO"),
         OBESIDAD = recode(OBESIDAD,
                           "1" = "SI", 
                           "2" = "NO",
                           "97" = "NO APLICA",
                           "98" = "SE IGNORA",
                           "99" = "NO ESPECIFICADO"),
         RENAL_CRONICA = recode(RENAL_CRONICA,
                                "1" = "SI", 
                                "2" = "NO",
                                "97" = "NO APLICA",
                                "98" = "SE IGNORA",
                                "99" = "NO ESPECIFICADO"),
         GRUPO_EDAD = factor(case_when(EDAD >= 0 & EDAD <= 4 ~ '0 - 4',
                                       EDAD >= 5  & EDAD <= 9 ~ '5 - 9',
                                       EDAD >= 10  & EDAD <= 14 ~ '10 - 14',
                                       EDAD >= 15  & EDAD <= 19 ~ '15 - 19',
                                       EDAD >= 20  & EDAD <= 24 ~ '20 - 24',
                                       EDAD >= 25  & EDAD <= 29 ~ '25 - 29',
                                       EDAD >= 30  & EDAD <= 34 ~ '30 - 34',
                                       EDAD >= 35  & EDAD <= 39 ~ '35 - 39',
                                       EDAD >= 40  & EDAD <= 44 ~ '40 - 44',
                                       EDAD >= 45  & EDAD <= 49 ~ '45 - 49',
                                       EDAD >= 50  & EDAD <= 54 ~ '50 - 54',
                                       EDAD >= 55  & EDAD <= 59 ~ '55 - 59',
                                       EDAD >= 60  & EDAD <= 64 ~ '60 - 64',
                                       EDAD >= 65 ~ '65 o más'))) %>%
  add_column(., AÑO = 2021) %>% 
  filter(!is.na(FECHA_DEF),
         CLASIFICACION_FINAL %in% c(1,2,3),
         FECHA_DEF <= "2021-12-31") %>%
  left_join(., catalogo_municipios, by = c("ENTIDAD_RES" = "ENTIDAD", "MUNICIPIO_RES" = "CLAVE_MUNICIPIO")) %>% 
  mutate(SEXO = as.factor(SEXO),
         DIABETES = as.factor(DIABETES),
         EPOC = as.factor(EPOC),
         ASMA = as.factor(ASMA),
         HIPERTENSION = as.factor(HIPERTENSION),
         CARDIOVASCULAR = as.factor(CARDIOVASCULAR),
         OBESIDAD = as.factor(OBESIDAD),
         RENAL_CRONICA = as.factor(RENAL_CRONICA))

# Estadisticas descriptivas
summary(COVID2021)

## Categoricas
table(COVID2021$DIABETES)
prop.table(table(COVID2021$DIABETES))

table(COVID2021$EPOC)
prop.table(table(COVID2021$EPOC))

table(COVID2021$ASMA)
prop.table(table(COVID2021$ASMA))

table(COVID2021$HIPERTENSION)
prop.table(table(COVID2021$HIPERTENSION))

table(COVID2021$CARDIOVASCULAR)
prop.table(table(COVID2021$CARDIOVASCULAR))

table(COVID2021$OBESIDAD)
prop.table(table(COVID2021$OBESIDAD))

table(COVID2021$RENAL_CRONICA)
prop.table(table(COVID2021$RENAL_CRONICA))

## Continuas
sd(COVID2021$EDAD)

#### ZMVM ####

def_zmvm <- rbind(COVID2020, COVID2021) %>%
  select(FECHA_DEF, AÑO, ENTIDAD_RES, MUNICIPIO, SEXO, EDAD, GRUPO_EDAD, DIABETES, EPOC, ASMA, HIPERTENSION,
         CARDIOVASCULAR, OBESIDAD, RENAL_CRONICA) %>% 
  filter(ENTIDAD_RES %in% c("CIUDAD DE MEXICO", "MEXICO")) %>% 
  mutate(MUNICIPIO_RES = chartr("ÁÉÍÓÚ", "AEIOU", MUNICIPIO))



