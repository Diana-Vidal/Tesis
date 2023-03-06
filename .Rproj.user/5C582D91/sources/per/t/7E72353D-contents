#### LIMPIEZA DE BASES PAEA DEFUNCIONES POR COVID-19 ####

# Cargamos librerias
library(data.table)
library(tidyverse)


# Creamos vector con variables a utilizar de Datos Abiertos
variables <- c("ID_REGISTRO", "SEXO", "ENTIDAD_RES", "MUNICIPIO_RES","FECHA_SINTOMAS", "FECHA_DEF", "EDAD",
               "DIABETES", "EPOC", "ASMA", "HIPERTENSION", "CARDIOVASCULAR", "OBESIDAD", "RENAL_CRONICA",
               "CLASIFICACION_FINAL")

# Cargamos catalogo de municipios que vamos a utilzar para obtener nombres
catalogo_municipios <- fread("Bases/Catalogo_Municipios.csv")


#### 2020 ####

# Cargamos la base de COVID-19 Datos Abiertos 2020
# Modificamos el formato de fecha
# Renombramos las variables
# Creamos variable de grupo de edad
# Filtramos Clasificacion final por confirmados a COVID-19
# Unimos base con el catalogo de municipios
# Creamos factores las variables cualitativas
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


#### 2021 ####

# Cargamos la base de COVID-19 Datos Abiertos 2021
# Modificamos el formato de fecha
# Renombramos las variables
# Creamos variable de grupo de edad
# Filtramos Clasificacion final por confirmados a COVID-19
# Unimos base con el catalogo de municipios
# Creamos factores las variables cualitativas
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


#### DEFUNCIONES DE ZMVM EN 2020 Y 2021 ####

# Cargamos base con zonas geograficas de la ZMVM
zonas <- fread("Bases/Zonas_ZMVM.csv")

# Cargamos base con población por alcaldía/municipio y por año
pob_zmvm <- fread("Bases/Municipios_ZMVM_Pob.csv") %>% 
  select(-2)

# Unimos las bases de defunciones de COVID-19 del 2020 y 2021
# Seleccionamos las variables a utilizar
# Filtramos las defunciones de la CDMX y el EDOMEX
# Creamos columna con los nombres de los municipios sin acentos
# Unimos la base de zonas geograficas de acuerdo a Entidad y Municipio
# Eliminamos los Municipios que no pertenecen a la ZMVM (los que quedaron con NA)
# Unimos la base de poblaciones por alcaldia/municipio
def_zmvm <- rbind(COVID2020, COVID2021) %>%
  select(FECHA_DEF, FECHA_SINTOMAS, AÑO, ENTIDAD_RES, MUNICIPIO, SEXO, EDAD, GRUPO_EDAD, DIABETES, EPOC, ASMA, HIPERTENSION,
         CARDIOVASCULAR, OBESIDAD, RENAL_CRONICA) %>% 
  filter(ENTIDAD_RES %in% c("CIUDAD DE MEXICO", "MEXICO")) %>% 
  mutate(MUNICIPIO_RES = chartr("ÁÉÍÓÚ", "AEIOU", MUNICIPIO),
         GRUPO_EDAD = factor(GRUPO_EDAD, levels = c("0 - 4",
                                                    "5 - 9",
                                                    "10 - 14",
                                                    "15 - 19",
                                                    "20 - 24",
                                                    "25 - 29",
                                                    "30 - 34",
                                                    "35 - 39",
                                                    "40 - 44",
                                                    "45 - 49",
                                                    "50 - 54",
                                                    "55 - 59",
                                                    "60 - 64",
                                                    "65 o más"))) %>% 
  left_join(., zonas, by = c("ENTIDAD_RES" = "ENTIDAD",
                             "MUNICIPIO_RES" = "MUNICIPIO")) %>% 
  drop_na(ZONA) %>% 
  left_join(., pob_zmvm, by = c("MUNICIPIO_RES" = "MUNICIPIO",
                                "AÑO" = "AÑO")) %>% 
  select(-5)

# Guardamos como csv la base creada
write.csv(def_zmvm, "Bases/def_zmvm.csv",
          row.names = F,
          fileEncoding = "ISO-8859-1")


#### CREAMOS BASE PARA EMPATAR PARA REGRESION DE POISSON ####

def_zmvm <- fread("Bases/def_zmvm.csv",
                  encoding = "Latin-1")


# Agrupamos por municipio
def_mun <- def_zmvm %>% 
  group_by(FECHA_DEF, MUNICIPIO_RES, ZONA) %>% 
  summarise(num_def = n())%>% 
  rename(fecha = FECHA_DEF,
         municipio = MUNICIPIO_RES,
         zona = ZONA)


# Agrupamos por zona
def_zona <- def_zmvm %>% 
  group_by(FECHA_DEF, ZONA) %>% 
  summarise(num_def = n())%>% 
  rename(fecha = FECHA_DEF,
         zona = ZONA)



