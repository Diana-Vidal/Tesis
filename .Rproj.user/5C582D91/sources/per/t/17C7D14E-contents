#### BASE DATOS METEOROLOGICOS ####

library(data.table)
library(tidyverse)
library(dint)
library(lubridate)
library(naniar)


# Cargamos las bases de los datos meteorologicos de 2019 a 2021
meteo2019 <- fread("Bases/Bases Meteo/meteorología_2019.CSV") %>%
  select(Fecha = date,
         Estacion = id_station,
         Parametro = id_parameter,
         Valor = value,
         Unidad = unit) %>% 
  mutate(Fecha = dmy_hm(Fecha))

meteo2020 <- fread("Bases/Bases Meteo/meteorología_2020.CSV") %>%
  select(Fecha = date,
         Estacion = id_station,
         Parametro = id_parameter,
         Valor = value,
         Unidad = unit) %>% 
  mutate(Fecha = dmy_hm(Fecha))

meteo2021 <- fread("Bases/Bases Meteo/meteorología_2021.CSV") %>%
  select(Fecha = date,
         Estacion = id_station,
         Parametro = id_parameter,
         Valor = value,
         Unidad = unit) %>% 
  mutate(Fecha = dmy_hm(Fecha))


# Cargamos base de zonas
zonas <- fread("Bases/Zonas_ZMVM.csv")

# Cargamos base de estaciones con municipio
# estac_mpo_meteo <- fread("Bases/Bases Meteo/Catálogos/estac_mpo_meteo.csv") 

# Cargamos el catalogo de estaciones
# Quitamos acentos
# Seleccionamos variables
# Unimos con base de municipios meteo
# cat_est <- fread("Bases/Bases Meteo/Catálogos/cat_estacion.csv",
#                  encoding = "Latin-1") %>% 
#   mutate(nombre_estac = chartr("ÁÉÍÓÚ", "AEIOU", nom_estac)) %>% 
#   select(nombre_estac, cve_estac, longitud, latitud) %>% 
#   left_join(., estac_mpo_meteo, by  = c("cve_estac" = "Clave"))
# 
# write.csv(cat_est, "Bases/Bases Meteo/Catálogos/cat_estac_meteo.csv",
#           row.names = F,
#           fileEncoding = "ISO-8859-1")


# Cargamos catalogo de estaciones
cat_estac_meteo <- fread("Bases/Bases Meteo/Catálogos/cat_estac_meteo.csv",
                                           encoding = "Latin-1")


# Unimos las bases de los tres años
# Unimos con catalogo de estaciones
# Unimos con los municipios donde se encuentran las estaciones
# Renombramos las columnas y las acomodammos
# Filtramos los parametros que vamos a utilizar
meteo <- rbind(meteo2019,meteo2020, meteo2021) %>% 
  left_join(., cat_estac_meteo, by = c("Estacion" = "cve_estac")) %>% 
  left_join(., zonas, by = c("Alcaldía o municipio" = "MUNICIPIO",
                             "Entidad" = "ENTIDAD")) %>% 
  rename("fecha" = 1,
         "id_station" = 2,
         "parametro" = 3,
         "value" = 4,
         "nom_estac" = 6,
         "municipio" = 9,
         "entidad" =10,
         "zona" = 11) %>%
  # mutate(fecha = as.Date(fecha, format = "%Y-%m-%d")) %>%
  select(fecha,nom_estac, value, parametro, municipio, entidad, zona) %>% 
  filter(parametro%in%c("RH", "TMP", "WSP")) %>% 
  drop_na(zona)


# write.csv(meteo, "Bases/meteo.csv",
#           row.names = F,
#           fileEncoding = "ISO-8859-1")
# 
# meteo <- fread("Bases/meteo.csv",
#             encoding = "Latin-1")

# meteo <- meteo %>% 
#   sample_frac(.5)


# Calculamos los promedios para cada estacion
# meteo_promed <- meteo %>%
#   filter(nom_estac == "ACOLMAN",
#          parametro == "RH") %>% 
#   select(fecha, nom_estac, value, parametro) %>% 
#   mutate(hora = substring(fecha, 12,19)) %>% 
#   mutate(dia = as.Date(fecha, format = "%Y-%m-%d")) %>% 
#   mutate(dia2 = ifelse(hora == "00:00:00",
#                        as.Date(as.numeric(dia) -1 + as.numeric(as.Date(origin = "1970-01-01"))), 
#                        dia))


# Promedios de la estacion Acolman
# meteo_promed <- meteo %>%
#   filter(nom_estac == "ACOLMAN",
#          parametro == "RH") %>% 
#   select(fecha, nom_estac, value, parametro) %>% 
#   mutate(dia = as.Date(fecha, format = "%Y-%m-%d"),
#          hora = hms::hms(second(fecha), minute(fecha), hour(fecha)),
#          dia2 = if_else(hora == 00:00:00,as.Date((dia)-1), dia)) %>% 
#   group_by(dia2, nom_estac, parametro) %>% 
#   summarise(value = mean(value, na.rm = T))



#### CALCULO DE PROMEDIOS ####

# Revisamos las estaciones que cumplen con más de 18 mediciones en 24 h
# Quitamos valores nulos
# Cambiamos a formato fecha, separamos las horasa y ajustamos la fecha para las 12 h
# Agrupamos por dia, nombre de estacion y parametro y agrupamos por el numero de horas
promedios_meteo <- meteo %>%
  drop_na(value) %>%
  # mutate(nom_estac = recode(nom_estac, "AJUSCO" = "AJUSCO-MEDIO",
  #                           "AJUSCO MEDIO" = "AJUSCO-MEDIO",
  #                           "SANTA FE" = "CUAJIMALPA",
  #                           "SAN AGUSTIN" = "AGUS-XAL",
  #                           "XALOSTOC" = "AGUS-XAL",
  #                           "GUSTAVO A. MADERO" = "GAM-LAA",
  #                           "LABORATORIO DE ANALISIS AMBIENTAL" = "GAM-LAA",
  #                           "UAM IZTAPALAPA" = "UAMIZTA-SANTIAGO",
  #                           "SANTIAGO ACAHUALTEPEC" = "UAMIZTA-SANTIAGO",
  #                           "NEZAHUALCOYOTL" = "NEZA-FES",
  #                           "FES ARAGON" = "NEZA-FES")) %>% 
  mutate(dia = as.Date(fecha, format = "%Y-%m-%d"),
         hora = hms::hms(second(fecha), minute(fecha), hour(fecha)),
         dia2 = if_else(hora == 00:00:00,as.Date((dia)-1), dia)) %>%
  # filter(nom_estac == "UAM XOCHIMILCO") %>% 
    select(dia2, hora, nom_estac, value, parametro) %>% 
  group_by(dia2, nom_estac, parametro) %>% 
  summarise(numhoras = n()) %>% 
  mutate(pasa = if_else(numhoras>= 18, "pasa", "no pasa"))

# Revisamos cuantas estaciones pasan
prop.table(table(promedios_meteo$pasa))


# Revisamos el numero de horas
summary(promedios_meteo$numhoras)


# Filtramos para ver las estaciones que si cumplen con el criterio de representatividad
# promedios_pasa <- promedios_meteo%>% 
#   filter(pasa == "pasa")

# Sacamos el nombre de las estaciones que cumplen el criterio de representatividad
# unique(promedios_pasa$nom_estac)

# # Hacemos una lista con las estaciones
# estac_meteo_pasan <- c("ACOLMAN", "AJUSCO", "AJUSCO MEDIO", "BENITO JUAREZ",
#                        "CHALCO", "CUAJIMALPA", "CUAUTITLAN","FES ACATLAN", "GUSTAVO A. MADERO",
#                        "HOSPITAL GENERAL DE MEXICO", "INVESTIGACIONES NUCLEARES",
#                        "LABORATORIO DE ANALISIS AMBIENTAL", "MERCED", "MIGUEL HIDALGO",
#                        "MILPA ALTA", "MONTECILLO","NEZAHUALCOYOTL", "PEDREGAL",                         
#                        "SAN AGUSTIN", "SANTA FE", "TLAHUAC", "TLALNEPANTLA", 
#                        "UAM IZTAPALAPA", "UAM XOCHIMILCO", "VILLA DE LAS FLORES",
#                        "XALOSTOC", "FES ARAGON", "SANTIAGO ACAHUALTEPEC")


# Promedios de las estaciones
# Quitamos valores nulos
# Cambiamos a formato fecha, separamos las horasa y ajustamos la fecha para las 12 h
# Agrupamos por fecha, nombre de estacion, parametros, municipio, entidad y zona
# Calculamos el promedio
prom_meteo <- meteo %>%
  drop_na(value) %>% 
  mutate(dia = as.Date(fecha, format = "%Y-%m-%d"),
         hora = hms::hms(second(fecha), minute(fecha), hour(fecha)),
         dia2 = if_else(hora == 00:00:00,as.Date((dia)-1), dia)) %>% 
  group_by(dia2, nom_estac, parametro, municipio, entidad, zona) %>% 
  summarise(value = mean(value, na.rm = T)) %>% 
  rename(fecha = dia2) %>% 
  select(fecha, nom_estac, value, parametro, municipio, entidad, zona)

# Unimos base de promedios con la de promedios pasa
meteo_promedios <- left_join(prom_meteo, promedios_meteo, by = c("fecha" = "dia2",
                                                                           "nom_estac",
                                                                           "parametro")) %>% 
  filter(pasa == "pasa")

# Revismos que solo esten lo valores que si cumplen con criterio de representatividad
prop.table(table(meteo_promedios$pasa))

unique(meteo_promedios$pasa)


# Agrupamos por municipio
prom_meteo_mun <- meteo_promedios %>% 
  group_by(fecha, parametro, municipio, entidad, zona) %>% 
  summarise(value= mean(value))


# Agrupamos por zona
prom_meteo_zona <- meteo_promedios %>% 
  group_by(fecha, parametro, zona) %>% 
  summarise(value= mean(value))




  



