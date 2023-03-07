#### CALCULO DE TASAS DE MORTALIDAD CRUDAS Y AJUSTADAS

# Cargamos librerias
library(data.table)
library(tidyverse)
library(dplyr)


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
  group_by(CLAVE_ENT, NOM_ENT, AÑO,) %>% 
  summarise(POB=sum(POB))

# Cargamos bases de CONAPO 1 para sacar grupos de edad
CONAPO_gpoedad <- fread("Bases/CONAPO/base_municipios_final_datos_01.csv", encoding = "Latin-1") %>% 
  rename("AÑO" = 7) %>% 
  filter(AÑO %in% c("2020","2021"),
         CLAVE_ENT %in% c("9", "15")) %>% 
  group_by(CLAVE_ENT, NOM_ENT, CLAVE, MUN, AÑO, EDAD_QUIN) %>% 
  summarise(POB=sum(POB))

# Descargamos base
write.csv(CONAPO_gpoedad,"Bases/CONAPO/CONAPO_gpoedad.csv",
          row.names = F,
          fileEncoding = "ISO-8859-1")

# Cargamos catalogo de municipios que vamos a utilzar para obtener nombres
catalogo_municipios_gpoedad <- fread("Bases/Catalogo_Municipios.csv") %>% 
  filter(CLAVE_ENTIDAD %in% c("9", "15"))

# Descargamos base
write.csv(catalogo_municipios_gpoedad,"Bases/CONAPO/catalogo_municipios_gpoedad.csv",
          row.names = F,
          fileEncoding = "ISO-8859-1")

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

#### CALCULO DE MORTALIDAD POR GRUPO DE EDAD 2020 Y 2021 ####

#Cargamos bases para unirlas
CONAPO_gpoedad <- fread("Bases/CONAPO/CONAPO_gpoedad.csv",
                        encoding = "Latin-1")

catalogo_municipios_gpoedad <- fread("Bases/CONAPO/catalogo_municipios_gpoedad.csv",
                                     encoding = "Latin-1")

# Unimos base y renombramos
gpo_edad <- left_join(CONAPO_gpoedad, catalogo_municipios_gpoedad,
                      by = c("CLAVE_ENT" = "CLAVE_ENTIDAD",
                             "CLAVE" = "CLAVE_MUNICIPIO")) %>% 
  select(AÑO, ENTIDAD, MUNICIPIO, EDAD_QUIN, POB) %>% 
  rename(GPO_EDAD_POB = "POB") %>% 
  mutate(EDAD_QUIN = recode(EDAD_QUIN,
                            "pobm_00_04" = "0 - 4",
                            "pobm_05_09" = "5 - 9",
                            "pobm_10_14" = "10 - 14",
                            "pobm_15_19" = "15 - 19",
                            "pobm_20_24" = "20 - 24",
                            "pobm_25_29" = "25 - 29",
                            "pobm_30_34" = "30 - 34",
                            "pobm_35_39" = "35 - 39",
                            "pobm_40_44" = "40 - 44",
                            "pobm_45_49" = "45 - 49",
                            "pobm_50_54" = "50 - 54",
                            "pobm_55_59" = "55 - 59",
                            "pobm_60_64" = "60 - 64",
                            "pobm_65_mm" = "65 o más"),
         MUNICIPIO = chartr("ÁÉÍÓÚ", "AEIOU", MUNICIPIO))

# Unimos con base de defunciones
def_gpoedad <- left_join(def_zmvm, gpo_edad,
                         by = c("AÑO" = "AÑO",
                                "ENTIDAD_RES" = "ENTIDAD",
                                "MUNICIPIO_RES" = "MUNICIPIO",
                                "GRUPO_EDAD" = "EDAD_QUIN"))

# Calculamos mortalidad por grupo de edad por 100,000 hab
mort_mun_gpoedad <- def_gpoedad %>%
  group_by(ENTIDAD_RES, MUNICIPIO_RES, ZONA, AÑO, GRUPO_EDAD, GPO_EDAD_POB) %>% 
  summarise(NUM_DEF=n()) %>% 
  mutate(TASA_MORTALIDAD = (NUM_DEF/GPO_EDAD_POB)*100000)

# Descargamos base
write.csv(mort_mun_gpoedad,"Bases/mort_mun_gpoedad.csv",
          row.names = F,
          fileEncoding = "ISO-8859-1")

#Cargamos base
mort_mun_gpoedad <- fread("Bases/mort_mun_gpoedad.csv",
                              encoding = "Latin-1")


