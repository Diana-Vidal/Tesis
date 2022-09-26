library(data.table)
library(tidyverse)

variables <- c("ID_REGISTRO", "ENTIDAD_RES", "MUNICIPIO_RES", "FECHA_DEF", "CLASIFICACION_FINAL")

COVID2020 <- fread("Bases/Datos abiertos/COVID19MEXICO2020.csv", select = variables) %>% 
  mutate(FECHA_DEF = as.Date(FECHA_DEF,format = "%Y-%m-%d")) %>%
  filter(!is.na(FECHA_DEF),
         CLASIFICACION_FINAL %in% c(1,2,3)) %>% 
  group_by(ENTIDAD_RES) %>% 
  summarise(NUM_DEF = n())

COVID2021 <- fread("Bases/Datos abiertos/COVID19MEXICO2021.csv", select = variables) %>% 
  mutate(FECHA_DEF = as.Date(FECHA_DEF,format = "%Y-%m-%d")) %>%
  filter(!is.na(FECHA_DEF),
         CLASIFICACION_FINAL %in% c(1,2,3)) %>% 
  filter(FECHA_DEF <= "2021-12-31")

COVIDmerge <- rbind(COVID2020, COVID2021)

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

CONAPO2020 <- rbind(CONAPO1, CONAPO2) %>% 
  filter(AÑO=="2020")

CONAPO2021 <- rbind(CONAPO1, CONAPO2) %>% 
  filter(AÑO=="2021")

mortalidad2020 <- left_join(COVID2020, CONAPO2020, by = c("ENTIDAD_RES" = "CLAVE_ENT"))
