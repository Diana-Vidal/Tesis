#### LIMPIEZA DE BASE PM2.5 ####

library(data.table)
library(tidyverse)

# Cargamos las bases de los promedios de PM2.5 de 2019 a 2021
pm2019 <- fread("Bases/Bases PM/promedios_2019_ps.csv")
pm2020 <- fread("Bases/Bases PM/promedios_2020_ps.csv")
pm2021 <- fread("Bases/Bases PM/promedios_2021_ps.csv")

#Cargamos el catalogo de estaciones
cat_est <- fread("Bases/Bases PM/Catálogos/cat_estacion.csv")

# Cargamos base de estaciones con municipio
estac_mpo <- fread("Bases/Bases PM/Catálogos/estac_mpo.csv")

# Unimos las bases de los tres años
# Unimos con catalogo de estaciones
# Unimos con los municipios donde se encuentran las estaciones
pm <- rbind(pm2019,pm2020, pm2021) %>% 
  left_join(., cat_est, by = c("id_station" = "cve_estac")) %>% 
  left_join(.,estac_mpo, by = c("nom_estac" = "Nombre")) %>%
  left_join(., zonas, by = c("Alcaldía o municipio" = "MUNICIPIO",
                             "Entidad" = "ENTIDAD")) %>% 
  rename("fecha" = 1,
         "cve_estac" = 2,
         "parametro" = 3,
         "municipio" = 13,
         "entidad" =14,
         "zona" = 15) %>%
  mutate(fecha = as.Date(fecha, format = "%Y-%m-%d"),
         parametro = trimws(parametro, "r")) %>%
  select(fecha, cve_estac, nom_estac, value, parametro, longitud, latitud, municipio, entidad, zona)


# Guardamos como csv
write.csv(pm, "Bases/pm.csv",
          row.names = F,
          fileEncoding = "ISO-8859-1")

# Cargamos base de pm
pm <- fread("Bases/pm.csv",
            encoding = "Latin-1")

#### CREAMOS BASE PARA EMPATAR PARA REGRESION DE POISSON ####

# Se calculan los promedios de estaciones que se encuentran en el mismo municipio/
# alcaldia
# promedios <- pm %>% 
#   filter(nom_estac%in%c("UAM XOCHIMILCO", "CENTRO DE CIENCIAS DE LA ATMOSFERA",
#                         "UAM IZTAPALAPA", "SANTIAGO ACAHUALTEPEC",
#                         "NEZAHUALCOYOTL", "FES ARAGON",
#                         "AJUSCO", "AJUSCO MEDIO"))%>% 
#   mutate(nom_estac = recode(nom_estac, "UAM XOCHIMILCO" = "UAMXOCHI-CCA",
#                             "CENTRO DE CIENCIAS DE LA ATMOSFERA" = "UAMXOCHI-CCA",
#                             "UAM IZTAPALAPA" = "UAMIZTA-SANTIAGO",
#                             "SANTIAGO ACAHUALTEPEC" = "UAMIZTA-SANTIAGO",
#                             "NEZAHUALCOYOTL" = "NEZA-FES",
#                             "FES ARAGON" = "NEZA-FES",
#                             "AJUSCO" = "AJUSCO-MEDIO",
#                             "AJUSCO MEDIO" = "AJUSCO-MEDIO")) %>% 
#   group_by(fecha, nom_estac, parametro, municipio, entidad, zona) %>% 
#   summarise(value = mean(value,na.rm = T)) %>% 
#   select(fecha, nom_estac, value, parametro, municipio, entidad, zona)
# 
# 
# pm_2 <- pm %>% 
#   filter(!nom_estac%in%c("UAM XOCHIMILCO", "CENTRO DE CIENCIAS DE LA ATMOSFERA",
#                         "UAM IZTAPALAPA", "SANTIAGO ACAHUALTEPEC",
#                         "NEZAHUALCOYOTL", "FES ARAGON",
#                         "AJUSCO", "AJUSCO MEDIO"))
# 
# pm_3 <- rbind(pm_2, promedios) %>% 
#   filter(!is.na(zona))
# 
# pm_final <- pm_3 %>% 
#   left_join(., promedios_meteo, by = c("nom_estac" = "nom_estac"))


# Agrupamos por municipio
prom_pm_mun <- pm %>% 
  drop_na(zona) %>% 
  group_by(fecha, parametro, municipio, entidad, zona) %>% 
  summarise(value = mean(value))


# Agrupamos por zona
prom_pm_zona <- pm %>%
  drop_na(zona) %>%
  group_by(fecha, parametro, zona) %>% 
  summarise(value= mean(value))


