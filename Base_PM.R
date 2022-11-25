#### BASE PM2.5 ####

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
         "municipio" = 13,
         "entidad" =14,
         "zona" = 15) %>%
  mutate(fecha = as.Date(fecha, format = "%Y-%m-%d")) %>%
  select(fecha,nom_estac, id_station, value, longitud, latitud, municipio, entidad, zona)


head(pm)
unique(pm$zona)
class(pm)
view(pm)
str(pm)
summary(pm)
