
library(tidyverse)
library(sf)
library(gstat)

cdmx_mun <- 
  st_read("/Users/tex/OneDrive/SHP_Bases/shp_inegi_2020/09_ciudaddemexico/conjunto_de_datos/09mun.shp") %>% 
  st_transform(6372) %>% 
  print()

zona_monit <- 
  st_set_geometry(cdmx_mun, NULL) %>% 
  mutate(zona_monit = c("NO", "SO", "SO", "NE", "CE", "SE", "SO", "SE", 
                        "SO", "SE", "SE", "SE", "CE", "CE", "NO", "CE")) %>% 
  as_tibble() %>% 
  select(CVEGEO, zona_monit) %>% 
  print()

cdmx_mun <- 
  inner_join(cdmx_mun, zona_monit, by = "CVEGEO") %>% 
  print()

cdmx_ageb <- 
  st_read("/Users/tex/OneDrive/SHP_Bases/shp_inegi_2020/09_ciudaddemexico/conjunto_de_datos/09a.shp") %>% 
  st_transform(6372) %>% 
  print()

cdmx_ageb <- 
  cdmx_ageb %>% 
  mutate(CLAVE_GEO = str_c(CVE_ENT, CVE_MUN), .after = CVEGEO) %>% 
  inner_join(zona_monit, by = c("CLAVE_GEO" = "CVEGEO")) %>% 
  print()

ggplot() + 
  geom_sf(data = cdmx_mun, fill = "transparent") + 
  geom_sf(data = cdmx_ageb) + 
  theme_bw()

cdmx_ageb_c <- 
  cdmx_ageb %>% 
  st_centroid() %>% 
  print()

ggplot() + 
  geom_sf(data = cdmx_mun, fill = "transparent") + 
  geom_sf(data = cdmx_ageb) + 
  geom_sf(data = cdmx_ageb_c, size = 0.2) + 
  theme_bw()

sites <- 
  st_read("/Users/tex/OneDrive/SHP_Bases/Shapes_estaciones/sites_sinaica_v3.shp") %>% 
  st_transform(6372) %>% 
  print()

sites <- 
  st_join(sites, cdmx_mun, join = st_within) %>% 
  filter(!is.na(CVEGEO)) %>% 
  select(site, clave, zona_monit) %>% 
  print()

ggplot() + 
  geom_sf(data = cdmx_mun, fill = "transparent") + 
  geom_sf(data = cdmx_ageb) + 
  geom_sf(data = cdmx_ageb_c, size = 0.2) + 
  geom_sf(data = sites, color = "red", size = 1) + 
  theme_bw()

# Sustituir este bloque por tu malla de datos de contaminantesx
fechas <- 
  merge(seq(as.Date("2019-01-01"), as.Date("2019-01-10"), by = 1), 
      sites$site) %>% 
  as_tibble() %>% 
  rename(fecha = x, site = y) %>% 
  mutate(pm25 = runif(200, min = 5.3, 58.4)) %>% 
  arrange(fecha, site) %>% 
  print()

sites_serie <- 
  inner_join(sites, fechas, by = "site", multiple = "all") %>% 
  select(fecha, site, clave, zona_monit, pm25) %>% 
  print()

### Preparar datos para estimación
fechas_estim <- as.character(unique(sites_serie$fecha))

estim <- list()

# Escribir la zona de análisis
zona_estim <- "NO"

for (i in fechas_estim) {

# filtrar fecha/zona
sites_dia <- 
  sites_serie %>% 
  filter(fecha == i & zona_monit == zona_estim) %>% 
  print()

# filtrar ageb/zona
ageb_estim <- 
  cdmx_ageb_c %>% 
  filter(zona_monit == zona_estim) %>% 
  print()

idw_tmp <- 
  gstat(formula = pm25 ~ 1, 
        data = sites_dia, 
        set = list(idp = 2))

estimaciones <- 
  predict(idw_tmp, ageb_estim) %>% 
  cbind(ageb_estim) %>% 
  rename(pm25_estim = var1.pred) %>% 
  select(-var1.var, -geometry.1) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  group_by(CVE_ENT, CVE_MUN) %>%
  summarise(pm25_estim = mean(pm25_estim, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(zona_monit = zona_estim, 
         fecha = as.Date(i)) %>% 
  select(fecha, CVE_ENT, CVE_MUN, zona_monit, pm25_estim)

estim[[i]] <- estimaciones

}

estim_NO <- do.call(rbind, estim) %>% print()

ggplot() + 
  geom_sf(data = cdmx_mun, fill = "transparent") + 
  geom_sf(data = ageb_estim, size = 0.2) + 
  geom_sf(data = sites_dia, color = "red", size = 1) + 
  theme_bw()


