#### ANALISIS DESCRIPTIVO DE CONCENTRACIONES DE PM2.5 EN ZMVM ####

library(data.table)
library(tidyverse)
library(dint)
library(scales)

# Cargamos base de pm
# Eliminamos valores nulos de zona (municipios que no pertenecen a ZMVM)
pm <- fread("Bases/pm.csv",
            encoding = "Latin-1") %>% 
  filter(!is.na(zona))

# Valores unicos de estacion
unique(pm$nom_estac)

# Valores unicos de municipio
unique(pm$municipio)

# Valores unicos de zona
unique(pm$zona)

head(pm)
class(pm$fecha)
str(pm)

#### CONCENTRACIONES POR ESTACION ####

# Estadisticos descriptivos
summary(pm)

table(pm$nom_estac)

table(pm$municipio)

table(pm$zona)

# Desviacion estandar de concentraciones
sd(pm$value)

hist(pm$value)

# Grafico de lineas de concentraciones 2019, 2020 y 2021
pm %>% 
  ggplot()+
  geom_line(aes(x = fecha, y = value))+
  geom_smooth(aes(x = fecha, y = value))


# Grafico de lineas de concentraciones durante 2019
pm %>%
  filter(fecha >= "2019-01-01" & fecha <= "2019-12-31") %>%
  ggplot()+
  geom_line(aes(x = fecha, y = value))+
  geom_smooth(aes(x = fecha, y = value))

# Grafico de lineas de concentraciones durante 2020
pm %>%
  filter(fecha >= "2020-01-01" & fecha <= "2020-12-31") %>%
  ggplot()+
  geom_line(aes(x = fecha, y = value))+
  geom_smooth(aes(x = fecha, y = value))

# Grafico de lineas de concentraciones durante 2021
pm %>%
  filter(fecha >= "2021-01-01" & fecha <= "2021-12-31") %>%
  ggplot()+
  geom_line(aes(x = fecha, y = value))+
  geom_smooth(aes(x = fecha, y = value))

# Boxplot de concentraciones por estacion en 2019
pm %>% 
  filter(fecha >= "2019-01-01" & fecha <= "2019-12-31") %>% 
  ggplot() +
  geom_boxplot(aes(x = cve_estac, y = value, fill = cve_estac)) +
  labs (title = "Concentraciones promedio en 24 h de PM2.5 por estación de la ZMVM durante 2019",
        caption = "Creacion propia con datos abiertos del SIMAT",
        x = "Estaciones",
        y = "Concentracion promedio en 24 horas")
+
  facet_grid(zona~.)

# Boxplot de concentraciones por estacion en 2020
pm %>% 
  filter(fecha >= "2020-01-01" & fecha <= "2020-12-31") %>% 
  ggplot() +
  geom_boxplot(aes(x = cve_estac, y = value, fill = cve_estac)) +
  labs (title = "Concentraciones promedio en 24 h de PM2.5 por estación de la ZMVM durante 2020",
        caption = "Creacion propia con datos abiertos del SIMAT",
        x = "Estaciones",
        y = "Concentracion promedio en 24 horas")

# Boxplot de concentraciones por estacion en 2021
pm %>% 
  filter(fecha >= "2021-01-01" & fecha <= "2021-12-31") %>% 
  ggplot() +
  geom_boxplot(aes(x = cve_estac, y = value, fill = cve_estac)) +
  labs (title = "Concentraciones promedio en 24 h de PM2.5 por estación de la ZMVM durante 2021",
        caption = "Creacion propia con datos abiertos del SIMAT",
        x = "Estaciones",
        y = "Concentracion promedio en 24 horas")

#### CONCENTRACIONES POR ZONA ####

pm_zonas <- pm %>%
  group_by(fecha, zona) %>% 
  summarise(value = mean(value, na.rm = T))

# 2019
pm_zonas_2019 <- pm %>%
  filter(fecha >= "2019-01-01" & fecha <= "2019-12-31") %>%
  group_by(fecha, zona) %>% 
  summarise(value = mean(value, na.rm = T))

summary(pm_zonas_2019)

sd(pm_zonas_2019$value)

# 2020
pm_zonas_2020 <- pm %>%
  filter(fecha >= "2020-01-01" & fecha <= "2020-12-31") %>%
  group_by(fecha, zona) %>% 
  summarise(value = mean(value, na.rm = T))

summary(pm_zonas_2020)

sd(pm_zonas_2020$value)

# 2021
pm_zonas_2021 <- pm %>%
  filter(fecha >= "2021-01-01" & fecha <= "2021-12-31") %>%
  group_by(fecha, zona) %>% 
  summarise(value = mean(value, na.rm = T))

summary(pm_zonas_2021)

sd(pm_zonas_2021$value)


# Boxplot de concentraciones por zona en 2019
pm_zonas %>% 
  filter(fecha >= "2019-01-01" & fecha <= "2019-12-31") %>% 
  ggplot()+
  geom_boxplot(aes(x = zona, y = value, fill = zona))+
  labs (title = "Concentraciones promedio en 24 h de PM2.5 por zona de la ZMVM durante 2019",
        caption = "Creacion propia con datos abiertos del SIMAT",
        x = "Zona",
        y = "Concentracion promedio en 24 horas")

# Boxplot de concentraciones por zona en 2020
pm_zonas %>% 
  filter(fecha >= "2020-01-01" & fecha <= "2020-12-31") %>% 
  ggplot()+
  geom_boxplot(aes(x = zona, y = value, fill = zona))+
  labs (title = "Concentraciones promedio en 24 h de PM2.5 por zona de la ZMVM durante 2020",
        caption = "Creacion propia con datos abiertos del SIMAT",
        x = "Zona",
        y = "Concentracion promedio en 24 horas")

# Boxplot de concentraciones por zona en 2021
pm_zonas %>% 
  filter(fecha >= "2021-01-01" & fecha <= "2021-12-31") %>% 
  ggplot()+
  geom_boxplot(aes(x = zona, y = value, fill = zona))+
  labs (title = "Concentraciones promedio en 24 h de PM2.5 por zona de la ZMVM durante 2021",
        caption = "Creacion propia con datos abiertos del SIMAT",
        x = "Zona",
        y = "Concentracion promedio en 24 horas")

#### CONCENTRACIONES POR MES ####

pm_mes <- pm %>% 
  mutate(dia = weekdays(fecha),
         mes = months(fecha),
         anio = year(fecha))

# 2019
pm_mes_2019 <- pm_mes %>%
  filter(fecha >= "2019-01-01" & fecha <= "2019-12-31") %>%
  group_by(mes) %>% 
  summarise(value = mean(value, na.rm = T))

summary(pm_mes_2019)

sd(pm_mes_2019$value)

# 2020
pm_mes_2020 <- pm_mes %>%
  filter(fecha >= "2020-01-01" & fecha <= "2020-12-31") %>%
  group_by(mes) %>% 
  summarise(value = mean(value, na.rm = T))

summary(pm_mes_2020)

sd(pm_mes_2020$value)

# 2021
pm_mes_2021 <- pm_mes %>%
  filter(fecha >= "2021-01-01" & fecha <= "2021-12-31") %>%
  group_by(mes) %>% 
  summarise(value = mean(value, na.rm = T))

summary(pm_mes_2021)

sd(pm_mes_2021$value)


# mes_orden <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio","agosto",  "septiembre", "octubre",  "noviembre",  "diciembre")
# 
# pm_mes %>%
#   select(mes, cve_estac, value) %>%
#   filter(mes == "enero") %>%
#   group_by(mes, cve_estac) %>% 
#   summarise(value = max(mean(value, na.rm = T))) %>%  View()
#   ggplot()+
#   geom_boxplot(aes(x = factor(mes, levels = mes_orden), y = value, fill = mes))+
#   labs (title = "Concentraciones promedio en 24 h de PM2.5 de la ZMVM por mes durante 2019",
#         caption = "Creacion propia con datos abiertos del SIMAT",
#         x = "Zona", 
#         y = "Concentracion promedio en 24 horas")
  
  
meses_orden <- c("enero", "febrero", "marzo","abril", "mayo", "junio","julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")

# Grafico de barras estaciones con mayor concentraciones por mes
pm %>%
  select(cve_estac, value, fecha ) %>% 
  mutate(fecha = as.Date(fecha)) %>% 
  mutate(mes = months(fecha)) %>% 
  group_by(cve_estac, mes) %>%
  summarize(value = mean(value)) %>%
  group_by(mes) %>%
  slice_max(value, n = 1) %>% 
  ggplot()+
  geom_bar(aes(x = factor(mes, levels = meses_orden), y = value, fill = cve_estac), stat = "identity")

# Boxplot de concentraciones por mes durante 2019
pm %>% 
  mutate(fecha = as.Date(fecha)) %>% 
  mutate(mes = months(fecha),
         ano = year(fecha)) %>%
  filter(ano == "2019") %>% 
  group_by(fecha, cve_estac, mes) %>%
  summarize(value = mean(value)) %>%
  group_by(fecha, cve_estac) %>% 
  slice_max(value, n = 1) %>% 
  ggplot() +
  geom_boxplot(aes(x = factor(mes, levels = meses_orden), y = value, fill = cve_estac))

# Boxplot de concentraciones por mes durante 2020
pm %>% 
  mutate(fecha = as.Date(fecha)) %>% 
  mutate(mes = months(fecha),
         ano = year(fecha)) %>%
  filter(ano == "2020") %>% 
  group_by(fecha, cve_estac, mes) %>%
  summarize(value = mean(value)) %>%
  group_by(fecha, cve_estac) %>% 
  slice_max(value, n = 1) %>% 
  ggplot() +
  geom_boxplot(aes(x = factor(mes, levels = meses_orden), y = value, fill = cve_estac))

# Boxplot de concentraciones por mes durante 2021
pm %>% 
  mutate(fecha = as.Date(fecha)) %>% 
  mutate(mes = months(fecha),
         ano = year(fecha)) %>%
  filter(ano == "2021") %>% 
  group_by(fecha, cve_estac, mes) %>%
  summarize(value = mean(value)) %>%
  group_by(fecha, cve_estac) %>% 
  slice_max(value, n = 1) %>% 
  ggplot() +
  geom_boxplot(aes(x = factor(mes, levels = meses_orden), y = value, fill = cve_estac))






