#### ANALISIS DESCRIPTIVO DE DEFUNCIONES POR COVID-19 ####

# Cargamos librerias
library(data.table)
library(tidyverse)
library(tables)
library(ggplot2)


# Cargamos la base de defunciones
def_zmvm <- fread("Bases/def_zmvm.csv",
                  encoding = "Latin-1")

# Valores unicos de Municipio
unique(def_zmvm$MUNICIPIO_RES)

# Valores unicos de Zona
unique(def_zmvm$ZONA)

#### ESTADISTICOS DESCRIPTIVOS ####

summary(def_zmvm)

hist(def_zmvm$GRUPO_EDAD)

## Cualitativas categoricas
table(def_zmvm$SEXO)
prop.table(table(def_zmvm$SEXO))

table(def_zmvm$GRUPO_EDAD)
prop.table(table(def_zmvm$GRUPO_EDAD))

table(def_zmvm$DIABETES)
prop.table(table(def_zmvm$DIABETES))

table(def_zmvm$EPOC)
prop.table(table(def_zmvm$EPOC))

table(def_zmvm$ASMA)
prop.table(table(def_zmvm$ASMA))

table(def_zmvm$HIPERTENSION)
prop.table(table(def_zmvm$HIPERTENSION))

table(def_zmvm$CARDIOVASCULAR)
prop.table(table(def_zmvm$CARDIOVASCULAR))

table(def_zmvm$OBESIDAD)
prop.table(table(def_zmvm$OBESIDAD))

table(def_zmvm$RENAL_CRONICA)
prop.table(table(def_zmvm$RENAL_CRONICA))

## Continuas
sd(def_zmvm$EDAD)

#### GRAFICOS ####

# Creamos factor para grupo de edad
GRUPO_EDAD_ord <- c("0 - 4",
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
                    "65 o más")

# Defunciones por grupo de edad
def_zmvm %>% 
  ggplot()+
  geom_bar(aes(x = factor(GRUPO_EDAD, levels = GRUPO_EDAD_ord), y = ..count..), position = "dodge")+
  labs (title = "Gráfico n. Defunciones por COVID-19 en la ZMVM durante el 2020 y 2021.",
        caption = "Creación propia con datos obtenidos de base de datos abiertos del Sistema de Vigilancia de Enfermedades Respiratorias.",
        x = "Grupo de edad",
        y = "Número de defunciones") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption.position = "panel",
        plot.caption = element_text(hjust = 0.5))

# Defunciones por zona geografica en ZMVM
def_zmvm %>% 
  ggplot()+
  geom_bar(aes(x = ZONA, y = ..count.., fill = ZONA), position = "dodge")+
  labs (title = "Gráfico n. Defunciones por COVID-19 por zona gegráfica de la ZMVM durante el 2020 y 2021.",
        caption = "Creación propia con datos obtenidos de base de datos abiertos del Sistema de Vigilancia de Enfermedades Respiratorias.",
        x = "Zona geográfica",
        y = "Número de defunciones")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption.position = "panel",
        plot.caption = element_text(hjust = 0.5),
        legend.position = "none")

# # Defunciones por zona geografica en ZMVM por entidad
# def_zmvm %>% 
#   ggplot()+
#   geom_bar(aes(x = ZONA, y = ..count.., fill = ZONA), position = "dodge")+
#   facet_wrap(~ENTIDAD_RES, scales = "free")

# Defunciones por zona geografica y sexo
def_zmvm %>% 
  ggplot()+
  geom_bar(aes(x = SEXO, y = ..count.., fill = SEXO), position = "dodge")+
  facet_grid(~ZONA, scales = "free") +
  labs (title = "Gráfico n. Defunciones por COVID-19 por sexo y zona gegráfica de la ZMVM, durante el 2020 y 2021.",
        caption = "Creación propia con datos obtenidos de base de datos abiertos del Sistema de Vigilancia de Enfermedades Respiratorias.",
        x = "Sexo",
        y = "Número de defunciones")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption.position = "panel",
        plot.caption = element_text(hjust = 0.5),
        legend.position = "none")

# Defunciones por región, por sexo y grupo de edad
def_zmvm %>%
  ggplot()+
  geom_bar(aes(y = factor(GRUPO_EDAD, levels = GRUPO_EDAD_ord), x = ..count.., fill = SEXO), position = "dodge")+
  facet_wrap(~ZONA, scales = "free") +
  xlim(0,7500) +
  labs (title = "Gráfico n. Defunciones por COVID-19 por sexo, grupo de edad y zona gegráfica de la ZMVM, durante el 2020 y 2021.",
        caption = "Creación propia con datos obtenidos de base de datos abiertos del Sistema de Vigilancia de Enfermedades Respiratorias.",
        x = "Número de defunciones",
        y = "Grupo de edad")  +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption.position = "panel",
        plot.caption = element_text(hjust = 0.5))

# Defunciones por grupo de edad y comorbilidad y zona
def_zmvm %>% 
  select(SEXO, GRUPO_EDAD, ZONA, DIABETES, EPOC, ASMA, HIPERTENSION, CARDIOVASCULAR, OBESIDAD, RENAL_CRONICA) %>% 
  gather(., COMORBILIDAD, COM_VALOR, 4:10) %>%
  filter(COM_VALOR == "SI") %>% 
  group_by(ZONA, GRUPO_EDAD, COMORBILIDAD) %>% 
  summarise(DEF = n()) %>% 
  ggplot() +
  geom_bar(aes(x = DEF, y = factor(GRUPO_EDAD, levels = GRUPO_EDAD_ord), fill = COMORBILIDAD), position = "fill", stat = "identity") +
  facet_wrap(~ZONA, scales = "free") +
  scale_x_continuous(labels = scales::percent) +
  labs (title = "Gráfico n. Proporción de defunciones por COVID-19 por grupo de edad, comorbilidad y zona gegráfica de la ZMVM, durante el 2020 y 2021.",
        caption = "Creación propia con datos obtenidos de base de datos abiertos del Sistema de Vigilancia de Enfermedades Respiratorias.",
        x = "Proporción de defunciones",
        y = "Grupo de edad")  +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption.position = "panel",
        plot.caption = element_text(hjust = 0.5))
  

  

