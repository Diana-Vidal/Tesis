#### ANALISIS DESCRIPTIVO DE DEFUNCIONES POR COVID-19 ####

# Cargamos librerias
library(data.table)
library(tidyverse)

# Cargamos la base de defunciones
def_zmvm <- fread("Bases/def_zmvm.csv", encoding = "Latin-1")

view(def_zmvm)
head(def_zmvm)

# Valores unicos de Municipio
unique(def_zmvm$MUNICIPIO_RES)

# Valores unicos de Zona
unique(def_zmvm$ZONA)

#### ESTADISTICOS DESCRIPTIVOS ####

summary(def_zmvm)

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

##### GRAFICOS ####

# Defunciones por zona geografica en ZMVM
def_zmvm %>% 
  ggplot()+
  geom_bar(aes(x = ZONA, y = ..count.., fill = ZONA), position = "dodge")

# Defunciones por zona geografica en ZMVM por entidad
def_zmvm %>% 
  ggplot()+
  geom_bar(aes(x = ZONA, y = ..count.., fill = ZONA), position = "dodge")+
  facet_wrap(~ENTIDAD_RES, scales = "free")

# Defunciones por zona geografica y sexo
def_zmvm %>% 
  ggplot()+
  geom_bar(aes(x = SEXO, y = ..count.., fill = SEXO), position = "dodge")+
  facet_grid(~ZONA, scales = "free")

# Defunciones por región, por sexo y grupo de edad
def_zmvm %>%
  ggplot()+
  geom_bar(aes(y = GRUPO_EDAD, x = ..count.., fill = SEXO), position = "fill")+
  facet_wrap(~ZONA, scales = "free")

hist(def_zmvm$EDAD)
