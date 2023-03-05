#### SERIE DE TIEMPO PARA PM 2.5 ####

## Cargamos librerias

pacman::p_load(rio,          # File import
               here,         # File locator
               tidyverse,    # data management + ggplot2 graphics
               tsibble,      # handle time series datasets
               slider,       # for calculating moving averages
               imputeTS,     # for filling in missing values
               feasts,       # for time series decomposition and autocorrelation
               forecast,     # fit sin and cosin terms to data (note: must load after feasts)
               trending,     # fit and assess models 
               tmaptools,    # for getting geocoordinates (lon/lat) based on place names
               ecmwfr,       # for interacting with copernicus sateliate CDS API
               stars,        # for reading in .nc (climate data) files
               units,        # for defining units of measurement (climate data)
               yardstick,    # for looking at model accuracy
               surveillance  # for aberration detection
)

install.packages("vctrs", repos = "https://cran.us.r-project.org")

library(vctrs)
library(slider)

library(zoo)

library(astsa)
library(tseries)


## Cargamos base de pm2.5
pm <- fread("Bases/pm.csv",
            encoding = "Latin-1") %>% 
  filter(!is.na(zona))


## Creamos la variable semanaepi empezando por domingo
## Cambiamos semanaepi a formato de fecha
## Agrupamos por semana epi y fecha
## Sacamos promedios por dia
pm_st <- pm %>% 
  mutate(semanaepi = yearweek(fecha, week_start = 7)) %>% 
  group_by(semanaepi) %>% 
  summarise(value = mean(value))


## Convertimos el data frame en un objeto de serie de tiempo
pm_st <- tsibble(pm_st, index = semanaepi)

## Lo revisamos
class(pm_st)

## Creamos un grafico para ver la serie de tiempo
ggplot(pm_st, aes(x = semanaepi, y = value))+
  geom_line()

## Medias moviles
pm_mm <- pm_st %>% 
  mutate(media_movil = rollmean(value, k = 4, fill = NA))

## Visualizamos con un grafico
ggplot(pm_mm, aes(x  = semanaepi))+
  geom_line(aes(y = value))+
  geom_line(aes(y = media_movil), colour = "red")

##Descomposicion
 pm_st %>% 
   model(classical_decomposition(value, type = "additive")) %>% 
   components() %>% 
   autoplot()

## Autocorrelacion
pm_st %>% 
  ACF(value, lag_max = 52) %>% 
  autoplot()

## prueba de independencia
Box.test(pm_st$value, type = "Ljung-Box") 



##### SERIE DE TIEMPO ####

# Creamos data frame para serie de tiempo
pm.st <- pm_st[,-1]

# Conciertimos en objeto de serie de tiempo y
# Le asignamos las fechas de inicio y final, por semana (por eso 52)
pm.ts = ts(pm.st, start = c(2019,1), end = c(2021,52), frequency = 52)

# La abrimos
pm.ts

# La vemos en una grafica
plot(pm.ts)

# Creamos boxplot de la serie de tiempo
boxplot(pm.ts~cycle(pm.ts))

cycle(pm.ts)

# Las series de tiempo pueden ser de modelo aditivo o multiplicativo
# El modelo aditivo sirve para estimar una regresion

# Para modelo aditivo = varianza constante
modeloaditivo = decompose(pm.ts)
plot(modeloaditivo)

# Para modelo multiplicativo = varianza incrementa
modelomultiplicativo = decompose(pm.ts, type = "mult")
plot(modelomultiplicativo)

#Para estimar la tendencia
Tendencia = modelomultiplicativo$trend
print(Tendencia)

#Para estimar la Estacionalidad
Estacionalidad = modelomultiplicativo$seasonal
print(Estacionalidad)

# Es importante tener tendencia y estacionalidad para incorporarlos al modelo de regresion
# Si es un modelo multiplicativo se tienen que usar logaritmos o diferencias para
# convertirlo en modelo aditivo y poder estimar modelo de regresion

ts.plot(cbind(Tendencia, Tendencia*Estacionalidad), lty = 1:2)


### ESTO ES PARA MODELOS ARIMA

# La convertimos en logaritmo para convertirla en estacionaria
serielog =log(pm.ts)
serielog
plot(serielog)
adf.test(serielog, alternative = "stationary")

# Como no funcionó calculamos las diferencias para convertirla en estacionaria
seriedif = diff(pm.ts)
seriedif
plot(seriedif)
adf.test(seriedif, alternative = "stationary")
