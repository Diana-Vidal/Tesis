#### REGRESION DE POISSON ####

# 1. Cargamos base
#### Reemplazamos NAs con ceros
#### Filtramos las fechas desde dic de 2019 hasta dic de 2021
regres_zona <- fread("Bases/regres_zona.csv",
                     encoding = "Latin-1") %>% 
  mutate_if(., is.numeric, ~replace(., is.na(.), 0)) %>% 
  filter(fecha >= "2020-03-01" & fecha <= "2021-12-31")
         # zona == "NORESTE")

# 2. Revisamos si hay sobredispersion de los datos con la prueba de Cameron y Trivedi
#### Cargamos libreria overdisp
library(overdisp)

overdisp(regres_zona, dependent.position = 7, predictor.position = 3:6)
#### En esta prueba se tiene que aceptar la H0 para que no haya sobredispersion
#### En este caso se rechaza la H0 por lo que si hay sobredispersion de los datos
#### Esto significa que se debe de utilizar un modelo de REGRESION BINOMIAL NEGATIVO o
#### una regresion QUASIPOISSON

# 3. Revisamos la variable de respuesta = num. de defunciones con un histograma
ggplot(regres_zona, aes(num_def)) +
  geom_histogram(binwith = 5, position = "dodge") +
  xlab("Numero de defunciones por COVID-19") +
  theme_bw()
#### Se ven datos inflados con ceros
### Esto significa que se debe de realizar un ajuste del modelo


