

install.packages("devtools")

devtools::install_github("AllanZamb/DatosAbiertosCOVID19")

pacman::p_load(tidyverse, aweek)


Datos_Abiertos <- DatosAbiertosCOVID19::DatosCOVID19(POBLACIONES = "muni")
#Defunciones
a<-Datos_Abiertos %>%
  filter(CLASIFICACION_FINAL %in% c("POSITIVO A SARS-COV-2",
                                    "CONFIRMADO POR ASOCIACION CLINICA",
                                    "CONFIRMADO POR DICTAMINACION"),
         ENTIDAD_RES %in% c("CIUDAD DE MEXICO", "MEXICO"),
         !is.na(FECHA_DEF),
         FECHA_DEF >= "2020-03-01" & FECHA_DEF <= "2021-12-31") %>%
  merge(read.csv("https://raw.githubusercontent.com/AllanZamb/DatosAbiertosCOVID19/main/poblaciones/Zonas.csv", encoding = "UTF-8")) %>%
  drop_na(ZONA) %>%
  group_by(FECHA_DEF, ENTIDAD_RES, MUNICIPIO_RES,ZONA, POB_MUNI_RES) %>%
  summarise(Defunciones = n() ) %>%
  mutate(SE = sprintf("%02d", epiweek(FECHA_DEF)),
         Anio = epiyear(FECHA_DEF),
         FechaSE = week2date(paste0(Anio,"-","W",SE),"Sunday"))

write.csv(a, "def_zonas.csv", row.names = F)




#Zonas geográficas
Datos_Abiertos %>%
  filter(CLASIFICACION_FINAL %in% c("POSITIVO A SARS-COV-2",
                                    "CONFIRMADO POR ASOCIACION CLINICA",
                                    "CONFIRMADO POR DICTAMINACION"),
         ENTIDAD_RES %in% c("CIUDAD DE MEXICO", "MEXICO"),
         !is.na(FECHA_DEF),
         FECHA_DEF >= "2020-03-01" & FECHA_DEF <= "2021-12-31") %>%
  merge(zonas) %>%
  drop_na(ZONA) %>%

  group_by(FECHA_DEF, ENTIDAD_RES, MUNICIPIO_RES,ZONA, POB_MUNI_RES) %>%
  summarise(Defunciones = n() ) %>%
  #mutate(Mortalidad = (Defunciones/POB_MUNI_RES)*100000) %>%
  group_by(ZONA) %>%
  summarise(Defunciones = sum(Defunciones, na.rm = T) ) %>%
  ggplot()+
  geom_bar(aes(x = reorder(ZONA, -Defunciones), y= Defunciones, fill = ZONA  ), stat = "identity")+
  labs(x = "Áreas geográficas de la ZMVM", y = "Defunciones")+
  theme_minimal()+
  theme(legend.position = "none")




#Porcentajes
Datos_Abiertos %>%
  filter(CLASIFICACION_FINAL %in% c("POSITIVO A SARS-COV-2",
                                    "CONFIRMADO POR ASOCIACION CLINICA",
                                    "CONFIRMADO POR DICTAMINACION"),
         ENTIDAD_RES %in% c("CIUDAD DE MEXICO", "MEXICO"),
         !is.na(FECHA_DEF),
         FECHA_DEF >= "2020-03-01" & FECHA_DEF <= "2021-12-31") %>%
  merge(zonas) %>%
  drop_na(ZONA) %>%
  group_by(FECHA_DEF, ENTIDAD_RES, MUNICIPIO_RES,ZONA, POB_MUNI_RES) %>%
  summarise(Defunciones = n() ) %>%
  group_by(ZONA) %>%
  summarise(Defunciones = sum(Defunciones, na.rm = T) ) %>%
  mutate(perc = Defunciones/sum(Defunciones)*100 )  %>%
  arrange(desc(ZONA)) %>%
  mutate(lab.ypos = cumsum(perc) - 0.5*perc) %>%
  ggplot( aes(x = "", y = perc, fill = ZONA)) +
  geom_bar(width = 1, stat = "identity", color = "white")+
  coord_polar("y",start = 0) +
  blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = lab.ypos,
                label = percent(perc/100)), size =5, color = "white")+
  labs(title = "Áreas geográficas de la ZMVM", fill = "Zona")


