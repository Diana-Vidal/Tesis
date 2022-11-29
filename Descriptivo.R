#### ANALISIS DESCRIPTIVO DE DEFUNCIONES ####

table(def_zmvm$SEXO)
prop.table(table(def_zmvm$SEXO))

def_zmvm %>%
  mutate(GRUPO_EDAD = factor(GRUPO_EDAD, levels = c("0 - 4",
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
                                                    "65 o más"))) %>% 
  ggplot()+
  geom_bar(aes(y = GRUPO_EDAD, x = ..count.., fill = SEXO), position = "fill")+
  facet_wrap(~ZONA, scales = "free")

#####

mort_mun <- def_zmvm %>%
  group_by(ENTIDAD_RES, MUNICIPIO_RES, AÑO,POB_MUN, POB_ENT, ZONA) %>% 
  summarise(NUM_DEF=n()) %>% 
  mutate(TASA_MORTALIDAD = (NUM_DEF/POB_MUN)*100000,
         TASA_AJUSTADA = (NUM_DEF/POB_ENT)*1000000)


MX <- getData("GADM", country = "MX", level = 2)

MX@data$NAME_1[which(MX@data$NAME_1%in% c("Distrito Federal"))]<- "Ciudad de México"
MX@data$NAME_1 <- toupper(MX@data$NAME_1)
MX@data$NAME_1 <- stri_trans_general(MX@data$NAME_1, "Latin-ASCII")
MX@data$NAME_2 <- toupper(MX@data$NAME_2)
MX@data$NAME_2 <- stri_trans_general(MX@data$NAME_2, "Latin-ASCII")

MX@data <- MX@data %>% 
  filter(NAME_1 %in% c("CIUDAD DE MEXICO", "MEXICO"))

MX@data <- left_join(MX@data, mort_mun,
                     by = c("NAME_1" = "ENTIDAD_RES"
                                               ,"NAME_2"= "MUNICIPIO_RES")) %>% 
  filter(!is.na(ZONA))


#Valores numéricos
color_num <- colorNumeric(palette = "RdBu",
                          domain = mort_mun$TASA_AJUSTADA, reverse = T)


leaflet() %>%
  addProviderTiles(provider = "CartoDB.PositronNoLabels") %>%
  addPolygons(data = MX@data,
              fillColor  = ~color_num(mort_mun$TASA_AJUSTADA),
              color = "black",
              weight = 1,
              label = paste0(MX$NAME_2, ": ", round(mort_mun$TASA_AJUSTADA,0) ),
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5) %>%
  addLegend(data = MX, position = "topright", pal = color_num, values = ~mort_mun$TASA_AJUSTADA,
            bins = seq(0,400, by =50), #Aqui modificar el valor mínimo al maximo por x
            title = "Mortalidad por COVID-19 durante 2020")
