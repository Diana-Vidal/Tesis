#### REGRESION DE POISSON ####

# LISTA DE MUNICIPIOS DENTRO DEL AREA DE INFLUENCIA DE 5 KM

municipios <- c("ATENCO", "TEXCOCO", "CHICONCUAC", "ECATEPEC DE MORELOS", "COACALCO DE BERRIOZABAL",
                "TLALNEPANTLA DE BAZ", "TULTITLAN", "GUSTAVO A. MADERO", "CUAUTITLAN IZCALLI",
                "NICOLAS ROMERO", "ATIZAPAN DE ZARAGOZA", "NAUCALPAN DE JUAREZ", "AZCAPOTZALCO",
                "CUAUHTEMOC", "NEZAHUALCOYOTL", "VENUSTIANO CARRANZA", "IZTACALCO", "IZTAPALAPA",
                "BENITO JUAREZ", "ALVARO OBREGON", "COYOACAN", "HUIXQUILUCAN", "CUAJIMALPA DE MORELOS",
                "LA MAGDALENA CONTRERAS", "TLALPAN", "XOCHIMILCO", "TLAHUAC", "MILPA ALTA") 


#### POR MUNICIPIO ####

# Union de bases de pm y meteo para regresion
regresion_mun <- merge(prom_pm_mun, prom_meteo_mun, all = T)

# Separamos variables de parametro
# Union con base de defunciones
regres_mun <- regresion_mun %>%
  # spread(., key = parametro, value = value)
  pivot_wider(.,names_from = parametro, values_from = value) %>% 
  left_join(., def_mun) %>% 
  filter(municipio%in%municipios)

# Guardamos como csv
write.csv(regres_mun, "Bases/regres_mun.csv",
          row.names = F,
          fileEncoding = "ISO-8859-1")

# Cargamos base
regres_mun <- fread("Bases/regres_mun.csv",
            encoding = "Latin-1")

#### POR ZONA ####

# Union de bases de pm y meteo para regresion
regresion_zona <- merge(prom_pm_zona, prom_meteo_zona, all = T)

# Separamos variables de parametro
# Union con base de defunciones
regres_zona <- regresion_zona %>%
  # spread(., key = parametro, value = value)
  pivot_wider(.,names_from = parametro, values_from = value) %>% 
  left_join(., def_zona)

# Guardamos como csv
write.csv(regres_zona, "Bases/regres_zona.csv",
          row.names = F,
          fileEncoding = "ISO-8859-1")

# Cargamos base
regres_zona <- fread("Bases/regres_zona.csv",
                    encoding = "Latin-1")





           