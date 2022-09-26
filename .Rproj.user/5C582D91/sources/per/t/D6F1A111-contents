
variables <- c("ID_REGISTRO", "ENTIDAD_UM", "FECHA_DEF", "CLASIFICACION_FINAL")

COVID2020_ <- fread("Bases/Datos abiertos/COVID19MEXICO2020.csv", select = variables) %>% 
  mutate(FECHA_DEF = as.Date(FECHA_DEF,format = "%Y-%m-%d")) %>%
  filter(!is.na(FECHA_DEF),
         CLASIFICACION_FINAL %in% c(1,2,3)) %>% 
  group_by(ENTIDAD_UM) %>% 
  summarise(NUM_DEF = n())

sum(COVID2020_$NUM_DEF)+sum(COVID2021$NUM_DEF)


COVID2021_ <- fread("Bases/Datos abiertos/COVID19MEXICO2021.csv", select = variables) %>% 
  mutate(FECHA_DEF = as.Date(FECHA_DEF,format = "%Y-%m-%d")) %>%
  filter(!is.na(FECHA_DEF),
         CLASIFICACION_FINAL %in% c(1,2,3)) %>% 
  filter(FECHA_DEF <= "2021-12-31") %>% 
  group_by(ENTIDAD_UM) %>% 
  summarise(NUM_DEF = n())

mortalidad2020_ <- left_join(COVID2020_, CONAPO2020, by = c("ENTIDAD_UM" = "CLAVE_ENT")) %>% 
  mutate(tasa_mortalidad = (NUM_DEF/POB)*10000)

mortalidad2021_ <- left_join(COVID2021_, CONAPO2021, by = c("ENTIDAD_UM" = "CLAVE_ENT")) %>% 
  mutate(tasa_mortalidad = (NUM_DEF/POB)*10000)

COVIDmerge_ <- rbind(COVID2020_, COVID2021_) %>% 
  group_by(ENTIDAD_UM) %>% 
  summarise(NUM_DEF = sum(NUM_DEF)) %>%
  left_join(.,CONAPO2021, by = c("ENTIDAD_UM" = "CLAVE_ENT")) %>% 
  mutate(tasa_mortalidad = (NUM_DEF/POB)*10000)
