library(dplyr)
library(data.table)
library(GGally)
library(PerformanceAnalytics)


regres_zona <- fread("C:\\Users\\alan.garcia\\Downloads\\regres_zona.csv") 



#Correlación individual
regres_zona %>%
  mutate(num_def = ifelse(is.na(num_def), 0, num_def)) %>% 
  filter(zona == "NORESTE") %>% 
  select("PM2.5", "RH", "TMP", "WSP", "num_def") %>% 
  
  mutate(num_def_7d = zoo::rollmean(num_def, k = 7, fill = NA, align = "right"),
         PM2.5_7d = zoo::rollmean(PM2.5, k = 7, fill = NA, align = "right"),
         RH_7d = zoo::rollmean(RH, k = 7, fill = NA, align = "right"),
         TMP_7d = zoo::rollmean(TMP, k = 7, fill = NA, align = "right"),
         WSP_7d = zoo::rollmean(WSP, k = 7, fill = NA, align = "right")) %>%  

  chart.Correlation(., histogram=TRUE, pch=19)



#Multicorrelación
regres_zona %>%
  mutate(fecha = as.Date(fecha),
         num_def = ifelse(is.na(num_def), 0, num_def)) %>% 
  #filter(zona == "CENTRO") %>% 
  select("zona","fecha","PM2.5", "RH", "TMP", "WSP", "num_def") %>% 
  mutate(num_def_7d = zoo::rollmean(num_def, k = 7, fill = NA, align = "right"),
         PM2.5_7d = zoo::rollmean(PM2.5, k = 7, fill = NA, align = "right"),
         RH_7d = zoo::rollmean(RH, k = 7, fill = NA, align = "right"),
         TMP_7d = zoo::rollmean(TMP, k = 7, fill = NA, align = "right"),
         WSP_7d = zoo::rollmean(WSP, k = 7, fill = NA, align = "right")) %>% 
  filter(fecha >= "2020-03-25") %>% 
  select(zona, num_def_7d, PM2.5_7d,    RH_7d,   TMP_7d, WSP_7d) %>% 
  ggpairs(., columns = 2:6, aes(color = zona),
          lower = list(continuous = "smooth"))
  
  

#Pendiente---
regres_mun <- fread("C:\\Users\\alan.garcia\\Downloads\\regres_mun.csv") 


regres_zona_NE <- regres_zona %>% 
  filter(zona == "NORESTE")%>% 
  filter(fecha >= "2020-01-01" & fecha <= "2021-12-31")

cor(x, y, use="everything",
    method=c("pearson", "kendall", "spearman"))

cor(x = regres_zona_NE$num_def, y = regres_zona_NE$PM2.5, method = "spearman")

with(regres_zona, plot(x= num_def, y= PM2.5, pch=20, col='blue',
                 xlab='Defunciones por COVID-19', las=1,
                 ylab='Concentraciones de PM2.5'))

sum(is.na(regres_zona$PM2.5))


 