library(dplyr)
library(tidyverse)
library(rjson)
library(tidyjson)



consumo_alcohol <- fromJSON(file = "C:/Users/USUARIO/Desktop/1_PRACTICA_FUENTES/VAC_fuentes-de-datos/CONJUNTOSDEDATOS/consumo_de_alcohol.json")

view(consumo_alcohol)


head(consumo_alcohol)
spread_all(consumo_alcohol)

consumo_alcohol %>%
  spread_all() %>% 
  View()

consumo_alcohol <- spread_all(consumo_alcohol)

consumo_alcohol %>% 
  spread_all() %>% 
  gather_object %>% 
  json_types %>% 
  count(name, type)

consumo_alcohol %>%
  enter_object(Data) %>%
  gather_array %>%
  spread_all %>%
  #hacer un join para unir tablas
view(consumo_alcohol)

consumo_alcohol %>%
  enter_object(MetaData) %>%
  gather_array %>%
  spread_all %>%
  #hacer un join para unir las tablas 
view(consumo_alcohol)


