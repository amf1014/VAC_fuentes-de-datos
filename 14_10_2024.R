library(dplyr)
library(tidyverse)
library(rjson)
library(tidyjson)



consumo_alcohol <- fromJSON(file = "CONJUNTOSDEDATOS/consumo_de_alcohol.json")

#view(consumo_alcohol)


head(consumo_alcohol)
spread_all(consumo_alcohol)


consumo_alcohol_mejor<-consumo_alcohol %>% 
  spread_all() %>% 
  gather_object %>% 
  json_types

consumo_alcohol_<-consumo_alcohol %>% 
  spread_all() %>% 
  gather_object %>% 
  json_types %>% 
  count(name, type)
view(consumo_alcohol_mejor)
view(consumo_alcohol_)


consumo_alcohol %>%
  enter_object(Data) %>%
  gather_array() %>%
  spread_all() 
#view(consumo_alcohol)
#hacer un join para unir tablas




