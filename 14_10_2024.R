library(dplyr)
library(tidyverse)
library(rjson)
library(tidyjson)



consumo_alcohol <- fromJSON(file = "INPUT/DATA/consumo_de_alcohol.json")

head(consumo_alcohol)

consumo_alcohol_tipos<-consumo_alcohol %>% 
  gather_object %>% 
  json_types


consumo_alcohol_tiposresum<-consumo_alcohol %>%  
  gather_object %>% 
  json_types %>% 
  count(name, type)
view(consumo_alcohol_mejor)
view(consumo_alcohol_mejorr)


consumo_alcohol<-consumo_alcohol %>%
  enter_object(Data) %>%
  gather_array() %>%
  spread_all() %>%
  select(-document.id, -array.index)
view(consumo_alcohol)

consumo_alcohol<-consumo_alcohol %>%
  enter_object(MetaData) %>%
  gather_array() %>%
  spread_all %>%
  select(-document.id, -array.index)
  
  
view(consumo_alcohol)

#hacer un join para unir tablas


