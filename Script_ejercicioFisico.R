#Script de base: número de días por semana de ejercicio físico durante el tiempo de ocio según sexo y comunidad autónoma. Población de 15 y más años

library(dplyr)
library(tidyverse)
library(rjson)
library(tidyjson)

ejercicioFisico <- fromJSON(file = "INPUT/DATA/Número de días por semana de ejercicio físico durante el tiempo de ocio.json")

ejercicioFisico

ejercicioFisico %>%
  gather_object %>% 
  json_types %>% 
  count(name, type)

ejercicioFisico %>%
  enter_object(Data) %>%
  gather_array %>%
  spread_all%>%
  select(-document.id, -array.index)

ejercicioFisico %>%
  enter_object(MetaData) %>%
  gather_array %>%
  spread_all %>%
  select(-document.id, -array.index)

view(ejercicioFisico)