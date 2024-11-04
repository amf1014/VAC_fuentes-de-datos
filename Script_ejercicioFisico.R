#Script de base: número de días por semana de ejercicio físico durante el tiempo de ocio según sexo y comunidad autónoma. Población de 15 y más años

library(dplyr)
library(tidyverse)
library(rjson)
library(tidyjson)

ejercicioFisico = fromJSON(file = "CONJUNTOSDEDATOS/Número de días por semana de ejercicio físico durante el tiempo de ocio.json")

head(ejercicioFisico)

spread_all(ejercicioFisico)

ejercicioFisico %>%
  spread_all()


ejercicioFisico%>%
  gather_object %>%
  json_types

ejercicioFisico %>%
  enter_object(Data) %>%
  gather_array %>%
  spread_all 

ejercicioFisico %>%
  enter_object(MetaData) %>%
  gather_array %>%
  spread_all 

ejercicioFisico %>%
  enter_object(Nombre) %>%
  gather_object %>%
  spread_all

view(ejercicioFisico)
