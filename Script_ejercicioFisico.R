#Script de base: número de días por semana de ejercicio físico durante el tiempo de ocio según sexo y comunidad autónoma. Población de 15 y más años

library(dplyr)
library(tidyverse)
library(rjson)
library(tidyjson)

ejercicioFisico <- fromJSON(file = "INPUT/DATA/Número de días por semana de ejercicio físico durante el tiempo de ocio.json")

ejercicioFisico

tiposEjercicioFisico_visiongeneral<-ejercicioFisico%>% 
  gather_object %>% 
  json_types

tiposEjercicioFisico_visiongeneral

tiposEjercicioFisico <- ejercicioFisico %>%
  gather_object %>% 
  json_types %>% 
  count(name, type)

tiposEjercicioFisico

nombresEjercicioFisico <- ejercicioFisico %>%
  enter_object(Nombre)

nombresEjercicioFisico

ejercicioFisicoData <- ejercicioFisico %>%
  enter_object(Data) %>%
  gather_array %>%
  spread_all

ejercicioFisicoMetaData <-ejercicioFisico %>%
  enter_object(MetaData) %>%
  gather_array %>%
  spread_all

nombresEjercicioFisico
ejercicioFisicoData
ejercicioFisicoMetaData

tablaUnion <- full_join(nombresEjercicioFisico, ejercicioFisicoData, by="document.id")
tablaUnion

ejercicioFisicoEstructurado <- left_join(ejercicioFisicoMetaData, tablaUnion, by="document.id")
ejercicioFisicoEstructurado

mutate(SEXO, ratio = for(elemento in ejercicioFisicoMetaData$T3_Variable) {if(elemento=="sexo"){} } )