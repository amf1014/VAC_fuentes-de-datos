library(dplyr)
library(tidyverse)
library(rjson)
library(tidyjson)

suicidio <- fromJSON(file = "INPUT/DATA/ConjuntoDatos_SeminarioFuentes.json")

suicidio

head(suicidio)

suicidio1 <- suicidio %>%
  gather_object %>%
  json_types %>%
  count(name,type)

suicidio2 <- suicidio %>%
  enter_object(Data) %>%
  gather_array %>%
  spread_all %>%
  select(-document.id, -array.index)

View(suicidio2)

suicidio3 <- suicidio %>%
  enter_object(MetaData) %>%
  gather_array %>%
  spread_all %>%
  select(-document.id, -array.index)
  
suicidio %>%
  enter_object(Nombre) %>%
  gather_object %>%
  spread_all
