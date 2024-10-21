library(dplyr)
library(tidyverse)
library(rjson)
library(tidyjson)

suicidio = fromJSON(file = "C:/Users/Usuario/Downloads/Primer_Cuatri_TERCERO/FUENTES DE DATOS BIOMEDICAS Y WEB SEMANTICA/Practicas/VAC_fuentes-de-datos/CONJUNTOSDEDATOS/ConjuntoDatos_SeminarioFuentes.json")

head(suicidio)
View(suicidio)

spread_all(suicidio)

suicidio %>%
  spread_all() %>%
  View()

suicidio <- spread_all(suicidio)
