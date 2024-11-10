#Script de datos: número de días por semana de ejercicio físico durante el tiempo de ocio según sexo y comunidad autónoma. Población de 15 y más años

library(dplyr)
library(tidyverse)
library(rjson)
library(tidyjson)

ejercicioFisico <- fromJSON(file = "INPUT/DATA/Ejercicio_fisico.json")

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

#Con la línea siguiente busco interpretar como se guardan los datos.
nombresEjercicioFisico

#Estructurar los datos
ejercicioFisicoData <- ejercicioFisico %>%
  enter_object(Data) %>%
  gather_array %>%
  spread_all%>%
  select(-array.index)

ejercicioFisicoMetaData <-ejercicioFisico %>%
  enter_object(MetaData) %>%
  gather_array %>%
  spread_all %>%
  select(document.id, Nombre)

ejercicioFisicoData
ejercicioFisicoMetaData

ejercicioFisicoUnion <- left_join(ejercicioFisicoMetaData, ejercicioFisicoData, by="document.id")
ejercicioFisicoUnion

ejercicioFisicoUnion <- ejercicioFisicoUnion %>%
  mutate(
    Sexo = case_when(
      Nombre == "Ambos sexos" ~ "Ambos sexos",
      Nombre == "Hombres" ~ "Hombres",
      Nombre == "Mujeres" ~ "Mujeres",
      TRUE ~ as.character(NA)
    ),
    `Comunidades autonomas` = case_when(
      Nombre == "Total" ~ "Total Nacional",
      Nombre == "Andalucía" ~ "Andalucía",
      Nombre == "Aragón" ~ "Aragón",
      Nombre == "Asturias (Principado de)" ~ "Principado de Asturias",
      Nombre == "Balears (Illes)" ~ "Islas Baleares",
      Nombre == "Canarias" ~ "Islas Canarias",
      Nombre == "Cantabria" ~ "Cantabria",
      Nombre == "Castilla y León" ~ "Castilla y León",
      Nombre == "Castilla-La Mancha" ~ "Castilla-La Mancha",
      Nombre == "Cataluña" ~ "Cataluña",
      Nombre == "Comunitat Valenciana" ~ "Comunitat Valenciana",
      Nombre == "Extremadura" ~ "Extremadura",
      Nombre == "Galicia" ~ "Galicia",
      Nombre == "Madrid (Comunidad de)" ~ "Madrid",
      Nombre == "Murcia (Región de)" ~ "Murcia",
      Nombre == "Navarra (Comunidad Foral de)" ~ "Navarra",
      Nombre == "País Vasco" ~ "País Vasco",
      Nombre == "Rioja (La)" ~ "La Rioja",
      Nombre == "Ceuta (Ciudad Autónoma de)" ~ "Ceuta",
      Nombre == "Melilla (Ciudad Autónoma de)" ~ "Melilla",
      TRUE ~ as.character(NA)
    ),
    `Frecuencia de ejercicio` = case_when(
      Nombre == "TOTAL" ~ "TOTAL",
      Nombre == "Ninguno" ~ "Ninguno",
      Nombre == "1 o 2 días a la semana" ~ "1 o 2 días a la semana",
      Nombre == "3 o 4 días a la semana" ~ "3 o 4 días a la semana",
      Nombre == "5 o 6 días a la semana" ~ "5 o 6 días a la semana",
      Nombre == "7 días a la semana" ~ "7 días a la semana",
      TRUE ~ as.character(NA)
    )
  )

ejercicioFisicoUnion

ejercicioFisicoUnion <- ejercicioFisicoUnion %>%
  fill(Sexo, `Comunidades autonomas`, `Frecuencia de ejercicio`, .direction = "up")

ejercicioFisicoUnion

ejercicioFisicoUnion <- filter(ejercicioFisicoUnion, ejercicioFisicoUnion$Nombre == ejercicioFisicoUnion$Sexo)
  
ejercicioFisicoUnion

ejercicioFisicoUnion <- ejercicioFisicoUnion %>%
  select(-Nombre,-document.id)%>%
  rename("Miles de personas"=Valor)

ejercicioFisicoUnion

totalPersonasComunidad <- ejercicioFisicoUnion %>%
  filter(`Frecuencia de ejercicio` == "TOTAL") %>%
  select(`Comunidades autonomas`, `Total personas según la comunidad` = `Miles de personas`)

totalPersonasComunidad
duplicated(totalPersonasComunidad)

totalPersonasComunidad <- totalPersonasComunidad %>%
  distinct(`Comunidades autonomas`, .keep_all = TRUE)

duplicated(totalPersonasComunidad)

ejercicioFisicoUnion <- left_join(ejercicioFisicoUnion, totalPersonasComunidad, by="Comunidades autonomas")

ejercicioFisicoUnion <- ejercicioFisicoUnion %>%
  mutate(
    Ratio = `Miles de personas` / `Total personas según la comunidad`,
    Porcentaje = Ratio * 100
  )%>%
  select(-`Total personas según la comunidad`)

frecuenciaNadaYMaxEjercicioComunidad <- ejercicioFisicoUnion%>%
  mutate(
    NadaMax = case_when(
      `Frecuencia de ejercicio` == "Ninguno" ~ `Ratio`,
      `Frecuencia de ejercicio` == "7 días a la semana" ~ `Ratio`,
      TRUE ~ as.double(NA)
    ))%>%
  select(`Comunidades autonomas`,Sexo,`Frecuencia de ejercicio`,`Ratio`, NadaMax) %>%
  filter(!is.na(NadaMax))

frecuenciaNadaYMaxEjercicioComunidad

ComparacionNadaYMaxEjercicio <- frecuenciaNadaYMaxEjercicioComunidad%>%
  spread(`Frecuencia de ejercicio`, Ratio)%>%
  fill(`7 días a la semana`,`Ninguno`, .direction = "up")%>%
  filter(NadaMax==`7 días a la semana`)%>%
  mutate(
    `Comparacion nada y máximo ejercicio por comunidad y sexo` = `Ninguno` - `7 días a la semana`
  )%>%
  select(-NadaMax)

ComparacionNadaYMaxEjercicio <- ComparacionNadaYMaxEjercicio%>%
  mutate(
    `Porcentaje nada y máximo ejercicio` = ComparacionNadaYMaxEjercicio$`Comparacion nada y máximo ejercicio por comunidad y sexo`*100)

ComparacionNadaYMaxEjercicio

NadaFrenteMaxEjercicioHombres <- ComparacionNadaYMaxEjercicio%>%
  filter(Sexo=="Hombres")%>%
  select(-`Comparacion nada y máximo ejercicio por comunidad y sexo`)

NadaFrenteMaxEjercicioHombres

NadaFrenteMaxEjercicioMujeres <- ComparacionNadaYMaxEjercicio%>%
  filter(Sexo=="Mujeres")%>%
  select(-`Comparacion nada y máximo ejercicio por comunidad y sexo`)

NadaFrenteMaxEjercicioMujeres

NadaFrenteMaxEjercicioAmbos <- ComparacionNadaYMaxEjercicio%>%
  filter(Sexo=="Ambos sexos")%>%
  select(-`Comparacion nada y máximo ejercicio por comunidad y sexo`)

NadaFrenteMaxEjercicioAmbos


DiferenciaDeActividadEntreAmbosSexosComunidad <- full_join(NadaFrenteMaxEjercicioMujeres,NadaFrenteMaxEjercicioHombres, by = c("Comunidades autonomas"))%>%
  rename(`Porcentaje Mujeres` =`Porcentaje nada y máximo ejercicio.x`, `Porcentaje Hombres` =`Porcentaje nada y máximo ejercicio.y`, ` Mujeres: 7 días a la semana` =`7 días a la semana.x`, `Hombres: 7 días a la semana` =`7 días a la semana.y`, `Mujeres: Ninguno`= Ninguno.x, `Hombres: Ninguno`= Ninguno.y)%>%
  select(-Sexo.x, -Sexo.y)%>%
  mutate(
    `Porcentaje deferido ningun ejercicio mujeres respecto hombres` = (`Mujeres: Ninguno`- `Hombres: Ninguno`)*100,
    `Porcentaje deferido 7 días a la semana ejercicio mujeres respecto hombres` = (` Mujeres: 7 días a la semana`-`Hombres: 7 días a la semana`)*100)%>%
  select(`Comunidades autonomas`,`Porcentaje deferido ningun ejercicio mujeres respecto hombres`,`Porcentaje deferido 7 días a la semana ejercicio mujeres respecto hombres`)

