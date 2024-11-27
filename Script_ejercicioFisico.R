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
    `Comunidades_autonomas` = case_when(
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
    `Frecuencia_de_ejercicio` = case_when(
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
  fill(Sexo, `Comunidades_autonomas`, `Frecuencia_de_ejercicio`, .direction = "up")

ejercicioFisicoUnion

ejercicioFisicoUnion <- filter(ejercicioFisicoUnion, ejercicioFisicoUnion$Nombre == ejercicioFisicoUnion$Sexo)
  
ejercicioFisicoUnion

ejercicioFisicoUnion <- ejercicioFisicoUnion %>%
  select(-Nombre,-document.id)%>%
  rename("Miles_de_personas"=Valor)

ejercicioFisicoUnion

totalPersonasComunidad <- ejercicioFisicoUnion %>%
  filter(`Frecuencia_de_ejercicio` == "TOTAL") %>%
  select(`Comunidades_autonomas`, `Total_personas_según_la_comunidad` = `Miles_de_personas`)

totalPersonasComunidad
duplicated(totalPersonasComunidad)

totalPersonasComunidad <- totalPersonasComunidad %>%
  distinct(`Comunidades_autonomas`, .keep_all = TRUE)

duplicated(totalPersonasComunidad)

ejercicioFisicoUnion <- left_join(ejercicioFisicoUnion, totalPersonasComunidad, by="Comunidades_autonomas")

ejercicioFisicoUnion <- ejercicioFisicoUnion %>%
  mutate(
    Ratio = `Miles_de_personas` / `Total_personas_según_la_comunidad`,
    Porcentaje = Ratio * 100
  )%>%
  select(-`Total_personas_según_la_comunidad`)

frecuenciaNadaYMaxEjercicioComunidad <- ejercicioFisicoUnion%>%
  mutate(
    NadaMax = case_when(
      `Frecuencia_de_ejercicio` == "Ninguno" ~ `Ratio`,
      `Frecuencia_de_ejercicio` == "7 días a la semana" ~ `Ratio`,
      TRUE ~ as.double(NA)
    ))%>%
  select(`Comunidades_autonomas`,Sexo,`Frecuencia_de_ejercicio`,`Ratio`, NadaMax) %>%
  filter(!is.na(NadaMax))

frecuenciaNadaYMaxEjercicioComunidad

ComparacionNadaYMaxEjercicio <- frecuenciaNadaYMaxEjercicioComunidad%>%
  spread(`Frecuencia_de_ejercicio`, Ratio)%>%
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

#Diferencias entre los valores extremos de realización de ejercicio físico comparado con el sexo

NadaFrenteMaxEjercicioHombres <- ComparacionNadaYMaxEjercicio%>%
  filter(Sexo=="Hombres")%>%
  select(-`Comparacion nada y máximo ejercicio por comunidad y sexo`)

ExtremosEjercicioHombres <- NadaFrenteMaxEjercicioHombres%>%
  pivot_longer(
  cols = c("7 días a la semana", "Ninguno"), 
  names_to = "Ejercicio_fisico",            
  values_to = "Valor"                     
)%>%
  select(-`Porcentaje nada y máximo ejercicio`)

NadaFrenteMaxEjercicioMujeres <- ComparacionNadaYMaxEjercicio%>%
  filter(Sexo=="Mujeres")%>%
  select(-`Comparacion nada y máximo ejercicio por comunidad y sexo`)

ExtremosEjercicioMujeres <- NadaFrenteMaxEjercicioMujeres%>%
  pivot_longer(
    cols = c("7 días a la semana", "Ninguno"), 
    names_to = "Ejercicio_fisico",            
    values_to = "Valor"                     
  )%>%
  select(-`Porcentaje nada y máximo ejercicio`)

ExtremosRealizacionEjercicio <- left_join(ExtremosEjercicioHombres, ExtremosEjercicioMujeres, by=c("Comunidades_autonomas"))
  
ExtremosUnion <- ExtremosRealizacionEjercicio %>%
  pivot_longer(
    cols = starts_with("Valor"),  
    names_to = "Sexo",            
    values_to = "Valor"           
  )%>%
  mutate(
    Sexo = ifelse(Sexo == "Valor.x","Hombres", "Mujeres"))%>%
  select(-`Sexo.x`,-`Sexo.y`)%>%
  distinct(Comunidades_autonomas, Sexo, Valor,.keep_all = TRUE)

ExtremosUnion <- ExtremosUnion%>%
  mutate(
    Frecuencia_Ejercicio = case_when(
      Sexo == "Mujeres" ~ Ejercicio_fisico.y, 
      Sexo == "Hombres" ~ Ejercicio_fisico.x
    ))%>%
  select(-Ejercicio_fisico.x, -Ejercicio_fisico.y)
  
ExtremosUnion <- ExtremosUnion %>%
  pivot_longer(
    cols = starts_with("Ejercicio_fisico"),
    values_to = "FrecuenciasExtremo")%>%
  select(-`name`)
  

ggplot(ExtremosUnion, aes(Comunidades_autonomas,Valor))+geom_point(aes(colour=factor(Sexo), shape = factor(Frecuencia_Ejercicio)))



NadaFrenteMaxEjercicioAmbos <- ComparacionNadaYMaxEjercicio%>%
  filter(Sexo=="Ambos sexos")%>%
  select(-`Comparacion nada y máximo ejercicio por comunidad y sexo`)

NadaFrenteMaxEjercicioAmbos


DiferenciaDeActividadEntreAmbosSexosComunidad <- full_join(NadaFrenteMaxEjercicioMujeres,NadaFrenteMaxEjercicioHombres, by = c("Comunidades_autonomas"))%>%
  rename(`Porcentaje_Mujeres` =`Porcentaje nada y máximo ejercicio.x`, 
         `Porcentaje_Hombres` =`Porcentaje nada y máximo ejercicio.y`, 
         ` Mujeres: 7 días a la semana` =`7 días a la semana.x`, 
         `Hombres: 7 días a la semana` =`7 días a la semana.y`, 
         `Mujeres: Ninguno`= Ninguno.x, `Hombres: Ninguno`= Ninguno.y)%>%
  select(-Sexo.x, -Sexo.y)%>%
  mutate(
    `Porcentaje deferido ningun ejercicio mujeres respecto hombres` = (`Mujeres: Ninguno`- `Hombres: Ninguno`)*100,
    `Porcentaje deferido 7 días a la semana ejercicio mujeres respecto hombres` = (` Mujeres: 7 días a la semana`-`Hombres: 7 días a la semana`)*100)%>%
  select(`Comunidades_autonomas`,`Porcentaje deferido ningun ejercicio mujeres respecto hombres`,`Porcentaje deferido 7 días a la semana ejercicio mujeres respecto hombres`)

