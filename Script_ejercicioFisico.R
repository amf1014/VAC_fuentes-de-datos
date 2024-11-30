#Script de datos: número de días por semana de ejercicio físico durante el tiempo de ocio según sexo y comunidad autónoma. Población de 15 y más años

library(dplyr)
library(tidyverse)
library(rjson)
library(tidyjson)

#DOTPLOT
install.packages("plotly")
library(plotly)

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

#Comparación de frecuencias de ejercicio por comunidad ¿cuál puede ser la causa?
#EN HOMBRES
ejercicioHombres <- ejercicioFisicoUnion%>%
  filter(Sexo=="Hombres")%>%
  select(-Ratio, -Miles_de_personas, -Sexo)

ejercicioHombres <- ejercicioHombres%>%
  filter(Comunidades_autonomas!="Total Nacional")%>%
  filter(Frecuencia_de_ejercicio!="TOTAL")

ejercicioHombres

#Gráfico Hombres

graficoHombres <- ggplot(ejercicioHombres, aes(Comunidades_autonomas, Porcentaje, fill=Frecuencia_de_ejercicio))+
  geom_bar(stat="identity", position = position_dodge())+
  labs(title = "Ejercicio físico hombres ", subtitle = "Porcentaje de frecuencia de ejercicio físico por comunidades autónomas",x = "Comunidad Autónoma",y = "Porcentaje de frecuencia de ejercicio", fill = "Frecuencia de ejercicio")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

graficoHombres

#EN MUJERES
ejercicioMujeres <- ejercicioFisicoUnion%>%
  filter(Sexo=="Mujeres")%>%
  select(-Ratio, -Miles_de_personas, Sexo)

ejercicioMujeres <- ejercicioMujeres%>%
  filter(Comunidades_autonomas!="Total Nacional")%>%
  filter(Frecuencia_de_ejercicio!="TOTAL")

ejercicioMujeres

#Gráfico Mujeres

graficoMujeres <- ggplot(ejercicioMujeres, aes(Comunidades_autonomas, Porcentaje, fill=Frecuencia_de_ejercicio))+
  geom_bar(stat="identity", position = position_dodge())+
  labs(title = "Ejercicio físico mujeres", subtitle = "Porcentaje de frecuencia de ejercicio físico por comunidades autónomas",x = "Comunidad Autónoma",y = "Porcentaje de frecuencia de ejercicio", fill = "Frecuencia de ejercicio")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

graficoMujeres

#Comparación de los extremos de realización de ejercicio físico
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

#Con esto se genera una columna que el ratio de personas que realizan nada de ejercicio físico frente a las personas que realizan diariamente
ComparacionNadaYMaxEjercicio <- frecuenciaNadaYMaxEjercicioComunidad%>%
  spread(`Frecuencia_de_ejercicio`, Ratio)%>%
  fill(`7 días a la semana`,`Ninguno`, .direction = "up")%>%
  filter(NadaMax==`7 días a la semana`)%>%
  mutate(
    `Comparacion nada y máximo ejercicio por comunidad y sexo` = `Ninguno` - `7 días a la semana`
  )%>%
  select(-NadaMax)

#Con esto se genera una columna que muestra el porcentaje de personas que realizan nada de ejercicio físico frente a las personas que realizan diariamente
ComparacionNadaYMaxEjercicio <- ComparacionNadaYMaxEjercicio%>%
  mutate(
    `Porcentaje nada y máximo ejercicio` = ComparacionNadaYMaxEjercicio$`Comparacion nada y máximo ejercicio por comunidad y sexo`*100)

ComparacionNadaYMaxEjercicio

#Eliminación de la fila Total Nacional y todas las de ambos sexos
ComparacionNadaYMaxEjercicio <- ComparacionNadaYMaxEjercicio %>%
  filter(Comunidades_autonomas!="Total Nacional")%>%
  filter(Sexo!="Ambos sexos")

ggplot(ComparacionNadaYMaxEjercicio, aes(Comunidades_autonomas, `Porcentaje nada y máximo ejercicio`, fill=Sexo))+
  geom_bar(stat="identity", position = position_dodge())+
  labs(title = "Comparación ejercicio físico por sexo y comunidad", subtitle = "Porcentaje de personas que realizan nada de ejercicio en comparación con los que realizan diariamente",x = "Comunidad Autónoma",y = "Porcentaje nada frente a máximo ejercicio")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


##Diferencias entre los valores extremos de realización de ejercicio físico comparado con el sexo

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
    ))
  
ExtremosUnion <- ExtremosUnion %>%
  pivot_longer(cols = starts_with("Ejercicio_fisico"),
    values_to = "FrecuenciasExtremo")%>%
  select(-`name`)
  
ExtremosUnionFinal <- ExtremosUnion%>%
  filter(`Frecuencia_Ejercicio` == `FrecuenciasExtremo`) %>%
  distinct()%>%
  select(-`Frecuencia_Ejercicio`)

ExtremosUnionFinal <- ExtremosUnionFinal%>%
  mutate(
    Porcentaje = Valor*100
  )%>%
  select(-`Valor`)



RepresentacionExtremosEjercicio <- ggplot(ExtremosUnionFinal, aes(Comunidades_autonomas,Porcentaje))+
  geom_point(aes(colour=factor(Sexo), shape = factor(FrecuenciasExtremo)))+
  geom_smooth(method = "lm", se=TRUE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
RepresentacionExtremosEjercicio

ExtremosUnionFinal

#DOTPLOT  para comparar el ejercicio físico maximo y mínimo realizado por hombres y mujeres de forma general
#[URL]{https://plotly.com/r/getting-started/}

dotPlot_maxyminEjercicio <- plot_ly(ExtremosUnionFinal, x = ~Porcentaje, y = ~Sexo, color = ~FrecuenciasExtremo, type = "box") %>%
  layout(
    title = "Distribución de Ejercicio por Sexo y Frecuencia"
  )

dotPlot_maxyminEjercicio

ggplot(ExtremosUnionFinal, aes(Comunidades_autonomas,Porcentaje,fill=Sexo))+
  geom_bar(stat="identity", position = position_dodge())+
  labs(title = "Ejercicio físico extremos por Sexo y Comunidad Autónoma",x = "Comunidad Autónoma",y = "Porcentaje de individuos ejercicio físico")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

