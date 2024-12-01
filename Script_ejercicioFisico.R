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
      Nombre == "Comunitat Valenciana" ~ "Comunidad Valenciana",
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

ejercicioMinimoUnaVez <- ejercicioFisicoUnion%>%
  rename(sexo = Sexo, comunidades_autonomas=Comunidades_autonomas)%>%
  filter(sexo!="Ambos sexos", Frecuencia_de_ejercicio!='TOTAL', Frecuencia_de_ejercicio!='Ninguno', comunidades_autonomas!='Total Nacional')%>%
  select(-Miles_de_personas,-Porcentaje)

ejercicioNada <- ejercicioFisicoUnion%>%
  rename(sexo = Sexo, comunidades_autonomas=Comunidades_autonomas)%>%
  filter(sexo!="Ambos sexos", Frecuencia_de_ejercicio=='Ninguno', comunidades_autonomas!='Total Nacional')%>%
  select(-Miles_de_personas,-Porcentaje)

#Tabla de ejercicio_por_comunidad para comparar 
realizacion_ejercicio_por_comunidad <- ejercicioMinimoUnaVez %>%
  group_by(comunidades_autonomas) %>%
  summarize(ejercicio_medio_comunidad=sum(as.numeric(Ratio), na.rm = TRUE)/2)

#Tabla de nada_ejercicio_por_comunidad comparar 
nada_ejercicio_por_comunidad <- ejercicioNada %>%
  group_by(comunidades_autonomas) %>%
  summarize(ejercicio_medio_comunidad=sum(as.numeric(Ratio), na.rm = TRUE)/2)

#Tabla de realizacion_ejercicio_por_sexo
realizacion_ejercicio_por_sexo <- ejercicioMinimoUnaVez %>%
  group_by(sexo) %>%
  summarize(ejercicio_medio_sexo=sum(as.numeric(Ratio), na.rm = TRUE)/19)


#Comparación de frecuencias de ejercicio por comunidad ¿cuál puede ser la causa?
#EN HOMBRES
ejercicioHombres <- ejercicioFisicoUnion%>%
  filter(Sexo=="Hombres")%>%
  select(-Ratio, -Miles_de_personas, -Sexo)

ejercicioHombres <- ejercicioHombres%>%
  filter(Frecuencia_de_ejercicio!="TOTAL")

ejercicioHombres

#Gráfico Hombres

graficoHombres <- ggplot(ejercicioHombres, aes(Comunidades_autonomas, Porcentaje, fill=Frecuencia_de_ejercicio))+
  geom_bar(stat="identity", position = position_dodge())+
  labs(title = "Ejercicio físico hombres ", subtitle = "Porcentaje de personas según la frecuencia de ejercicio físico por cada comunidad autónoma",x = "Comunidad Autónoma",y = "Porcentaje de frecuencia de ejercicio", fill = "Frecuencia de ejercicio")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

graficoHombres

#EN MUJERES
ejercicioMujeres <- ejercicioFisicoUnion%>%
  filter(Sexo=="Mujeres")%>%
  select(-Ratio, -Miles_de_personas, Sexo)

ejercicioMujeres <- ejercicioMujeres%>%
  filter(Frecuencia_de_ejercicio!="TOTAL")

ejercicioMujeres

#Gráfico Mujeres

graficoMujeres <- ggplot(ejercicioMujeres, aes(Comunidades_autonomas, Porcentaje, fill=Frecuencia_de_ejercicio))+
  geom_bar(stat="identity", position = position_dodge())+
  labs(title = "Ejercicio físico mujeres", subtitle = "Porcentaje de personas según la frecuencia de ejercicio físico por cada comunidad autónoma",x = "Comunidad Autónoma",y = "Porcentaje de frecuencia de ejercicio", fill = "Frecuencia de ejercicio")+
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


#tabla de ejercicio_por_sexo para comparar con suicidio
NadaYMaxEjercicioSexoSinNacional <- frecuenciaNadaYMaxEjercicioComunidad%>%
  filter(Comunidades_autonomas!="Total Nacional")%>%
  select(-NadaMax)

#Nada de ejercicio físico
NadaEjercicioSexoSinNacional <- NadaYMaxEjercicioSexoSinNacional%>%
  filter(Frecuencia_de_ejercicio=="Ninguno")


ejercicio_min_por_sexo <- NadaEjercicioSexoSinNacional %>%
  group_by(Sexo) %>%
  summarize(Nada_de_ejercicio=sum(as.numeric(Ratio), na.rm = TRUE)/19)

#7 días a la semana ejercicio físico
MaxEjercicioSexoSinNacional <- NadaYMaxEjercicioSexoSinNacional%>%
  filter(Frecuencia_de_ejercicio=="7 días a la semana")


ejercicio_max_por_sexo <- MaxEjercicioSexoSinNacional %>%
  group_by(Sexo) %>%
  summarize(Maximo_ejercicio=mean(as.numeric(Ratio), na.rm = TRUE))



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
  filter(Comunidades_autonomas!="Total Nacional")

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
  labs(title = "Ejercicio físico extremos por Sexo y Comunidad Autónoma",x = "Comunidad Autónoma",y = "Porcentaje de individuos", colour = "Sexo", shape = "Frecuencias de ejercicio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
RepresentacionExtremosEjercicio

#geom_smooth(method = "lm", se=TRUE) +

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







##COMPARACIÓN EJERCICIO FÍSICO CON SUICIDIOS

ejercicio_max_por_sexo <- ejercicio_max_por_sexo%>%
  rename(sexo=Sexo)
ejercicio_min_por_sexo <- ejercicio_min_por_sexo%>%
  rename(sexo=Sexo)
suicidio_por_sexo

ejercicio_fisico_suicidio_sexo <- full_join(ejercicio_max_por_sexo, ejercicio_min_por_sexo, by = "sexo")

ejercicio_fisico_suicidio_sexo <- full_join(ejercicio_fisico_suicidio_sexo,suicidio_por_sexo, by = "sexo")%>%
  filter(sexo!="Ambos sexos")%>%
  mutate(Maximo_ejercicio = Maximo_ejercicio * 100,
         Nada_de_ejercicio = Nada_de_ejercicio * 100,
         suicidio_medio_sexo = suicidio_medio_sexo * 100)

grafico_suicidio_ejer_sexo <- ggplot(ejercicio_fisico_suicidio_sexo) +
  geom_point(aes(x = Maximo_ejercicio, y = suicidio_medio_sexo, color = sexo, shape = "Máximo ejercicio"), size = 3) +
  geom_point(aes(x = Nada_de_ejercicio, y = suicidio_medio_sexo, color = sexo, shape = "Nada de ejercicio"), size = 3) +
  labs(title = "Relación entre la actividad física y el suicidio según el sexo",
       x = "Porcentaje de individuos realizan o no ejercicio físico",
       y = "Porcentaje de suicidio medio",
       color = "Sexo",
       shape = "Tipo de ejercicio") +
  theme_minimal()

grafico_suicidio_ejer_sexo

# Convertir el gráfico a interactivo con ggplotly
grafico_interactivo_suicidio_ejer_sexo <- ggplotly(grafico_suicidio_ejer_sexo)

grafico_interactivo_suicidio_ejer_sexo

#Realización máxima de ejercicio frente a suicidio
ejercicioMax_suicidio_por_sexo <- full_join(ejercicio_max_por_sexo, suicidio_por_sexo, by = "sexo")%>%
  filter(sexo!="Ambos sexos")%>%
  mutate(Maximo_ejercicio = Maximo_ejercicio * 100,
         suicidio_medio_sexo = suicidio_medio_sexo * 100)
  
grafico_max_ejer_suicidio_sexo <-ggplot(ejercicioMax_suicidio_por_sexo, aes(x = Maximo_ejercicio, y = suicidio_medio_sexo, color = sexo)) +
  geom_point(size = 3) +
  labs(title = "Relación la realización de ejercicio diario y el suicidio",
       x = "Porcentaje de individuos medio que realizan ejercicio 7 veces a la semana",
       y = "Porcentaje de suicidio medio",
       color = "Sexo") +
  theme_minimal()

# Convertir el gráfico a interactivo con ggplotly
interactive_graph_max <- ggplotly(grafico_max_ejer_suicidio_sexo)

# Mostrar el gráfico interactivo
interactive_graph_max


#Realización mínima de ejercicio frente a suicidio
ejercicioMin_suicidio_por_sexo <- full_join(ejercicio_min_por_sexo, suicidio_por_sexo, by = "sexo")%>%
  filter(sexo!="Ambos sexos")%>%
  mutate(Nada_de_ejercicio = Nada_de_ejercicio * 100,
         suicidio_medio_sexo = suicidio_medio_sexo * 100)

grafico_minimoejer_suicidio_sexo <- ggplot(ejercicioMin_suicidio_por_sexo, aes(x = Nada_de_ejercicio, y = suicidio_medio_sexo, color = sexo)) +
  geom_point(size = 3) +
  labs(title = "Relación no realización de ejercicio físico y el suicidio",
       x = "Porcentaje de individuos medio que no realiza nada de ejercicio",
       y = "Porcentaje de suicidio medio",
       color = "Sexo") +
  theme_minimal()

# Convertir el gráfico a interactivo con ggplotly
interactive_graph_min <- ggplotly(grafico_minimoejer_suicidio_sexo)

# Mostrar el gráfico interactivo
interactive_graph_min


#Realización de ejercicio físico mínimo una vez por semana frente a suicidio por sexo 

realizacion_ejercicio_suicidio_sexo <- full_join(realizacion_ejercicio_por_sexo, suicidio_por_sexo, by = "sexo")%>%
  filter(sexo!="Ambos sexos")%>%
  mutate(ejercicio_medio_sexo = ejercicio_medio_sexo * 100,
         suicidio_medio_sexo = suicidio_medio_sexo * 100)


ggplot(realizacion_ejercicio_suicidio_sexo, aes(x = ejercicio_medio_sexo, y = suicidio_medio_sexo, color = sexo)) +
  geom_point(size = 5) +
  labs(title = "Relación de realización de ejercicio y el suicidio",
       subtitle = "Se relaciona la realización de ejercicio físico mínimo una vez por semana y el suicidio por sexo",
       x = "Porcentaje de individuos medio que realiza mínimo 1 vez por semana ejercicio físico ",
       y = "Porcentaje de suicidio medio",
       color = "Sexo") +
  theme_minimal()

#Realización de ejercicio físico mínimo una vez por semana frente a suicidio por comunidades 

realizacion_ejercicio_suicidio_por_comunidad <- full_join(realizacion_ejercicio_por_comunidad, suicidio_por_comunidad, by = "comunidades_autonomas")%>%
  filter(comunidades_autonomas!="Total nacional")%>%
  mutate(ejercicio_medio_comunidad = ejercicio_medio_comunidad * 100,
         suicidio_medio_comunidad = suicidio_medio_comunidad * 100)


grafico_comunidad_ejercicio_suicidio <- ggplot(realizacion_ejercicio_suicidio_por_comunidad, aes(x = ejercicio_medio_comunidad , y = suicidio_medio_comunidad, color = comunidades_autonomas)) +
  geom_point(size = 2) +
  labs(title = "Relación de realización de ejercicio y el suicidio",
       subtitle = "Se relaciona la realización de ejercicio físico mínimo una vez por semana y el suicidio por comunidades autónomas",
       x = "Porcentaje de individuos medio que realiza mínimo 1 vez por semana ejercicio físico ",
       y = "Porcentaje de suicidio medio",
       color = "Comunidades/Ciudades autónomas") +
  theme_minimal() +
  geom_smooth(method = "loess", se = TRUE, aes(group = 1))

# Convertir el gráfico a interactivo con ggplotly
grafico_interactivo_comunidad_ejercicio_suicidio <- ggplotly(grafico_comunidad_ejercicio_suicidio)

grafico_interactivo_comunidad_ejercicio_suicidio

#Realización nada ejercicio físico frente a suicidio por comunidades 

nada_ejercicio_suicidio_por_comunidad <- full_join(nada_ejercicio_por_comunidad, suicidio_por_comunidad, by = "comunidades_autonomas")%>%
  filter(comunidades_autonomas!="Total nacional")%>%
  mutate(ejercicio_medio_comunidad = ejercicio_medio_comunidad * 100,
         suicidio_medio_comunidad = suicidio_medio_comunidad * 100)+
  theme(legend.position = "none")


ggplot(nada_ejercicio_suicidio_por_comunidad, aes(x = ejercicio_medio_comunidad , y = suicidio_medio_comunidad, color = comunidades_autonomas)) +
  geom_point(size = 3) +
  labs(title = "Relación de no realización de ejercicio y el suicidio",
       subtitle = "Se relaciona la no realización de ejercicio físico con el suicidio por comunidades autónomas",
       x = "Porcentaje de individuos medio que no realiza ejercicio físico ",
       y = "Porcentaje de suicidio medio",
       color = "Comunidades/Ciudades autónomas") +
  theme_minimal() +
  geom_smooth(method = "loess", se = TRUE, aes(group = 1)) 
















ExtremosUnionFinal
nada_ejercicio_por_comunidad <- nada_ejercicio_por_comunidad%>%
  mutate(comunidades_autonomas = case_when(
    comunidades_autonomas == "Islas Baleares" ~ "Balears, Illes",
    comunidades_autonomas == "Islas Canarias" ~ "Canarias",
    comunidades_autonomas == "Castilla-La Mancha" ~ "Castilla - La Mancha",
    comunidades_autonomas == "Principado de Asturias" ~ "Asturias, Principado de",
    comunidades_autonomas == "Comunidad Valenciana" ~ "Comunitat Valenciana",
    comunidades_autonomas == "Madrid" ~ "Madrid, Comunidad de",
    comunidades_autonomas == "Murcia" ~ "Murcia, Región de",
    comunidades_autonomas == "Navarra" ~ "Navarra, Comunidad Foral de",
    comunidades_autonomas == "La Rioja" ~ "Rioja, La",
    TRUE ~ comunidades_autonomas
  ))

realizacion_ejercicio_por_comunidad <- realizacion_ejercicio_por_comunidad%>%
  mutate(comunidades_autonomas = case_when(
    comunidades_autonomas == "Islas Baleares" ~ "Balears, Illes",
    comunidades_autonomas == "Islas Canarias" ~ "Canarias",
    comunidades_autonomas == "Castilla-La Mancha" ~ "Castilla - La Mancha",
    comunidades_autonomas == "Principado de Asturias" ~ "Asturias, Principado de",
    comunidades_autonomas == "Comunidad Valenciana" ~ "Comunitat Valenciana",
    comunidades_autonomas == "Madrid" ~ "Madrid, Comunidad de",
    comunidades_autonomas == "Murcia" ~ "Murcia, Región de",
    comunidades_autonomas == "Navarra" ~ "Navarra, Comunidad Foral de",
    comunidades_autonomas == "La Rioja" ~ "Rioja, La",
    TRUE ~ comunidades_autonomas
  ))

ejercicioMinimoUnaVez <- ejercicioMinimoUnaVez%>%
  mutate(comunidades_autonomas = case_when(
    comunidades_autonomas == "Islas Baleares" ~ "Balears, Illes",
    comunidades_autonomas == "Islas Canarias" ~ "Canarias",
    comunidades_autonomas == "Castilla-La Mancha" ~ "Castilla - La Mancha",
    comunidades_autonomas == "Principado de Asturias" ~ "Asturias, Principado de",
    comunidades_autonomas == "Comunidad Valenciana" ~ "Comunitat Valenciana",
    comunidades_autonomas == "Madrid" ~ "Madrid, Comunidad de",
    comunidades_autonomas == "Murcia" ~ "Murcia, Región de",
    comunidades_autonomas == "Navarra" ~ "Navarra, Comunidad Foral de",
    comunidades_autonomas == "La Rioja" ~ "Rioja, La",
    TRUE ~ comunidades_autonomas
  ))


frecuenciaNadaYMaxEjercicioComunidad <- frecuenciaNadaYMaxEjercicioComunidad%>%
  mutate(Porcentaje = Ratio * 100) %>%
  select(-Ratio,-NadaMax)%>%
  rename(comunidades_autonomas=Comunidades_autonomas)

frecuenciaNadaYMaxEjercicioComunidad <- frecuenciaNadaYMaxEjercicioComunidad%>%
  mutate(comunidades_autonomas = case_when(
    comunidades_autonomas == "Islas Baleares" ~ "Balears, Illes",
    comunidades_autonomas == "Islas Canarias" ~ "Canarias",
    comunidades_autonomas == "Castilla-La Mancha" ~ "Castilla - La Mancha",
    comunidades_autonomas == "Principado de Asturias" ~ "Asturias, Principado de",
    comunidades_autonomas == "Comunidad Valenciana" ~ "Comunitat Valenciana",
    comunidades_autonomas == "Madrid" ~ "Madrid, Comunidad de",
    comunidades_autonomas == "Murcia" ~ "Murcia, Región de",
    comunidades_autonomas == "Navarra" ~ "Navarra, Comunidad Foral de",
    comunidades_autonomas == "La Rioja" ~ "Rioja, La",
    comunidades_autonomas == "Total Nacional" ~ "Total nacional",
    TRUE ~ comunidades_autonomas
  ))


frecuenciaNadaYMaxEjercicioComunidad <- frecuenciaNadaYMaxEjercicioComunidad%>%
  rename(Comunidades_autonomas=comunidades_autonomas)
  
ejercicioMujeres <- ejercicioMujeres%>%
  mutate(Comunidades_autonomas = case_when(
    Comunidades_autonomas == "Islas Baleares" ~ "Balears, Illes",
    Comunidades_autonomas == "Islas Canarias" ~ "Canarias",
    Comunidades_autonomas == "Castilla-La Mancha" ~ "Castilla - La Mancha",
    Comunidades_autonomas == "Principado de Asturias" ~ "Asturias, Principado de",
    Comunidades_autonomas == "Comunidad Valenciana" ~ "Comunitat Valenciana",
    Comunidades_autonomas == "Madrid" ~ "Madrid, Comunidad de",
    Comunidades_autonomas == "Murcia" ~ "Murcia, Región de",
    Comunidades_autonomas == "Navarra" ~ "Navarra, Comunidad Foral de",
    Comunidades_autonomas == "La Rioja" ~ "Rioja, La",
    Comunidades_autonomas == "Total Nacional" ~ "Total nacional",
    TRUE ~ Comunidades_autonomas
  ))

ejercicioMujeresNinguno <- ejercicioMujeres%>%
  filter(Frecuencia_de_ejercicio=="Ninguno")%>%
  select(-Frecuencia_de_ejercicio)%>%
  rename(Ninguno = Porcentaje)

ejercicioMujeresEjercicio <- ejercicioMujeres%>%
  filter(Frecuencia_de_ejercicio!="Ninguno")%>%
  group_by(Comunidades_autonomas) %>%
  summarize(Porcentaje=sum(as.numeric(Porcentaje), na.rm = TRUE))
  
  
ejercicioHombres <- ejercicioHombres%>%
  mutate(Comunidades_autonomas = case_when(
    Comunidades_autonomas == "Islas Baleares" ~ "Balears, Illes",
    Comunidades_autonomas == "Islas Canarias" ~ "Canarias",
    Comunidades_autonomas == "Castilla-La Mancha" ~ "Castilla - La Mancha",
    Comunidades_autonomas == "Principado de Asturias" ~ "Asturias, Principado de",
    Comunidades_autonomas == "Comunidad Valenciana" ~ "Comunitat Valenciana",
    Comunidades_autonomas == "Madrid" ~ "Madrid, Comunidad de",
    Comunidades_autonomas == "Murcia" ~ "Murcia, Región de",
    Comunidades_autonomas == "Navarra" ~ "Navarra, Comunidad Foral de",
    Comunidades_autonomas == "La Rioja" ~ "Rioja, La",
    Comunidades_autonomas == "Total Nacional" ~ "Total nacional",
    TRUE ~ Comunidades_autonomas
  ))

ejercicioHombresNinguno <- ejercicioHombres%>%
  filter(Frecuencia_de_ejercicio=="Ninguno")%>%
  select(-Frecuencia_de_ejercicio)%>%
  rename(Ninguno = Porcentaje)

ejercicioHombresEjercicio <- ejercicioHombres%>%
  filter(Frecuencia_de_ejercicio!="Ninguno")%>%
  group_by(Comunidades_autonomas) %>%
  summarize(Porcentaje=sum(as.numeric(Porcentaje), na.rm = TRUE))


ejercicioAmbosSexos <- ejercicioFisicoUnion%>%
  filter(Sexo=="Ambos sexos")%>%
  select(-Ratio, -Miles_de_personas, Sexo)

ejercicioAmbosSexos <- ejercicioAmbosSexos%>%
  filter(Frecuencia_de_ejercicio!="TOTAL")

ejercicioAmbosSexos <- ejercicioAmbosSexos%>%
  mutate(Comunidades_autonomas = case_when(
    Comunidades_autonomas == "Islas Baleares" ~ "Balears, Illes",
    Comunidades_autonomas == "Islas Canarias" ~ "Canarias",
    Comunidades_autonomas == "Castilla-La Mancha" ~ "Castilla - La Mancha",
    Comunidades_autonomas == "Principado de Asturias" ~ "Asturias, Principado de",
    Comunidades_autonomas == "Comunidad Valenciana" ~ "Comunitat Valenciana",
    Comunidades_autonomas == "Madrid" ~ "Madrid, Comunidad de",
    Comunidades_autonomas == "Murcia" ~ "Murcia, Región de",
    Comunidades_autonomas == "Navarra" ~ "Navarra, Comunidad Foral de",
    Comunidades_autonomas == "La Rioja" ~ "Rioja, La",
    Comunidades_autonomas == "Total Nacional" ~ "Total nacional",
    TRUE ~ Comunidades_autonomas
  ))


ejercicioAmbosSexosNinguno <- ejercicioAmbosSexos%>%
  filter(Frecuencia_de_ejercicio=="Ninguno")%>%
  select(-Frecuencia_de_ejercicio)%>%
  rename(Ninguno = Porcentaje)

ejercicioAmbosSexosEjercicio <- ejercicioAmbosSexos%>%
  filter(Frecuencia_de_ejercicio!="Ninguno")%>%
  group_by(Comunidades_autonomas) %>%
  summarize(Porcentaje=sum(as.numeric(Porcentaje), na.rm = TRUE))










##MAPAS


porcentaje_nada_ejercicio_por_comunidad <- nada_ejercicio_por_comunidad %>%
  mutate(porcentaje_ejercicio_medio_comunidad = ejercicio_medio_comunidad*100)%>%
  select(-ejercicio_medio_comunidad)

porcentaje_realizacion_ejercicio_por_comunidad <- realizacion_ejercicio_por_comunidad %>%
  mutate(porcentaje_ejercicio_medio_comunidad = ejercicio_medio_comunidad*100)%>%
  select(-ejercicio_medio_comunidad)

porcentaje_realizacion_ejercicio_sexo_comunidad <- ejercicioMinimoUnaVez%>%
  group_by(comunidades_autonomas,sexo) %>%
  summarize(ejercicio_medio_comunidad=sum(as.numeric(Ratio), na.rm = TRUE))%>%
  mutate(porcentaje_ejercicio_medio_comunidad = ejercicio_medio_comunidad*100)%>%
  select(-ejercicio_medio_comunidad)

mujeres_ejercicio_comunidad <- porcentaje_realizacion_ejercicio_sexo_comunidad %>%
  filter(sexo=="Mujeres")

hombres_ejercicio_comunidad <- porcentaje_realizacion_ejercicio_sexo_comunidad %>%
  filter(sexo=="Hombres")

porcentaje_realizacion_nada_ejercicio_sexo_comunidad <- ejercicioNada%>%
  group_by(comunidades_autonomas,sexo) %>%
  summarize(ejercicio_medio_comunidad=sum(as.numeric(Ratio), na.rm = TRUE))

mujeres_nada_ejercicio_comunidad <- porcentaje_realizacion_nada_ejercicio_sexo_comunidad %>%
  filter(sexo=="Mujeres")

hombres_nada_ejercicio_comunidad <- porcentaje_realizacion_nada_ejercicio_sexo_comunidad %>%
  filter(sexo=="Hombres")


levels(factor(ccaa_sf$ine.ccaa.name))
levels(factor(porcentaje_nada_ejercicio_por_comunidad$comunidades_autonomas))

ccaa_nada <- esp_get_ccaa() %>%
  left_join(porcentaje_nada_ejercicio_por_comunidad, by = c("ine.ccaa.name" = "comunidades_autonomas"))

ccaa_ejercicio <- esp_get_ccaa() %>%
  left_join(porcentaje_realizacion_ejercicio_por_comunidad, by = c("ine.ccaa.name" = "comunidades_autonomas"))



levels(factor(ccaa_sf$ine.ccaa.name))
levels(factor(mujeres_ejercicio_comunidad$comunidades_autonomas))

ccaa_m_ejercicio <- esp_get_ccaa() %>%
  left_join(mujeres_ejercicio_comunidad, by = c("ine.ccaa.name" = "comunidades_autonomas"))

levels(factor(ccaa_sf$ine.ccaa.name))
levels(factor(hombres_ejercicio_comunidad$comunidades_autonomas))

ccaa_h_ejercicio <- esp_get_ccaa() %>%
  left_join(hombres_ejercicio_comunidad, by = c("ine.ccaa.name" = "comunidades_autonomas"))



mapa_nada_ejercicio_global <- ggplot(ccaa_nada) +
  geom_sf(aes(fill = porcentaje_ejercicio_medio_comunidad), color = "grey70", linewidth = .3) +
  geom_sf(data = can, color = "grey70") +
  geom_sf_label(aes(label = round(porcentaje_ejercicio_medio_comunidad, 4)),
                fill = "white", alpha = 0.5,
                size = 3, label.size = 0
  ) +
  scale_fill_gradientn(
    colors = hcl.colors(10, "Purples", rev = TRUE),
    n.breaks = 10, labels = scales::label_number(suffix = "%"),
    guide = guide_legend(title = "Porcentaje Global Nada Ejercicio", position = "inside")
  ) +
  theme_void() +
  theme(legend.position = c(0.1, 0.6))

mapa_nada_ejercicio_global

mapa_ejercicio_medio_global <- ggplot(ccaa_ejercicio) +
  geom_sf(aes(fill = porcentaje_ejercicio_medio_comunidad), color = "grey70", linewidth = .3) +
  geom_sf(data = can, color = "grey70") +
  geom_sf_label(aes(label = round(porcentaje_ejercicio_medio_comunidad, 4)),
                fill = "white", alpha = 0.5,
                size = 3, label.size = 0
  ) +
  scale_fill_gradientn(
    colors = hcl.colors(10, "Greens", rev = TRUE),
    n.breaks = 10, labels = scales::label_number(suffix = "%"),
    guide = guide_legend(title = "Porcentaje Global Ejercicio Medio", position = "inside")
  ) +
  theme_void() +
  theme(legend.position = c(0.1, 0.6))

mapa_ejercicio_medio_global

comparacion_mapas_nada_vs_ejercicio <- mapa_nada_ejercicio_global + mapa_ejercicio_medio_global



mapa_ejecicio_mujeres <- ggplot(ccaa_m_ejercicio) +
  geom_sf(aes(fill = porcentaje_ejercicio_medio_comunidad), color = "grey70", linewidth = .3) +
  geom_sf(data = can, color = "grey70") +
  geom_sf_label(aes(label = round(porcentaje_ejercicio_medio_comunidad, 4)),
                fill = "white", alpha = 0.5,
                size = 3, label.size = 0
  ) +
  scale_fill_gradientn(
    colors = hcl.colors(10, "Reds", rev = TRUE),
    n.breaks = 10, labels = scales::label_number(suffix = "%"),
    guide = guide_legend(title = "Porcentaje Mujeres Ejercicio", position = "inside")
  ) +
  theme_void() +
  theme(legend.position = c(0.1, 0.6))


mapa_ejecicio_hombres <- ggplot(ccaa_h_ejercicio) +
  geom_sf(aes(fill = porcentaje_ejercicio_medio_comunidad), color = "grey70", linewidth = .3) +
  geom_sf(data = can, color = "grey70") +
  geom_sf_label(aes(label = round(porcentaje_ejercicio_medio_comunidad, 4)),
                fill = "white", alpha = 0.5,
                size = 3, label.size = 0
  ) +
  scale_fill_gradientn(
    colors = hcl.colors(10, "Blues", rev = TRUE),
    n.breaks = 10, labels = scales::label_number(suffix = "%"),
    guide = guide_legend(title = "Porcentaje Hombres Ejercicio", position = "inside")
  ) +
  theme_void() +
  theme(legend.position = c(0.1, 0.6))

mapas_ejercicio_por_sexos <- mapa_ejecicio_mujeres + mapa_ejecicio_hombres

mapas_ejercicio_por_sexos



#Guardo los gráficos


ggsave(
  filename = "Ejercicio_físico_hombres_por_comunidad.jpeg",
  plot = graficoHombres,
  path = "OUTPUT/Figures", 
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Ejercicio_físico_mujeres_por_comunidad.jpeg",
  plot = graficoMujeres,
  path = "OUTPUT/Figures", 
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Ejercicio_físico_mujeres_por_comunidad.jpeg",
  plot = graficoMujeres,
  path = "OUTPUT/Figures", 
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Extremos_ejercicio_por_comunidad_y_sexo.jpeg",
  plot = RepresentacionExtremosEjercicio ,
  path = "OUTPUT/Figures", 
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)


ggsave(
  filename = "Mapa_nada_ejercicio_vs_mínimo_un_día_ejercicio.jpeg",
  plot =  comparacion_mapas_nada_vs_ejercicio,
  path = "OUTPUT/Figures", 
  scale = 0.5,
  width = 110,
  height = 50,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Mapa_mínimo_un_día_ejercicio_por_sexo_y_comunidad.jpeg",
  plot =  mapas_ejercicio_por_sexos,
  path = "OUTPUT/Figures", 
  scale = 0.5,
  width = 110,
  height = 50,
  units = "cm",
  dpi = 320
)


