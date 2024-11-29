library(dplyr)
library(tidyverse)
library(rjson)
library(tidyjson)


consumo_alcohol <- fromJSON(file = "INPUT/DATA/consumo_de_alcohol.json")

head(consumo_alcohol)

consumo_alcohol_tiposresum<-consumo_alcohol %>%  
  gather_object %>% 
  json_types %>% 
  count(name, type)
consumo_alcohol_tiposresum

#consumo_alcohol_tiposresum$name[3]

consumo_alcohol1<-consumo_alcohol %>%
  enter_object(Data) %>%
  gather_array %>%
  spread_all %>%
  select("document.id", "Valor")

consumo_alcohol1

consumo_alcohol2<-consumo_alcohol %>%
  enter_object(MetaData) %>%
  gather_array %>%
  spread_all %>%
  select("document.id", "Nombre")
  
consumo_alcohol2

consumo_alcohol3 <- consumo_alcohol1 %>%
  select(c("document.id","Valor")) %>%
  full_join(x = .,
            y = consumo_alcohol2 %>%
              select(c("document.id","Nombre")),
            by = c("document.id"))


consumo_alcohol4 <- consumo_alcohol3 %>%
  mutate(
    Sexo = case_when(
      Nombre == "Ambos sexos" ~ "Ambos sexos",
      Nombre == "Hombres" ~ "Hombres",
      Nombre == "Mujeres" ~ "Mujeres",
      TRUE ~ as.character(NA)
    )
  )
consumo_alcohol5 <- consumo_alcohol4 %>%
  mutate(
    Comunidades_autonomas = case_when(
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
    )
  )

consumo_alcohol6 <- consumo_alcohol5 %>%
  mutate(
    Consumido = case_when(
      Nombre == "TOTAL" ~ "Total_personas_encuestadas",
      Nombre == "Sí ha consumido" ~ "Si_ha_consumido",
      Nombre == "No ha consumido" ~ "No_ha_consumido",
      Nombre == "No consta" ~ "No_consta",
      TRUE ~ as.character(NA)
    )
  )

consumo_alcohol7 <- 
  as.data.frame(apply(consumo_alcohol6, 2, function(col) col[!is.na(col)])) 



consumo_alcohol7$Valor <- consumo_alcohol7$Valor[seq(1, length(consumo_alcohol7$Valor), by = 3 )]



#Hago porcentaje de las personas que si han consumido con el total de personas encuestadas.

consumo_alcohol8<-consumo_alcohol7%>%
  pivot_wider(names_from = Consumido, values_from = Valor)

#view(consumo_alcohol8)

consumo_alcohol8 <- 
  as.data.frame(apply(consumo_alcohol8, 2, function(col) col[!is.na(col)])) 

consumo_alcohol8 <-consumo_alcohol8%>%
  mutate(
    `Si_ha_consumido` = as.numeric(`Si_ha_consumido`),
    `Total_personas_encuestadas` = as.numeric(`Total_personas_encuestadas`),
    `No_ha_consumido` = as.numeric(`No_ha_consumido`)
  )



consumo_alcohol9 <- consumo_alcohol8 %>%
  mutate(
    Porcentaje_consumo = (`Si_ha_consumido`/`Total_personas_encuestadas`)*100
  )

#Hago porcentaje de las personas que si han consumido con el total de personas encuestadas.

consumo_alcohol10 <- consumo_alcohol9 %>%
  mutate(
    Porcentaje_no_consumo = (`No_ha_consumido`/`Total_personas_encuestadas`)*100
  )


consumo_alcohol10 <- consumo_alcohol10[, -2]#Borra columna Nombre
#view(consumo_alcohol10)



#DATOS PARA GRÁFICAS(mirar)

consumo_por_sexo <- consumo_alcohol9 %>%
  group_by(Sexo) %>%
  summarize(consumo_medio_sexo = mean(Porcentaje_consumo, na.rm = TRUE)) 

#view(consumo_por_sexo)

consumo_por_comunidad <- consumo_alcohol9 %>%
  group_by(Comunidades_autonomas) %>%
  summarize(consumo_medio_comunidad = mean(Porcentaje_consumo, na.rm = TRUE))

#view(consumo_por_comunidad)

consumo_por_sexo_comunidad <- consumo_alcohol9 %>%
  group_by(Sexo, Comunidades_autonomas) %>%
  summarize(consumo_medio_sexo_comunidad = mean(Porcentaje_consumo, na.rm = TRUE))

#view(consumo_por_sexo_comunidad)


ggplot(consumo_por_sexo, aes(x = Sexo, y = consumo_medio_sexo, fill = Sexo)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Consumo Medio de Alcohol por Sexo", x = "Sexo", y = "Consumo Medio (%)") +
  theme_minimal()


ggplot(consumo_por_comunidad, aes(x = reorder(Comunidades_autonomas, consumo_medio_comunidad), y = consumo_medio_comunidad, fill = Comunidades_autonomas)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  labs(title = "Consumo Medio de Alcohol por Comunidad Autónoma", x = "Comunidad Autónoma", y = "Consumo Medio (unidades)") +theme_minimal() +theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(consumo_por_sexo_comunidad, aes(x = reorder(Comunidades_autonomas, consumo_medio_sexo_comunidad), y = consumo_medio_sexo_comunidad, fill = Sexo)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  labs(title = "Consumo Medio de Alcohol por Comunidad Autónoma y Sexo", x = "Comunidad Autónoma", y = "Consumo Medio (unidades)") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#COMPARACIÓN CONSUMO DE ALCOHOL CON EL EJERCICIO FÍSICO POR COMUNIDAD AUTÓNÓNOMA(variable no importante)

 

comparacion_datos <- full_join(x=consumo_alcohol8,y= ejercicioFisicoUnion, 
                               by = c("Comunidades_autonomas", "Sexo"))

comparacion_datos



# Promedio de ejercicio físico y consumo de alcohol por comunidad autónoma y sexo
resumen_ejercicio_alcohol <- comparacion_datos %>%
  group_by(Comunidades_autonomas, Sexo) %>%
  summarize(
    promedio_ejercicio = mean(Miles_de_personas, na.rm = TRUE), 
    promedio_personas_alcohol = mean(as.numeric(Valor), na.rm = TRUE) 
    
  )

resumen_ejercicio_alcohol




  
  




