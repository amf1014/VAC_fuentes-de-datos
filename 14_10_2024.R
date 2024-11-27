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

#view(consumo_alcohol3)

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
      Nombre == "TOTAL" ~ "Total consumido",
      Nombre == "Si ha consumido" ~ "Si ha consumido",
      Nombre == "No ha consumido" ~ "No ha consumido",
      Nombre == "No consta" ~ "No consta",
      TRUE ~ as.character(NA)
    )
  )
consumo_alcohol7 <- 
  as.data.frame(apply(consumo_alcohol6, 2, function(col) col[!is.na(col)])) 

consumo_alcohol7$Valor <- consumo_alcohol7$Valor[seq(1, length(consumo_alcohol7$Valor), by = 3)]

consumo_alcohol8 <- consumo_alcohol7 %>%
  mutate(
    habitantes = case_when(
      Comunidades_autonomas == "Total nacional" & Sexo == "Ambos sexos" ~ 48592802,
      Comunidades_autonomas == "Total nacional" & Sexo == "Hombres" ~ 23807546,
      Comunidades_autonomas == "Total nacional" & Sexo == "Mujeres" ~ 24785363,
      Comunidades_autonomas == "Andalucía" & Sexo == "Ambos sexos" ~ 8620120,
      Comunidades_autonomas == "Andalucía" & Sexo == "Hombres" ~ 4238717,
      Comunidades_autonomas == "Andalucía" & Sexo == "Mujeres" ~ 4381403,
      Comunidades_autonomas == "Aragón" & Sexo == "Ambos sexos" ~ 1348918,
      Comunidades_autonomas == "Aragón" & Sexo == "Hombres" ~ 666949,
      Comunidades_autonomas == "Aragón" & Sexo == "Mujeres" ~ 681969,
      Comunidades_autonomas == "Principado de Asturias" & Sexo == "Ambos sexos" ~ 1008876,
      Comunidades_autonomas == "Principado de Asturias" & Sexo == "Hombres" ~ 481018,
      Comunidades_autonomas == "Principado de Asturias" & Sexo == "Mujeres" ~ 527858,
      Comunidades_autonomas == "Islas Baleares" & Sexo == "Ambos sexos" ~ 1231487,
      Comunidades_autonomas == "Islas Baleares" & Sexo == "Hombres" ~ 614011,
      Comunidades_autonomas == "Islas Baleares" & Sexo == "Mujeres" ~ 617476,
      Comunidades_autonomas == "Islas Canarias" & Sexo == "Ambos sexos" ~ 2236013,
      Comunidades_autonomas == "Islas Canarias" & Sexo == "Hombres" ~ 1103805,
      Comunidades_autonomas == "Islas Canarias" & Sexo == "Mujeres" ~ 1132208,
      Comunidades_autonomas == "Cantabria" & Sexo == "Ambos sexos" ~ 591151,
      Comunidades_autonomas == "Cantabria" & Sexo == "Hombres" ~ 286341,
      Comunidades_autonomas == "Cantabria" & Sexo == "Mujeres" ~ 304810,
      Comunidades_autonomas == "Castilla y León" & Sexo == "Ambos sexos" ~ 2389959,
      Comunidades_autonomas == "Castilla y León" & Sexo == "Hombres" ~ 1175016,
      Comunidades_autonomas == "Castilla y León" & Sexo == "Mujeres" ~ 1214943,
      Comunidades_autonomas == "Castilla-La Mancha" & Sexo == "Ambos sexos" ~ 2100523,
      Comunidades_autonomas == "Castilla-La Mancha" & Sexo == "Hombres" ~ 1053361,
      Comunidades_autonomas == "Castilla-La Mancha" & Sexo == "Mujeres" ~ 1047162,
      Comunidades_autonomas == "Cataluña" & Sexo == "Ambos sexos" ~ 8021049,
      Comunidades_autonomas == "Cataluña" & Sexo == "Hombres" ~ 3948555,
      Comunidades_autonomas == "Cataluña" & Sexo == "Mujeres" ~ 4072494,
      Comunidades_autonomas == "Comunidad Valenciana" & Sexo == "Ambos sexos" ~ 5316478,
      Comunidades_autonomas == "Comunidad Valenciana" & Sexo == "Hombres" ~ 2613918,
      Comunidades_autonomas == "Comunidad Valenciana" & Sexo == "Mujeres" ~ 2702560,
      Comunidades_autonomas == "Extremadura" & Sexo == "Ambos sexos" ~ 1053423,
      Comunidades_autonomas == "Extremadura" & Sexo == "Hombres" ~ 521005,
      Comunidades_autonomas == "Extremadura" & Sexo == "Mujeres" ~ 532418,
      Comunidades_autonomas == "Galicia" & Sexo == "Ambos sexos" ~ 2705877,
      Comunidades_autonomas == "Galicia" & Sexo == "Hombres" ~ 1301669,
      Comunidades_autonomas == "Galicia" & Sexo == "Mujeres" ~ 1404208,
      Comunidades_autonomas == "Madrid" & Sexo == "Ambos sexos" ~ 7000621,
      Comunidades_autonomas == "Madrid" & Sexo == "Hombres" ~ 3352591,
      Comunidades_autonomas == "Madrid" & Sexo == "Mujeres" ~ 3648030,
      Comunidades_autonomas == "Murcia" & Sexo == "Ambos sexos" ~ 1569164,
      Comunidades_autonomas == "Murcia" & Sexo == "Hombres" ~ 786213,
      Comunidades_autonomas == "Murcia" & Sexo == "Mujeres" ~ 782951,
      Comunidades_autonomas == "Navarra" & Sexo == "Ambos sexos" ~ 678103,
      Comunidades_autonomas == "Navarra" & Sexo == "Hombres" ~ 335742,
      Comunidades_autonomas == "Navarra" & Sexo == "Mujeres" ~ 342361,
      Comunidades_autonomas == "País Vasco" & Sexo == "Ambos sexos" ~ 2227581,
      Comunidades_autonomas == "País Vasco" & Sexo == "Hombres" ~ 1083234,
      Comunidades_autonomas == "País Vasco" & Sexo == "Mujeres" ~ 1144347,
      Comunidades_autonomas == "La Rioja" & Sexo == "Ambos sexos" ~ 324226,
      Comunidades_autonomas == "La Rioja" & Sexo == "Hombres" ~ 160074,
      Comunidades_autonomas == "La Rioja" & Sexo == "Mujeres" ~ 164152,
      Comunidades_autonomas == "Ceuta" & Sexo == "Ambos sexos" ~ 83284,
      Comunidades_autonomas == "Ceuta" & Sexo == "Hombres" ~ 41990,
      Comunidades_autonomas == "Ceuta" & Sexo == "Mujeres" ~ 41294,
      Comunidades_autonomas == "Melilla" & Sexo == "Ambos sexos" ~ 86056,
      Comunidades_autonomas == "Melilla" & Sexo == "Hombres" ~ 43337,
      Comunidades_autonomas == "Melilla" & Sexo == "Mujeres" ~ 42719,
      TRUE ~ as.numeric(NA)
    )
  )

#view(consumo_alcohol6)


consumo_por_sexo<- consumo_alcohol8 %>%
  group_by(Sexo) %>%
  summarize(consumo_medio_sexo=mean(as.numeric(Valor), na.rm = TRUE))
consumo_por_sexo
#view(consumo_por_sexo)

consumo_por_comunidad <- consumo_alcohol8 %>%
  group_by(Comunidades_autonomas) %>%
  summarize(consumo_medio_comunidad = mean(as.numeric(Valor), na.rm = TRUE))

#view(consumo_por_comunidad)

consumo_por_sexo_comunidad <- consumo_alcohol8 %>%
  group_by(Sexo, Comunidades_autonomas) %>%
  summarize(consumo_medio_sexo_comunidad = mean(as.numeric(Valor), na.rm = TRUE))

#view(consumo_por_sexo_comunidad)


ggplot(consumo_por_sexo, aes(x = Sexo, y = consumo_medio_sexo, fill = Sexo)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Consumo Medio de Alcohol por Sexo", x = "Sexo", y = "Consumo Medio (unidades)") +
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

#COMPARACIÓN CONSUMO DE ALCOHOL CON EL EJERCICIO FÍSICO POR SEXO (variable no importante)

# Promedio de ejercicio físico y tasas de suicidio por comunidad autónoma y sexo
resumen_suicidio_ejercicio <- comparacion_datos %>%
  group_by(Comunidades_autonomas, Sexo) %>%
  summarize(
    promedio_ejercicio = mean(, na.rm = TRUE),
    promedio_suicidios = mean(, na.rm = TRUE)
  )

  
  




