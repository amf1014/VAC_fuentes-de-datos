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
    sexo = case_when(
      Nombre == "Ambos sexos" ~ "Ambos sexos",
      Nombre == "Hombres" ~ "Hombres",
      Nombre == "Mujeres" ~ "Mujeres",
      TRUE ~ as.character(NA)
    )
  )
consumo_alcohol5 <- consumo_alcohol4 %>%
  mutate(
    comunidades_autonomas = case_when(
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
    años = case_when(
      Nombre == "TOTAL" ~ "Total consumido",
      Nombre == "Si ha consumido" ~ "Si ha consumido",
      Nombre == "No ha consumido" ~ "No ha consumido",
      Nombre == "No consta" ~ "No consta",
      TRUE ~ as.character(NA)
    )
  )
view(consumo_alcohol6)

consumo_por_sexo<- consumo_alcohol6 %>%
  group_by(sexo) %>%
  summarize(consumo_medio_sexo=mean(Valor, na.rm = TRUE))

#view(consumo_por_sexo)

consumo_por_comunidad <- consumo_alcohol6 %>%
  group_by(comunidades_autonomas) %>%
  summarize(consumo_medio_comunidad = mean(Valor, na.rm = TRUE))

#view(consumo_por_comunidad)

consumo_por_sexo_comunidad <- consumo_alcohol6 %>%
  group_by(sexo, comunidades_autonomas) %>%
  summarize(consumo_medio_sexo_comunidad = mean(Valor, na.rm = TRUE))

#view(consumo_por_sexo_comunidad)

ggplot(consumo_por_sexo_comunidad, mapping = aes(x = comunidades_autonomas, y = consumo_medio_sexo_comunidad,fill = sexo)) +
  geom_bar(stat = "identity",position = position_dodge()) +
  labs(title = "Consumo Medio de Alcohol por Sexo y Comunidad Autónoma",x = "Comunidad Autónoma",y = "Consumo Medio (unidades)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
