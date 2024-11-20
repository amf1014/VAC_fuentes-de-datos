library(dplyr)
library(tidyverse)
library(rjson)
library(tidyjson)

suicidio <- fromJSON(file = "INPUT/DATA/Suicidios_por_comunidades.json")

suicidio

head(suicidio)

suicidio %>%
  gather_object %>%
  json_types %>%
  count(name,type)


suicidio1 <- suicidio %>%
  enter_object(Data) %>%
  gather_array %>%
  spread_all %>%
  select("document.id", "Valor")


suicidio2 <- suicidio %>%
  enter_object(MetaData) %>%
  gather_array %>%
  spread_all %>%
  select("document.id", "Nombre")
  

suicidio3 <- suicidio1 %>%
  select(c("document.id","Valor")) %>%
  full_join(x = .,
            y = suicidio2 %>%
              select(c("document.id","Nombre")),
            by = c("document.id"))

#view(suicidio3)

suicidio4 <- suicidio3 %>%
  mutate(
    sexo = case_when(
      Nombre == "Ambos sexos" ~ "Ambos sexos",
      Nombre == "Hombres" ~ "Hombres",
      Nombre == "Mujeres" ~ "Mujeres",
      TRUE ~ as.character(NA)
    )
  )

suicidio5 <- suicidio4 %>%
  mutate(
    años = case_when(
      Nombre == "Todas las edades" ~ "Todas las edades",
      Nombre == "Menores de 15 años" ~ "Menores de 15 años",
      Nombre == "De 15 a 29 años" ~ "De 15 a 29 años",
      Nombre == "De 30 a 39 años" ~ "De 30 a 39 años",
      Nombre == "De 40 a 44 años" ~ "De 40 a 44 años",
      Nombre == "De 45 a 49 años" ~ "De 45 a 49 años",
      Nombre == "De 50 a 54 años" ~ "De 50 a 54 años",
      Nombre == "De 55 a 59 años" ~ "De 55 a 59 años",
      Nombre == "De 60 a 64 años" ~ "De 60 a 64 años",
      Nombre == "De 65 a 69 años" ~ "De 65 a 69 años",
      Nombre == "De 70 a 74 años" ~ "De 70 a 74 años",
      Nombre == "De 75 a 79 años" ~ "De 75 a 79 años",
      Nombre == "De 80 a 84 años" ~ "De 80 a 84 años",
      Nombre == "De 85 a 89 años" ~ "De 85 a 89 años",
      Nombre == "De 90 a 94 años" ~ "De 90 a 94 años",
      Nombre == "De 95 años y más" ~ "De 95 años y más",
      TRUE ~ as.character(NA)
    )
  )

suicidio6 <- suicidio5 %>%
  mutate(
    comunidades_autonomas = case_when(
      Nombre == "Total" ~ "Total nacional",
      Nombre == "Andalucía" ~ "Andalucía",
      Nombre == "Aragón" ~ "Aragón",
      Nombre == "Asturias, Principado de" ~ "Principado de Asturias",
      Nombre == "Balears, Illes" ~ "Islas Baleares",
      Nombre == "Canarias" ~ "Islas Canarias",
      Nombre == "Cantabria" ~ "Cantabria",
      Nombre == "Castilla y León" ~ "Castilla y León",
      Nombre == "Castilla-La Mancha" ~ "Castilla-La Mancha",
      Nombre == "Cataluña" ~ "Cataluña",
      Nombre == "Comunitat Valenciana" ~ "Comunidad Valenciana",
      Nombre == "Extremadura" ~ "Extremadura",
      Nombre == "Galicia" ~ "Galicia",
      Nombre == "Madrid, Comunidad de" ~ "Madrid",
      Nombre == "Murcia, Región de" ~ "Murcia",
      Nombre == "Navarra, Comunidad Foral de" ~ "Navarra",
      Nombre == "País Vasco" ~ "País Vasco",
      Nombre == "Rioja, La" ~ "La Rioja",
      Nombre == "Ceuta" ~ "Ceuta",
      Nombre == "Melilla" ~ "Melilla",
      TRUE ~ as.character(NA)
    )
  )

#view(suicidio6)

suicidio7 <- 
  as.data.frame(apply(suicidio6, 2, function(col) col[!is.na(col)])) 
  

#view(suicidio7)

suicidio7 <- suicidio7[, -3]

#view(suicidio7)


suicidio7$Valor <- suicidio7$Valor[seq(1, length(suicidio7$Valor), by = 3)]
#view(suicidio7)


suicidio_por_sexo <- suicidio7 %>%
  group_by(sexo) %>%
  summarize(suicidio_medio_sexo=mean(as.numeric(Valor), na.rm = TRUE))

#view(suicidio_por_sexo)

suicidio_por_comunidad <- suicidio7 %>%
  group_by(comunidades_autonomas) %>%
  summarize(suicidio_medio_comunidad=mean(as.numeric(Valor), na.rm = TRUE))

#view(suicidio_por_comunidad)

suicidio_por_edad <- suicidio7 %>%
  group_by(años) %>%
  summarize(suicidio_medio_edad=mean(as.numeric(Valor), na.rm = TRUE))

#view(suicidio_por_edad)

suicidio_por_sexo_comunidad <- suicidio7 %>%
  group_by(sexo, comunidades_autonomas) %>%
  summarize(suicidio_medio_sexo_comunidad=mean(as.numeric(Valor), na.rm = TRUE))

#view(suicidio_por_sexo_comunidad_edad)

ggplot(suicidio_por_sexo_comunidad, mapping = aes(x = comunidades_autonomas, y = suicidio_medio_sexo_comunidad,fill = sexo)) +
  geom_bar(stat = "identity",position = position_dodge()) +
  labs(title = "Suicidio medio por Sexo y Comunidad Autónoma",x = "Comunidad Autónoma",y = "Suicidio Medio (unidades)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggplot(suicidio_por_comunidad, aes(x = reorder(comunidades_autonomas, suicidio_medio_comunidad), y = suicidio_medio_comunidad, fill = comunidades_autonomas)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Suicidio Medio por Comunidad Autónoma", x = "Comunidad Autónoma", y = "Suicidio Medio (unidades)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(suicidio_por_edad, aes(x = reorder(años, suicidio_medio_edad), y = suicidio_medio_edad, fill = años)) + geom_bar(stat = "identity", show.legend = FALSE) + 
  labs(title = "Suicidio Medio por Edad", x = "Edad", y = "Suicidio Medio (unidades)") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

suicidio8 <- suicidio7 %>%
  mutate(
    habitantes = case_when(
      comunidades_autonomas == "Total nacional" ~ "48.797.875",
      comunidades_autonomas == "Andalucía" ~ "8.637.152",
      comunidades_autonomas == "Aragón" ~ " 1.348.206 ",
      comunidades_autonomas == "Principado de Asturias" ~ "1.010.058",
      comunidades_autonomas == "Islas Baleares" ~ "1.238.812",
      comunidades_autonomas == "Islas Canarias" ~ "2.246.132",
      comunidades_autonomas == "Cantabria" ~ "593.044",
      comunidades_autonomas == "Castilla y León" ~ "2.393.741",
      comunidades_autonomas == "Castilla-La Mancha" ~ "2.107.420",
      comunidades_autonomas == "Cataluña" ~ "8.021.049",
      comunidades_autonomas == "Comunidad Valenciana" ~ "5.359.309",
      comunidades_autonomas == "Extremadura" ~ " 1.052.190",
      comunidades_autonomas == "Galicia" ~ "2.706.953",
      comunidades_autonomas == "Madrid" ~ "7.058.041",
      comunidades_autonomas == "Murcia" ~ "1.575.171",
      comunidades_autonomas == "Navarra" ~ "682.201",
      comunidades_autonomas == "País Vasco" ~ "2.219.019",
      comunidades_autonomas == "La Rioja" ~ "325.264",
      comunidades_autonomas == "Ceuta" ~ "83.386",
      comunidades_autonomas == "Melilla" ~ "86.418",
      TRUE ~ as.character(NA)
    )
  )
view(suicidio8)
