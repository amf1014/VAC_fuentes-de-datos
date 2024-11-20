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

#suicidio7 <- suicidio7%>%
  #select(-años)

# #suicidio8 <- suicidio7 %>%
#   #mutate(
#     habitantes = case_when(
#       comunidades_autonomas == "Total nacional" & sexo == "Ambos sexos" ~ "48.592.909",
#       comunidades_autonomas == "Total nacional" & sexo == "Hombres" ~ "23.807.546",
#       comunidades_autonomas == "Total nacional" & sexo == "Mujeres" ~ "24.785.363",
#       comunidades_autonomas == "Andalucía" & sexo == "Ambos sexos" ~ "8.620.120",
#       comunidades_autonomas == "Andalucía" & sexo == "Hombres" ~ "4.238.717",
#       comunidades_autonomas == "Andalucía" & sexo == "Mujeres" ~ "4.381.403",
#       comunidades_autonomas == "Aragón" & sexo == "Ambos sexos" ~ "1.348.918",
#       comunidades_autonomas == "Aragón" & sexo == "Hombres" ~ "666.949",
#       comunidades_autonomas == "Aragón" & sexo == "Mujeres" ~ "681.969 ",
#       comunidades_autonomas == "Principado de Asturias" & sexo == "Ambos sexos" ~ "1.008.876",
#       comunidades_autonomas == "Principado de Asturias" & sexo == "Hombres" ~ "481.018",
#       comunidades_autonomas == "Principado de Asturias" & sexo == "Mujeres" ~ "527.858",
#       comunidades_autonomas == "Islas Baleares" & sexo == "Ambos sexos" ~ "1.231.487",
#       comunidades_autonomas == "Islas Baleares" & sexo == "Hombres" ~ "614.011",
#       comunidades_autonomas == "Islas Baleares" & sexo == "Mujeres" ~ "617.476",
#       comunidades_autonomas == "Islas Canarias" & sexo == "Ambos sexos" ~ "2.236.013",
#       comunidades_autonomas == "Islas Canarias" & sexo == "Hombres" ~ "1.103.805",
#       comunidades_autonomas == "Islas Canarias" & sexo == "Mujeres" ~ "1.132.208",
#       comunidades_autonomas == "Cantabria" & sexo == "Ambos sexos" ~ "593.044",
#       comunidades_autonomas == "Cantabria" & sexo == "Hombres" ~ "286.341",
#       comunidades_autonomas == "Cantabria" & sexo == "Mujeres" ~ "304.810",
#       comunidades_autonomas == "Castilla y León" & sexo == "Ambos sexos" ~ "2.389.959",
#       comunidades_autonomas == "Castilla y León" & sexo == "Hombres" ~ "1.175.016",
#       comunidades_autonomas == "Castilla y León" & sexo == "Mujeres" ~ "1.214.943",
#       comunidades_autonomas == "Castilla-La Mancha" & sexo == "Ambos sexos" ~ "2.100.523",
#       comunidades_autonomas == "Castilla-La Mancha" & sexo == "Hombres" ~ "1.053.361",
#       comunidades_autonomas == "Castilla-La Mancha" & sexo == "Mujeres" ~ "1.047.162",
#       comunidades_autonomas == "Cataluña" & sexo == "Ambos sexos" ~ "8.021.049",
#       comunidades_autonomas == "Cataluña" & sexo == "Hombres" ~ "3.948.555",
#       comunidades_autonomas == "Cataluña" & sexo == "Mujeres" ~ "4.072.494",
#       comunidades_autonomas == "Comunidad Valenciana" & sexo == "Ambos sexos" ~ "5.316.478",
#       comunidades_autonomas == "Comunidad Valenciana" & sexo == "Hombres" ~ "2.613.918",
#       comunidades_autonomas == "Comunidad Valenciana" & sexo == "Mujeres" ~ "2.702.560",
#       comunidades_autonomas == "Extremadura" & sexo == "Ambos sexos" ~ "1.053.423",
#       comunidades_autonomas == "Extremadura" & sexo == "Hombres" ~ "521.005",
#       comunidades_autonomas == "Extremadura" & sexo == "Mujeres" ~ "532.418",
#       comunidades_autonomas == "Galicia" & sexo == "Ambos sexos" ~ "2.705.877",
#       comunidades_autonomas == "Galicia" & sexo == "Hombres" ~ "1.301.669",
#       comunidades_autonomas == "Galicia" & sexo == "Mujeres" ~ "1.404.208",
#       comunidades_autonomas == "Madrid" & sexo == "Ambos sexos" ~ "7.000.621",
#       comunidades_autonomas == "Madrid" & sexo == "Hombres" ~ "3.352.591",
#       comunidades_autonomas == "Madrid" & sexo == "Mujeres" ~ "3.648.030",
#       comunidades_autonomas == "Murcia" & sexo == "Ambos sexos" ~ "1.569.164",
#       comunidades_autonomas == "Murcia" & sexo == "Hombres" ~ "786.213",
#       comunidades_autonomas == "Murcia" & sexo == "Mujeres" ~ "782.951",
#       comunidades_autonomas == "Navarra" & sexo == "Ambos sexos" ~ "678.103",
#       comunidades_autonomas == "Navarra" & sexo == "Hombres" ~ "335.742",
#       comunidades_autonomas == "Navarra" & sexo == "Mujeres" ~ "342.361",
#       comunidades_autonomas == "País Vasco" & sexo == "Ambos sexos" ~ "2.227.581",
#       comunidades_autonomas == "País Vasco" & sexo == "Hombres" ~ "1.083.234",
#       comunidades_autonomas == "País Vasco" & sexo == "Mujeres" ~ "1.144.347",
#       comunidades_autonomas == "La Rioja" & sexo == "Ambos sexos" ~ "324.226",
#       comunidades_autonomas == "La Rioja" & sexo == "Hombres" ~ "160.074",
#       comunidades_autonomas == "La Rioja" & sexo == "Mujeres" ~ "164.152",
#       comunidades_autonomas == "Ceuta" & sexo == "Ambos sexos" ~ "83.284",
#       comunidades_autonomas == "Ceuta" & sexo == "Hombres" ~ "41.990",
#       comunidades_autonomas == "Ceuta" & sexo == "Mujeres" ~ "41.294",
#       comunidades_autonomas == "Melilla" & sexo == "Ambos sexos" ~ "86.056",
#       comunidades_autonomas == "Melilla" & sexo == "Hombres" ~ "43.337",
#       comunidades_autonomas == "Melilla" & sexo == "Mujeres" ~ "42.719",
#       TRUE ~ as.character(NA)
#     )
#   )
# view(suicidio8)
