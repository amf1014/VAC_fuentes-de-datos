install.packages("mapSpain", dependencies = TRUE)
install.packages("sf")
install.packages("patchwork")
install.packages('DT')
library(dplyr)
library(tidyverse)
library(rjson)
library(tidyjson)
library(ggplot2)
library(mapSpain)
library(sf)
library(patchwork)
library(DT)

#Importación de suicidio

suicidio <- fromJSON(file = "INPUT/DATA/Suicidios_por_comunidades.json")

#Importación de consumo de alcohol

consumo_alcohol <- fromJSON(file = "INPUT/DATA/consumo_de_alcohol.json")

#Importacion de ejercicio físico

ejercicioFisico <- fromJSON(file = "INPUT/DATA/Ejercicio_fisico.json")

#Formateo de datos
##Formateo de datos suicidio
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


suicidio7 <- 
  as.data.frame(apply(suicidio6, 2, function(col) col[!is.na(col)])) 




suicidio7 <- suicidio7[, -3]




suicidio7$Valor <- suicidio7$Valor[seq(1, length(suicidio7$Valor), by = 3)]


suicidio7$Valor <- suicidio7$Valor[seq(1, length(suicidio7$Valor), by = 16)]

suicidio7$sexo <- suicidio7$sexo[seq(1, length(suicidio7$sexo), by = 16)]

suicidio7$comunidades_autonomas <- suicidio7$comunidades_autonomas[seq(1, length(suicidio7$comunidades_autonomas), by = 16)]


suicidio8 <- suicidio7%>%
  select(-años)


suicidio9 <- suicidio8 %>%
  mutate(
    habitantes = case_when(
      comunidades_autonomas == "Total nacional" & sexo == "Ambos sexos" ~ 48592802,
      comunidades_autonomas == "Total nacional" & sexo == "Hombres" ~ 23807546,
      comunidades_autonomas == "Total nacional" & sexo == "Mujeres" ~ 24785363,
      comunidades_autonomas == "Andalucía" & sexo == "Ambos sexos" ~ 8620120,
      comunidades_autonomas == "Andalucía" & sexo == "Hombres" ~ 4238717,
      comunidades_autonomas == "Andalucía" & sexo == "Mujeres" ~ 4381403,
      comunidades_autonomas == "Aragón" & sexo == "Ambos sexos" ~ 1348918,
      comunidades_autonomas == "Aragón" & sexo == "Hombres" ~ 666949,
      comunidades_autonomas == "Aragón" & sexo == "Mujeres" ~ 681969,
      comunidades_autonomas == "Principado de Asturias" & sexo == "Ambos sexos" ~ 1008876,
      comunidades_autonomas == "Principado de Asturias" & sexo == "Hombres" ~ 481018,
      comunidades_autonomas == "Principado de Asturias" & sexo == "Mujeres" ~ 527858,
      comunidades_autonomas == "Islas Baleares" & sexo == "Ambos sexos" ~ 1231487,
      comunidades_autonomas == "Islas Baleares" & sexo == "Hombres" ~ 614011,
      comunidades_autonomas == "Islas Baleares" & sexo == "Mujeres" ~ 617476,
      comunidades_autonomas == "Islas Canarias" & sexo == "Ambos sexos" ~ 2236013,
      comunidades_autonomas == "Islas Canarias" & sexo == "Hombres" ~ 1103805,
      comunidades_autonomas == "Islas Canarias" & sexo == "Mujeres" ~ 1132208,
      comunidades_autonomas == "Cantabria" & sexo == "Ambos sexos" ~ 591151,
      comunidades_autonomas == "Cantabria" & sexo == "Hombres" ~ 286341,
      comunidades_autonomas == "Cantabria" & sexo == "Mujeres" ~ 304810,
      comunidades_autonomas == "Castilla y León" & sexo == "Ambos sexos" ~ 2389959,
      comunidades_autonomas == "Castilla y León" & sexo == "Hombres" ~ 1175016,
      comunidades_autonomas == "Castilla y León" & sexo == "Mujeres" ~ 1214943,
      comunidades_autonomas == "Castilla-La Mancha" & sexo == "Ambos sexos" ~ 2100523,
      comunidades_autonomas == "Castilla-La Mancha" & sexo == "Hombres" ~ 1053361,
      comunidades_autonomas == "Castilla-La Mancha" & sexo == "Mujeres" ~ 1047162,
      comunidades_autonomas == "Cataluña" & sexo == "Ambos sexos" ~ 8021049,
      comunidades_autonomas == "Cataluña" & sexo == "Hombres" ~ 3948555,
      comunidades_autonomas == "Cataluña" & sexo == "Mujeres" ~ 4072494,
      comunidades_autonomas == "Comunidad Valenciana" & sexo == "Ambos sexos" ~ 5316478,
      comunidades_autonomas == "Comunidad Valenciana" & sexo == "Hombres" ~ 2613918,
      comunidades_autonomas == "Comunidad Valenciana" & sexo == "Mujeres" ~ 2702560,
      comunidades_autonomas == "Extremadura" & sexo == "Ambos sexos" ~ 1053423,
      comunidades_autonomas == "Extremadura" & sexo == "Hombres" ~ 521005,
      comunidades_autonomas == "Extremadura" & sexo == "Mujeres" ~ 532418,
      comunidades_autonomas == "Galicia" & sexo == "Ambos sexos" ~ 2705877,
      comunidades_autonomas == "Galicia" & sexo == "Hombres" ~ 1301669,
      comunidades_autonomas == "Galicia" & sexo == "Mujeres" ~ 1404208,
      comunidades_autonomas == "Madrid" & sexo == "Ambos sexos" ~ 7000621,
      comunidades_autonomas == "Madrid" & sexo == "Hombres" ~ 3352591,
      comunidades_autonomas == "Madrid" & sexo == "Mujeres" ~ 3648030,
      comunidades_autonomas == "Murcia" & sexo == "Ambos sexos" ~ 1569164,
      comunidades_autonomas == "Murcia" & sexo == "Hombres" ~ 786213,
      comunidades_autonomas == "Murcia" & sexo == "Mujeres" ~ 782951,
      comunidades_autonomas == "Navarra" & sexo == "Ambos sexos" ~ 678103,
      comunidades_autonomas == "Navarra" & sexo == "Hombres" ~ 335742,
      comunidades_autonomas == "Navarra" & sexo == "Mujeres" ~ 342361,
      comunidades_autonomas == "País Vasco" & sexo == "Ambos sexos" ~ 2227581,
      comunidades_autonomas == "País Vasco" & sexo == "Hombres" ~ 1083234,
      comunidades_autonomas == "País Vasco" & sexo == "Mujeres" ~ 1144347,
      comunidades_autonomas == "La Rioja" & sexo == "Ambos sexos" ~ 324226,
      comunidades_autonomas == "La Rioja" & sexo == "Hombres" ~160074,
      comunidades_autonomas == "La Rioja" & sexo == "Mujeres" ~ 164152,
      comunidades_autonomas == "Ceuta" & sexo == "Ambos sexos" ~ 83284,
      comunidades_autonomas == "Ceuta" & sexo == "Hombres" ~ 41990,
      comunidades_autonomas == "Ceuta" & sexo == "Mujeres" ~ 41294,
      comunidades_autonomas == "Melilla" & sexo == "Ambos sexos" ~ 86056,
      comunidades_autonomas == "Melilla" & sexo == "Hombres" ~ 43337,
      comunidades_autonomas == "Melilla" & sexo == "Mujeres" ~ 42719,
      TRUE ~ as.numeric(NA)
    )
  )


##Formateo de datos de consumo de alcohol

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


##Formateo de datos ejercicio físico


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

ejercicioFisicoUnionFinal <- ejercicioFisicoUnion %>%
  select(-Miles_de_personas, -Ratio)



#Analisis de datos
##Analisis de datos del suicidio con gráficas

suicidio10 <- suicidio9 %>%
  mutate(
    porcentaje_suicidios = (as.numeric(Valor) / habitantes) * 100 
  )

suicidio10 <- suicidio10 %>% slice(1:60)

suicidio_por_sexo <- suicidio10 %>%
  group_by(sexo) %>%
  summarize(suicidio_medio_sexo=mean(as.numeric(porcentaje_suicidios), na.rm = TRUE))



suicidio_por_comunidad <- suicidio10 %>%
  group_by(comunidades_autonomas) %>%
  summarize(suicidio_medio_comunidad=mean(as.numeric(porcentaje_suicidios), na.rm = TRUE))




suicidio_por_sexo_comunidad <- suicidio10 %>%
  group_by(sexo, comunidades_autonomas) %>%
  summarize(suicidio_medio_sexo_comunidad=mean(as.numeric(porcentaje_suicidios), na.rm = TRUE))



graf_suicidio_por_sexo_comunidad <- ggplot(suicidio_por_sexo_comunidad, mapping = aes(x = comunidades_autonomas, y = suicidio_medio_sexo_comunidad,fill = sexo)) +
  geom_bar(stat = "identity",position = position_dodge()) +
  labs(title = "Suicidio medio por Sexo y Comunidad Autónoma",x = "Comunidad Autónoma",y = "Suicidio Medio (unidades)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

graf_suicidio_por_sexo_comunidad

graf_suicidio_por_comunidad <- ggplot(suicidio_por_comunidad, aes(x = reorder(comunidades_autonomas, suicidio_medio_comunidad), y = suicidio_medio_comunidad, fill = comunidades_autonomas)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Suicidio Medio por Comunidad Autónoma", x = "Comunidad Autónoma", y = "Suicidio Medio (unidades)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

graf_suicidio_por_comunidad

graf_suicidio_por_sexo <- ggplot(suicidio_por_sexo, aes(x = reorder(sexo, suicidio_medio_sexo), y = suicidio_medio_sexo, fill = sexo)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Suicidio Medio por sexo", x = "Sexo", y = "Suicidio Medio (unidades)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

graf_suicidio_por_sexo

census <- mapSpain::pobmun19

codelist <- mapSpain::esp_codelist %>%
  select(cpro, codauto) %>%
  distinct()

census_ccaa <- census %>%
  left_join(codelist) %>%
  group_by(codauto) %>%
  summarise(pob19 = sum(pob19), men = sum(men), women = sum(women)) %>%
  mutate(
    porc_women = women / pob19,
    porc_women_lab = paste0(round(100 * porc_women, 2), "%")
  )
ccaa_sf <- esp_get_ccaa() %>%
  left_join(census_ccaa)
can <- esp_get_can_box()


suicidio10 <- suicidio10 %>%
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


suicidio10 <- suicidio9 %>%
  mutate(
    porcentaje_suicidios = (as.numeric(Valor) / habitantes) * 100 
  )

suicidio10 <- suicidio10 %>% slice(1:60)

suicidio_por_sexo <- suicidio10 %>%
  group_by(sexo) %>%
  summarize(suicidio_medio_sexo=mean(as.numeric(porcentaje_suicidios), na.rm = TRUE))



suicidio_por_comunidad <- suicidio10 %>%
  group_by(comunidades_autonomas) %>%
  summarize(suicidio_medio_comunidad=mean(as.numeric(porcentaje_suicidios), na.rm = TRUE))




suicidio_por_sexo_comunidad <- suicidio10 %>%
  group_by(sexo, comunidades_autonomas) %>%
  summarize(suicidio_medio_sexo_comunidad=mean(as.numeric(porcentaje_suicidios), na.rm = TRUE))



graf_suicidio_por_sexo_comunidad <- ggplot(suicidio_por_sexo_comunidad, mapping = aes(x = comunidades_autonomas, y = suicidio_medio_sexo_comunidad,fill = sexo)) +
  geom_bar(stat = "identity",position = position_dodge()) +
  labs(title = "Suicidio medio por Sexo y Comunidad Autónoma",x = "Comunidad Autónoma",y = "Suicidio Medio (unidades)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

graf_suicidio_por_sexo_comunidad

graf_suicidio_por_comunidad <- ggplot(suicidio_por_comunidad, aes(x = reorder(comunidades_autonomas, suicidio_medio_comunidad), y = suicidio_medio_comunidad, fill = comunidades_autonomas)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Suicidio Medio por Comunidad Autónoma", x = "Comunidad Autónoma", y = "Suicidio Medio (unidades)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

graf_suicidio_por_comunidad

graf_suicidio_por_sexo <- ggplot(suicidio_por_sexo, aes(x = reorder(sexo, suicidio_medio_sexo), y = suicidio_medio_sexo, fill = sexo)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Suicidio Medio por sexo", x = "Sexo", y = "Suicidio Medio (unidades)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

graf_suicidio_por_sexo

census <- mapSpain::pobmun19

codelist <- mapSpain::esp_codelist %>%
  select(cpro, codauto) %>%
  distinct()

census_ccaa <- census %>%
  left_join(codelist) %>%
  group_by(codauto) %>%
  summarise(pob19 = sum(pob19), men = sum(men), women = sum(women)) %>%
  mutate(
    porc_women = women / pob19,
    porc_women_lab = paste0(round(100 * porc_women, 2), "%")
  )
ccaa_sf <- esp_get_ccaa() %>%
  left_join(census_ccaa)
can <- esp_get_can_box()


suicidio10 <- suicidio10 %>%
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

suicidio_global <- suicidio10 %>%
  filter(sexo == "Ambos sexos") %>%
  group_by(comunidades_autonomas)%>%
  summarize(porcentaje_global_suicidios = mean(porcentaje_suicidios, na.rm = TRUE))

suicidio_mujeres <- suicidio10 %>%
  filter(sexo == "Mujeres") %>%
  group_by(comunidades_autonomas) %>%
  summarize(
    porcentaje_mujeres_suicidios = mean(porcentaje_suicidios, na.rm = TRUE)
  )

suicidio_hombres <- suicidio10 %>%
  filter(sexo == "Hombres") %>%
  group_by(comunidades_autonomas)%>%
  summarize(porcentaje_hombres_suicidios = mean(porcentaje_suicidios, na.rm = TRUE))

levels(factor(ccaa_sf$ine.ccaa.name))
levels(factor(suicidio_mujeres$comunidades_autonomas))

ccaa_sf <- esp_get_ccaa() %>%
  left_join(suicidio_mujeres, by = c("ine.ccaa.name" = "comunidades_autonomas"))

ccaa_sm <- esp_get_ccaa() %>%
  left_join(suicidio_hombres, by = c("ine.ccaa.name" = "comunidades_autonomas"))

ccaa_sg <- esp_get_ccaa() %>%
  left_join(suicidio_global, by = c("ine.ccaa.name" = "comunidades_autonomas"))

grafico_suicidio_global <- ggplot(ccaa_sg) +
  geom_sf(aes(fill = porcentaje_global_suicidios), color = "grey70", linewidth = .3) +
  geom_sf(data = can, color = "grey70") +
  geom_sf_label(aes(label = round(porcentaje_global_suicidios, 4)),
                fill = "white", alpha = 0.5,
                size = 3, label.size = 0
  ) +
  scale_fill_gradientn(
    colors = hcl.colors(10, "Greens", rev = TRUE),
    n.breaks = 10, labels = scales::label_number(suffix = "%"),
    guide = guide_legend(title = "Porcentaje Global Suicidios", position = "inside")
  ) +
  theme_void() +
  theme(legend.position = c(0.1, 0.6))

grafico_suicidio_global

grafico_suicidio_mujeres <- ggplot(ccaa_sf) +
  geom_sf(aes(fill = porcentaje_mujeres_suicidios), color = "grey70", linewidth = .3) +
  geom_sf(data = can, color = "grey70") +
  geom_sf_label(aes(label = round(porcentaje_mujeres_suicidios, 4)),
                fill = "white", alpha = 0.5,
                size = 3, label.size = 0
  ) +
  scale_fill_gradientn(
    colors = hcl.colors(10, "Reds", rev = TRUE),
    n.breaks = 10, labels = scales::label_number(suffix = "%"),
    guide = guide_legend(title = "Porcentaje Mujeres Suicidios", position = "inside")
  ) +
  theme_void() +
  theme(legend.position = c(0.1, 0.6))

grafico_suicidios_hombres <- ggplot(ccaa_sm) +
  geom_sf(aes(fill = porcentaje_hombres_suicidios), color = "grey70", linewidth = .3) +
  geom_sf(data = can, color = "grey70") +
  geom_sf_label(aes(label = round(porcentaje_hombres_suicidios, 4)),
                fill = "white", alpha = 0.5,
                size = 3, label.size = 0
  ) +
  scale_fill_gradientn(
    colors = hcl.colors(10, "Blues", rev = TRUE),
    n.breaks = 10, labels = scales::label_number(suffix = "%"),
    guide = guide_legend(title = "Porcentaje Hombres Suicidios", position = "inside")
  ) +
  theme_void() +
  theme(legend.position = c(0.1, 0.6))

grafico_suicidio_por_sexos <- grafico_suicidio_mujeres + grafico_suicidios_hombres

grafico_suicidio_por_sexos

ggsave(
  filename = "Suicidio_global_mapa.jpeg",
  plot = grafico_suicidio_global ,
  path = "OUTPUT/Figures", 
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Suicidio_por_sexos_mapa.jpeg",
  plot = grafico_suicidio_por_sexos ,
  path = "OUTPUT/Figures",
  scale = 0.5,
  width = 100,
  height = 50,
  units = "cm",
  dpi = 320
)



##Analisis de datos del consumo de alcohol con gráficas

consumo_alcohol7 <- 
  as.data.frame(apply(consumo_alcohol6, 2, function(col) col[!is.na(col)])) 



consumo_alcohol7$Valor <- consumo_alcohol7$Valor[seq(1, length(consumo_alcohol7$Valor), by = 3 )]



#Hago porcentaje de las personas que si han consumido con el total de personas encuestadas.

consumo_alcohol8<-consumo_alcohol7%>%
  pivot_wider(names_from = Consumido, values_from = Valor)


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


consumo_alcohol10$Comunidades_autonomas <- consumo_alcohol10$Comunidades_autonomas[seq(1, length(consumo_alcohol10$Comunidades_autonomas), by =  4)]
consumo_alcohol10$Sexo <- consumo_alcohol10$Sexo[seq(1, length(consumo_alcohol10$Sexo), by =  4)]


#Gráficas de España
census_2 <- mapSpain::pobmun19

codelist_2 <- mapSpain::esp_codelist %>%
  select(cpro, codauto) %>%
  distinct()

census_ccaa_2 <- census_2 %>%
  left_join(codelist_2) %>%
  group_by(codauto) %>%
  summarise(pob19 = sum(pob19), men = sum(men), women = sum(women)) %>%
  mutate(
    porc_women = women / pob19,
    porc_women_lab = paste0(round(100 * porc_women, 2), "%")
  )
ccaa_sf_2 <- esp_get_ccaa() %>%
  left_join(census_ccaa_2)
can_2 <- esp_get_can_box()

consumo_alcohol10 <- consumo_alcohol10 %>%
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

consumo_global <- consumo_alcohol10 %>%
  filter(Sexo == "Ambos sexos") %>%
  group_by(Comunidades_autonomas)%>%
  summarize(Porcentaje_global_consumo = mean(Porcentaje_consumo, na.rm = TRUE))


no_consumo_global <- consumo_alcohol10 %>%
  filter(Sexo == "Ambos sexos") %>%
  group_by(Comunidades_autonomas)%>%
  summarize(Porcentaje_global_no_consumo = mean(Porcentaje_no_consumo, na.rm = TRUE))


consumo_mujeres <- consumo_alcohol10 %>%
  filter(Sexo == "Mujeres") %>%
  group_by(Comunidades_autonomas) %>%
  summarize(Porcentaje_mujeres_consumo = mean(Porcentaje_consumo, na.rm = TRUE))

consumo_hombres <- consumo_alcohol10 %>%
  filter(Sexo == "Hombres") %>%
  group_by(Comunidades_autonomas)%>%
  summarize(Porcentaje_hombres_consumo = mean(Porcentaje_consumo, na.rm = TRUE))

levels(factor(ccaa_sf_2$ine.ccaa.name))
levels(factor(consumo_mujeres$Comunidades_autonomas))

ccaa_sf_2 <- esp_get_ccaa() %>%
  left_join(consumo_mujeres, by = c("ine.ccaa.name" = "Comunidades_autonomas"))

ccaa_sm_2 <- esp_get_ccaa() %>%
  left_join(consumo_hombres, by = c("ine.ccaa.name" = "Comunidades_autonomas"))

ccaa_sg_2 <- esp_get_ccaa() %>%
  left_join(consumo_global, by = c("ine.ccaa.name" = "Comunidades_autonomas"))

grafico_consumo_global <- ggplot(ccaa_sg_2) +
  geom_sf(aes(fill = Porcentaje_global_consumo), color = "grey70", linewidth = .3) +
  geom_sf(data = can_2, color = "grey70") +
  geom_sf_label(aes(label = round(Porcentaje_global_consumo, 2)),
                fill = "white", alpha = 0.5,
                size = 3, label.size = 0
  ) +
  scale_fill_gradientn(
    colors = hcl.colors(10, "Greens", rev = TRUE),
    n.breaks = 10, labels = scales::label_number(suffix = "%"),
    guide = guide_legend(title = "Porcentaje Global Consumo", position = "inside")
  ) +
  theme_void() +
  theme(legend.position = c(0.1, 0.6))

grafico_consumo_global

grafico_consumo_mujeres <- ggplot(ccaa_sf_2) +
  geom_sf(aes(fill = Porcentaje_mujeres_consumo), color = "grey70", linewidth = .3) +
  geom_sf(data = can_2, color = "grey70") +
  geom_sf_label(aes(label = round(Porcentaje_mujeres_consumo, 2)),
                fill = "white", alpha = 0.5,
                size = 3, label.size = 0
  ) +
  scale_fill_gradientn(
    colors = hcl.colors(10, "Reds", rev = TRUE),
    n.breaks = 10, labels = scales::label_number(suffix = "%"),
    guide = guide_legend(title = "Porcentaje Mujeres Consumo", position = "inside")
  ) +
  theme_void() +
  theme(legend.position = c(0.1, 0.6))

grafico_consumo_hombres <- ggplot(ccaa_sm_2) +
  geom_sf(aes(fill = Porcentaje_hombres_consumo), color = "grey70", linewidth = .3) +
  geom_sf(data = can_2, color = "grey70") +
  geom_sf_label(aes(label = round(Porcentaje_hombres_consumo, 2)),
                fill = "white", alpha = 0.5,
                size = 3, label.size = 0
  ) +
  scale_fill_gradientn(
    colors = hcl.colors(10, "Blues", rev = TRUE),
    n.breaks = 10, labels = scales::label_number(suffix = "%"),
    guide = guide_legend(title = "Porcentaje Hombres Consumo", position = "inside")
  ) +
  theme_void() +
  theme(legend.position = c(0.1, 0.6))

grafico_consumo_por_sexos <- grafico_consumo_mujeres + grafico_consumo_hombres

grafico_consumo_por_sexos

#Guardo los graficos
ggsave(
  filename = "Consumo_global.jpeg",
  plot = grafico_consumo_global ,
  path = "OUTPUT/Figures", # ruta relativa
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Consumo_por_sexos.jpeg",
  plot = grafico_consumo_por_sexos ,
  path = "OUTPUT/Figures", # ruta relativa
  scale = 0.5,
  width = 100,
  height = 50,
  units = "cm",
  dpi = 320
)
#DATOS PARA GRÁFICAS

consumo_por_sexo <- consumo_alcohol10 %>%
  group_by(Sexo) %>%
  summarize(consumo_medio_sexo = mean(Porcentaje_consumo, na.rm = TRUE)) 


consumo_por_comunidad <- consumo_alcohol10 %>%
  group_by(Comunidades_autonomas) %>%
  summarize(consumo_medio_comunidad = mean(Porcentaje_consumo, na.rm = TRUE))


consumo_por_sexo_comunidad <- consumo_alcohol10 %>%
  group_by(Sexo, Comunidades_autonomas) %>%
  summarize(consumo_medio_sexo_comunidad = mean(Porcentaje_consumo, na.rm = TRUE))



Grafica_consumo_por_sexo<-ggplot(consumo_por_sexo, aes(x = Sexo, y = consumo_medio_sexo, fill = Sexo)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Consumo Medio de Alcohol por Sexo", x = "Sexo", y = "Consumo Medio (%)") 

ggsave(
  filename = "Grafica_consumo_por_sexo.jpeg",
  plot = Grafica_consumo_por_sexo ,
  path = "OUTPUT/Figures", 
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)
Grafica_consumo_por_comunidad<-ggplot(consumo_por_comunidad, aes(x = reorder(Comunidades_autonomas, consumo_medio_comunidad), y = consumo_medio_comunidad, fill = Comunidades_autonomas)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  labs(title = "Consumo Medio de Alcohol por Comunidad Autónoma", x = "Comunidad Autónoma", y = "Porcentaje Medio de Consumo") +theme_minimal() +theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(
  filename = "Grafica_consumo_por_comunidad.jpeg",
  plot = Grafica_consumo_por_comunidad,
  path = "OUTPUT/Figures", 
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

Grafica_consumo_por_comunidad_y_sexo<-ggplot(consumo_por_sexo_comunidad, aes(x = reorder(Comunidades_autonomas, consumo_medio_sexo_comunidad), y = consumo_medio_sexo_comunidad, fill = Sexo)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  labs(title = "Consumo Medio de Alcohol por Comunidad Autónoma y Sexo", x = "Comunidad Autónoma", y = "Consumo Medio (unidades)") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "Grafica_consumo_por_comunidad_y_sexo.jpeg",
  plot = Grafica_consumo_por_comunidad_y_sexo,
  path = "OUTPUT/Figures", 
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

Porcentajes_alcohol_fila<- consumo_alcohol10 %>%
  pivot_longer(data=.,names_to = "Porcentajes",values_to = "valor",cols= c(Porcentaje_consumo,Porcentaje_no_consumo))



##Analisis de datos ejercicio físico con gráficas

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


###Formateo de datos para correcto análisis
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


###MAPAS

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
    colors = hcl.colors(10, "Reds", rev = TRUE),
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
    colors = hcl.colors(10, "Purples", rev = TRUE),
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

#Relaciones
##Relación suicidio con consumo de alcohol
suicidio_global_mujeres <- suicidio_global %>%
  left_join(suicidio_mujeres, by = "comunidades_autonomas")

suicidio_final <- suicidio_global_mujeres %>%
  left_join(suicidio_hombres, by = "comunidades_autonomas")

suicidio_alcohol <- suicidio_final %>%
  left_join(consumo_global, by = c("comunidades_autonomas" = "Comunidades_autonomas"))

suicidio_alcohol2 <- suicidio_alcohol %>%
  left_join(consumo_mujeres, by = c("comunidades_autonomas" = "Comunidades_autonomas"))

suicidio_alcohol3 <- suicidio_alcohol2 %>%
  left_join(consumo_hombres, by = c("comunidades_autonomas" = "Comunidades_autonomas"))

 no_consumo_global <- consumo_alcohol10 %>%
   filter(Sexo == "Ambos sexos") %>%
   group_by(Comunidades_autonomas)%>%
   summarize(Porcentaje_global_no_consumo = mean(Porcentaje_no_consumo, na.rm = TRUE))
 
 no_consumo_mujeres <- consumo_alcohol10 %>%
   filter(Sexo == "Mujeres") %>%
   group_by(Comunidades_autonomas) %>%
   summarize(Porcentaje_mujeres_no_consumo = mean(Porcentaje_no_consumo, na.rm = TRUE))
 
 no_consumo_hombres <- consumo_alcohol10 %>%
   filter(Sexo == "Hombres") %>%
   group_by(Comunidades_autonomas)%>%
   summarize(Porcentaje_hombres_no_consumo = mean(Porcentaje_no_consumo, na.rm = TRUE))

suicidio_alcohol4 <- suicidio_alcohol3 %>%
  left_join(no_consumo_global, by = c("comunidades_autonomas" = "Comunidades_autonomas"))

suicidio_alcohol5 <- suicidio_alcohol4 %>%
  left_join(no_consumo_mujeres, by = c("comunidades_autonomas" = "Comunidades_autonomas"))

suicidio_alcohol_final <- suicidio_alcohol5 %>%
  left_join(no_consumo_hombres, by = c("comunidades_autonomas" = "Comunidades_autonomas"))


suicidio_alcohol_final_2 <- suicidio_alcohol_final %>%
  pivot_longer(cols = c(porcentaje_global_suicidios, Porcentaje_global_consumo),
               names_to = "tipo_de_porcentaje",
               values_to = "porcentaje")

grafica_suicidio_alcohol_global_barras <- 
  ggplot(suicidio_alcohol_final_2, aes(x = comunidades_autonomas, y = porcentaje))+
  geom_bar(aes(fill = tipo_de_porcentaje), stat = "identity", position = "dodge")+
  facet_wrap(~ tipo_de_porcentaje, scales = "free_y")+
  labs(title = "Relación entre suicidios y consumo de alcohol",
       x = "Porcentajes",
       y = "Comunidades autónomas")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))

grafica_suicidio_alcohol_global_barras

grafica_suicidio_alcohol_global_puntos <- 
  ggplot(suicidio_alcohol_final, aes(x = porcentaje_global_suicidios, y = Porcentaje_global_consumo))+
  geom_point(aes(color = comunidades_autonomas), size = 3, alpha = 0.7)+
  geom_smooth(method = "lm", se = TRUE, color = "blue")+
  labs(
    title = "Relación entre suicidios y consumo de alcohol",
    x = "Porcentaje_global_suicidios",
    y = "Porcentaje_global_consumo"
  )+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))

ggplotly(grafica_suicidio_alcohol_global_puntos)


suicidio_alcohol_final_3 <- suicidio_alcohol_final %>%
  pivot_longer(cols = c(porcentaje_mujeres_suicidios, Porcentaje_mujeres_consumo),
               names_to = "tipo_de_porcentaje",
               values_to = "porcentaje")

grafica_suicidio_alcohol_mujeres_barras <- 
  ggplot(suicidio_alcohol_final_3, aes(x = comunidades_autonomas, y = porcentaje))+
  geom_bar(aes(fill = tipo_de_porcentaje), stat = "identity", position = "dodge")+
  facet_wrap(~ tipo_de_porcentaje, scales = "free_y")+
  labs(title = "Relación entre suicidios y consumo de alcohol en mujeres",
       x = "Porcentajes",
       y = "Comunidades autónomas")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))

grafica_suicidio_alcohol_mujeres_barras

grafica_suicidio_alcohol_mujeres_puntos <- 
  ggplot(suicidio_alcohol_final, aes(x = porcentaje_mujeres_suicidios, y = Porcentaje_mujeres_consumo))+
  geom_point(aes(color = comunidades_autonomas), size = 3, alpha = 0.7)+
  geom_smooth(method = "lm", se = TRUE, color = "blue")+
  labs(
    title = "Relación entre suicidios y consumo de alcohol en mujeres",
    x = "Porcentaje_global_suicidios",
    y = "Porcentaje_global_consumo"
  )+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))

ggplotly(grafica_suicidio_alcohol_mujeres_puntos)

suicidio_alcohol_final_4 <- suicidio_alcohol_final %>%
  pivot_longer(cols = c(porcentaje_hombres_suicidios, Porcentaje_hombres_consumo),
               names_to = "tipo_de_porcentaje",
               values_to = "porcentaje")

grafica_suicidio_alcohol_hombres_barras <- 
  ggplot(suicidio_alcohol_final_4, aes(x = comunidades_autonomas, y = porcentaje))+
  geom_bar(aes(fill = tipo_de_porcentaje), stat = "identity", position = "dodge")+
  facet_wrap(~ tipo_de_porcentaje, scales = "free_y")+
  labs(title = "Relación entre suicidios y consumo de alcohol en hombres",
       x = "Porcentajes",
       y = "Comunidades autónomas")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))

grafica_suicidio_alcohol_hombres_barras

grafica_suicidio_alcohol_hombres_puntos <- 
  ggplot(suicidio_alcohol_final, aes(x = porcentaje_hombres_suicidios, y = Porcentaje_hombres_consumo))+
  geom_point(aes(color = comunidades_autonomas), size = 3, alpha = 0.7)+
  geom_smooth(method = "lm", se = TRUE, color = "blue")+
  labs(
    title = "Relación entre suicidios y consumo de alcohol en hombres",
    x = "Porcentaje_global_suicidios",
    y = "Porcentaje_global_consumo"
  )+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))

ggplotly(grafica_suicidio_alcohol_hombres_puntos)


#Guardo los gráficos

ggsave(
  filename = "Suicidio_alcohol_global_barras.jpeg",
  plot = grafica_suicidio_alcohol_global_barras ,
  path = "OUTPUT/Figures", 
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Suicidio_alcohol_global_puntos.jpeg",
  plot = grafica_suicidio_alcohol_global_puntos ,
  path = "OUTPUT/Figures", 
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Suicidio_alcohol_mujeres_barras.jpeg",
  plot = grafica_suicidio_alcohol_mujeres_barras ,
  path = "OUTPUT/Figures",
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Suicidio_alcohol_mujeres_puntos.jpeg",
  plot = grafica_suicidio_alcohol_mujeres_puntos ,
  path = "OUTPUT/Figures",
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Suicidio_alcohol_hombres_barras.jpeg",
  plot = grafica_suicidio_alcohol_hombres_barras ,
  path = "OUTPUT/Figures", 
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Suicidio_alcohol_hombres_puntos.jpeg",
  plot = grafica_suicidio_alcohol_hombres_puntos ,
  path = "OUTPUT/Figures", 
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Suicidio_por_sexo_comunidad.jpeg",
  plot = graf_suicidio_por_sexo_comunidad ,
  path = "OUTPUT/Figures", 
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Suicidio_por_comunidad.jpeg",
  plot = graf_suicidio_por_comunidad ,
  path = "OUTPUT/Figures", 
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Suicidio_por_sexo.jpeg",
  plot = graf_suicidio_por_sexo ,
  path = "OUTPUT/Figures", 
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)





##Relación de ejercicio físico con consumo de alcohol

no_consumo_global <- consumo_alcohol10 %>%
  filter(Sexo == "Ambos sexos") %>%
  group_by(Comunidades_autonomas)%>%
  summarize(Porcentaje_global_no_consumo = mean(Porcentaje_no_consumo, na.rm = TRUE))

no_consumo_mujeres <- consumo_alcohol10 %>%
  filter(Sexo == "Mujeres") %>%
  group_by(Comunidades_autonomas) %>%
  summarize(Porcentaje_mujeres_no_consumo = mean(Porcentaje_no_consumo, na.rm = TRUE))

no_consumo_hombres <- consumo_alcohol10 %>%
  filter(Sexo == "Hombres") %>%
  group_by(Comunidades_autonomas)%>%
  summarize(Porcentaje_hombres_no_consumo = mean(Porcentaje_no_consumo, na.rm = TRUE))

frecuenciaNadaYMaxEjercicioComunidad2<-frecuenciaNadaYMaxEjercicioComunidad %>%
  pivot_wider(names_from =Frecuencia_de_ejercicio,values_from = Porcentaje)


comparacion_datos <- full_join(x=consumo_alcohol10,y=frecuenciaNadaYMaxEjercicioComunidad2,
                               by = c("Comunidades_autonomas", "Sexo"))


#comparacion consumo con no ejercicio

comparacion_cons_no_ej_mujeres<-consumo_global %>%
  left_join(ejercicioMujeresNinguno ,by = "Comunidades_autonomas")



#comparacion no consumo con ejercicio
comparacion_no_cons_ej_mujeres<- no_consumo_global  %>%
  left_join(ejercicioMujeresEjercicio ,by = "Comunidades_autonomas")



comparacion_cons_no_ej_hombres<-consumo_global %>%
  left_join(ejercicioHombresNinguno,by = "Comunidades_autonomas")


comparacion_no_cons_ej_hombres<- no_consumo_global  %>%
  left_join(ejercicioHombresEjercicio,by = "Comunidades_autonomas")



comparacion_cons_no_ej_ambos_sexos<-consumo_global %>%
  left_join(ejercicioAmbosSexosNinguno,by = "Comunidades_autonomas")


consumo_alcohol_no_ej_ambos_sexos_largo <- comparacion_cons_no_ej_ambos_sexos %>%
  pivot_longer(
    cols = c(Porcentaje_global_consumo,Ninguno), 
    names_to = "Tipo_porcentaje",                 
    values_to = "valor"                              
  )
grafica_consumo_no_ej_ambos_sexos <- 
  ggplot(consumo_alcohol_no_ej_ambos_sexos_largo, aes(x = Comunidades_autonomas, y = valor))+
  geom_bar(aes(fill = Tipo_porcentaje), stat = "identity", position = "dodge")+
  facet_wrap(~ Tipo_porcentaje, scales = "free_y")+
  labs(title = "Relación entre ejercicio fisico y consumo de alcohol en ambos sexos",
       x = "Porcentajes",
       y = "Comunidades autónomas")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))

ggsave(
  filename = "grafica_consumo_no_ej_ambos_sexos.jpeg",
  plot = grafica_consumo_no_ej_ambos_sexos ,
  path = "OUTPUT/Figures", 
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)


comparacion_no_cons_ej_ambos_sexos<- no_consumo_global  %>%
  left_join(ejercicioAmbosSexosEjercicio,by = "Comunidades_autonomas")

comparacion_no_cons_ej_ambos_sexos<-comparacion_no_cons_ej_ambos_sexos%>%
  rename(Porcentaje_ejercicio_7_dias = Porcentaje)



consumo_no_alcohol_ej_ambos_sexos_largo <-comparacion_no_cons_ej_ambos_sexos %>%
  pivot_longer(
    cols = c(Porcentaje_global_no_consumo, Porcentaje_ejercicio_7_dias ), 
    names_to = "Tipo_porcentaje",                 
    values_to = "valor"                              
  )

grafica_no_consumo_ej_ambos_sexos <- 
  ggplot(consumo_no_alcohol_ej_ambos_sexos_largo , aes(x = Comunidades_autonomas, y = valor))+
  geom_bar(aes(fill = Tipo_porcentaje), stat = "identity", position = "dodge")+
  facet_wrap(~ Tipo_porcentaje, scales = "free_y")+
  labs(title = "Relación entre ejercicio y consumo de alcohol en ambos sexos",
       x = "Porcentajes",
       y = "Comunidades autónomas")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))

ggsave(
  filename = "ggrafica_no_consumo_ej_ambos_sexos.jpeg",
  plot = grafica_no_consumo_ej_ambos_sexos ,
  path = "OUTPUT/Figures", 
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)


Grafica_ejercicio_no_consumo_puntos<-ggplot(comparacion_datos, aes(x = `7 días a la semana`, y = Porcentaje_no_consumo)) +
  geom_point(aes(color = Sexo), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Relación entre Porcentaje de no Consumo de Alcohol y Ejercicio Físico (7 días a la semana)",
    x = "Porcentaje de Ejercicio (7 días a la semana)",
    y = "Porcentaje de  no Consumo de Alcohol",
    color = "Sexo"
  ) 



Grafica_no_ejercicio_consumo_puntos<-ggplot(comparacion_datos, aes(x = Ninguno, y = Porcentaje_consumo)) +
  geom_point(aes(color = Sexo), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Relación entre Porcentaje de Consumo de Alcohol y Falta de Ejercicio Físico (Ninguno)",
    x = "Porcentaje de Falta de Ejercicio (Ninguno)",
    y = "Porcentaje de Consumo de Alcohol",
    color = "Sexo"
  )

ggsave(
  filename = "Grafica ejercicio y no consumo puntos.jpeg",
  plot = Grafica_ejercicio_no_consumo_puntos ,
  path = "OUTPUT/Figures", 
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Grafica no ejercicio y consumo puntos.jpeg",
  plot = Grafica_no_ejercicio_consumo_puntos,
  path = "OUTPUT/Figures", 
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

##Relación de ejercicio físico con suicidios

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
       x = "Porcentaje de individuos medio",
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
  geom_point(size = 3) +
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
