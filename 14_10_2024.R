library(dplyr)
library(tidyverse)
library(rjson)
library(tidyjson)
library(ggplot2)
library(mapSpain)
library(sf)
library(patchwork)


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


consumo_alcohol10$Comunidades_autonomas <- consumo_alcohol10$Comunidades_autonomas[seq(1, length(consumo_alcohol10$Comunidades_autonomas), by =  4)]
consumo_alcohol10$Sexo <- consumo_alcohol10$Sexo[seq(1, length(consumo_alcohol10$Sexo), by =  4)]
view(consumo_alcohol10)

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
  labs(title = "Consumo Medio de Alcohol por Sexo", x = "Sexo", y = "Consumo Medio (%)") 


ggplot(consumo_por_comunidad, aes(x = reorder(Comunidades_autonomas, consumo_medio_comunidad), y = consumo_medio_comunidad, fill = Comunidades_autonomas)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  labs(title = "Consumo Medio de Alcohol por Comunidad Autónoma", x = "Comunidad Autónoma", y = "Porcentaje Medio de Consumo") +theme_minimal() +theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(consumo_por_sexo_comunidad, aes(x = reorder(Comunidades_autonomas, consumo_medio_sexo_comunidad), y = consumo_medio_sexo_comunidad, fill = Sexo)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  labs(title = "Consumo Medio de Alcohol por Comunidad Autónoma y Sexo", x = "Comunidad Autónoma", y = "Consumo Medio (unidades)") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))


Porcentajes_alcohol_fila<- consumo_alcohol10 %>%
  pivot_longer(data=.,names_to = "Porcentajes",values_to = "valor",cols= c(Porcentaje_consumo,Porcentaje_no_consumo))

#view(Porcentajes_alcohol_fila)

#Revisar bien


 ggplot(Porcentajes_alcohol_fila, aes(Comunidades_autonomas,valor))+
  geom_point(aes(colour=factor(Sexo), shape = factor(Porcentajes)))+
  geom_smooth(method = "lm", se=TRUE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 

 #PREGUNTA NÚMERO 3 

ExtremosUnionFinal2<-ExtremosUnionFinal %>%
  pivot_wider(names_from =FrecuenciasExtremo,values_from = Porcentaje)

View(ExtremosUnionFinal2)

comparacion_datos <- full_join(x=consumo_alcohol10,y=ExtremosUnionFinal2,
                               by = c("Comunidades_autonomas", "Sexo"))

#view(comparacion_datos)
comparacion_datos

comparacion_datos2<-full_join(x=Porcentajes_alcohol_fila,y=ExtremosUnionFinal,
                              by = c("Comunidades_autonomas", "Sexo"))

comparacion_datos2

#view(comparacion_datos2)

comparacion_cons_ej_mujeres<-consumo_global %>%
  left_join(ejercicioMujeres,by = "Comunidades_autonomas")

comparacion_cons_ej_mujeres

comparacion_cons_ej_hombres<-consumo_global %>%
  left_join(ejercicioHombres,by = "Comunidades_autonomas")

comparacion_cons_ej_hombres

comparacion_nocons_ej_mujeres<-no_consumo_mujeres %>%
  left_join(ejercicioMujeres, by = "Comunidades_autonomas")

comparacion_nocons_ej_mujeres

comparacion_nocons_ej_hombres<-no_consumo_hombres %>%
  left_join(ejercicioHombres, by = "Comunidades_autonomas")

comparacion_nocons_ej_hombres


comparacion_porcentajes <- consumo_global  %>%
  left_join(porcentaje_realizacion_ejercicio_por_comunidad,by =c("Comunidades_autonomas" = "comunidades_autonomas") )

comparacion_porcentajes 

comparacion_no_porcentajes <- no_consumo_global %>%
  left_join(nada_ejercicio_por_comunidad,by =c("Comunidades_autonomas" = "comunidades_autonomas") )
  
comparacion_no_porcentajes

ggplot(comparacion_datos, aes(x = `7 días a la semana`, y = Porcentaje_consumo)) +
  geom_point(aes(color = Sexo), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Relación entre Porcentaje de Consumo de Alcohol y Ejercicio Físico (7 días a la semana)",
    x = "Porcentaje de Ejercicio (7 días a la semana)",
    y = "Porcentaje de Consumo de Alcohol",
    color = "Sexo"
  ) 



ggplot(comparacion_datos, aes(x = Ninguno, y = Porcentaje_no_consumo)) +
  geom_point(aes(color = Sexo), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Relación entre Porcentaje de No Consumo de Alcohol y Falta de Ejercicio Físico (Ninguno)",
    x = "Porcentaje de Falta de Ejercicio (Ninguno)",
    y = "Porcentaje de No Consumo de Alcohol",
    color = "Sexo"
  )



comparacion_porcentajes_alargado <- comparacion_porcentajes %>%
  select(Comunidades_autonomas, Porcentaje_global_consumo, ejercicio_medio_comunidad) %>%
  pivot_longer(
    cols = c(Porcentaje_global_consumo, ejercicio_medio_comunidad),
    names_to = "Tipo",
    values_to = "Porcentaje"
  )

comparacion_porcentajes_alargado

#mirar valores NA

ggplot(comparacion_porcentajes_alargado, aes(
  x = reorder(Comunidades_autonomas, Porcentaje),
  y = Porcentaje,
  fill = Tipo
)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(
    title = "Comparación entre Consumo de Alcohol y Realización de Ejercicio",
    x = "Comunidades Autónomas",
    y = "Porcentaje",
    fill = "Indicador"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

  




