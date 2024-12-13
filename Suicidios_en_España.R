install.packages("mapSpain", dependencies = TRUE)
install.packages("sf")
install.packages("patchwork")
install.packages("DT")
install.packages("plotly")
library(dplyr)
library(tidyverse)
library(rjson)
library(tidyjson)
library(ggplot2)
library(mapSpain)
library(sf)
library(patchwork)
library(DT)
library(plotly)

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

# Relacion con consumo de alcohol
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

# no_consumo_global <- consumo_alcohol10 %>%
#   filter(Sexo == "Ambos sexos") %>%
#   group_by(Comunidades_autonomas)%>%
#   summarize(Porcentaje_global_no_consumo = mean(Porcentaje_no_consumo, na.rm = TRUE))
# 
# no_consumo_mujeres <- consumo_alcohol10 %>%
#   filter(Sexo == "Mujeres") %>%
#   group_by(Comunidades_autonomas) %>%
#   summarize(Porcentaje_mujeres_no_consumo = mean(Porcentaje_no_consumo, na.rm = TRUE))
# 
# no_consumo_hombres <- consumo_alcohol10 %>%
#   filter(Sexo == "Hombres") %>%
#   group_by(Comunidades_autonomas)%>%
#   summarize(Porcentaje_hombres_no_consumo = mean(Porcentaje_no_consumo, na.rm = TRUE))

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


