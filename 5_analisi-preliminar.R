#ANÁLISIS PRELIMINAR
library(readr)
library(tidyverse)
library(dplyr)
library(stringr)
articulos_scielo <- read_csv("articulos_scielo.csv")
md_revistas_arg <- read.csv("md_revistas_arg.csv")

bd_revistas_arg <- articulos_scielo %>% 
  left_join(md_revistas_arg, by = join_by(url == link))

#Al realizar el join al revés hay una inconsistencia que no termino de comprendet.
#Parece realizar un join many-to-many, aunque no lo advierte, dado que genera una
#cantidad de observaciones mayor a la de ambas bases de origen.
bd_revistas_arg2 <- md_revistas_arg %>% 
  left_join(articulos_scielo, by = join_by(link == url))
rm(bd_revistas_arg2)


#NORMALIZACIÓN Y LIMPIEZA DE LOS ARTÍCULOS
library(ACEP)
library(tm)
library(tidytext)
library(stringr)

stopwords_sugeridas <- c(unlist(get_stopwords(language = "es")), "named anchor")

bd_revistas_arg$contenido <- bd_revistas_arg$contenido %>%
  acep_cleaning()%>%
  removeWords(stopwords_sugeridas)%>%
  stripWhitespace()

write.csv(bd_revistas_arg, "bd_rev_arg_clean.csv", row.names = F)

#CALCULO DE MENCIONES

bd_clean <- read.csv("bd_rev_arg_clean.csv")

#Diccionarios
key_words <- c("ciencias computacionales", "sociales computacionales", "humanidades digitales",
               "metodologias computacionales", "metodologia computacional", "acercamiento computacional",
               "acercamientos computacionales")

diccionario <- c("mineria texto", "text mining", "topic modeling", "modelado topicos",
                 "r project", "big data", "datos masivos", "lectura distante", "distant reading",
                 "lenguaje natural", "natural lenguage", "lenguage processing",
                 "lenguaje programacion", "lenguajes programacion", "machine learning",
                 "scrape", "scraping", "data analysis")

# Crear una columna para cada término del diccionario
for (palabra in diccionario) {
  bd_clean[[paste0("menciones_", gsub(" ", "_", palabra))]] <- str_count(bd_clean$contenido, palabra)
}

# Crear una columna con la sumatoria de las menciones del diccionario en cada nota
bd_clean <- bd_clean %>%
  rowwise() %>%
  mutate(suma_menciones = sum(c_across(starts_with("menciones_")))) %>%
  ungroup()


# Crear una columna para cada key_word
for (palabra in key_words) {
  bd_clean[[paste0("key", gsub(" ", "_", palabra))]] <- str_count(bd_clean$contenido, palabra)
}

# Crear una columna con la sumatoria de las menciones del diccionario en cada nota
bd_clean <- bd_clean %>%
  rowwise() %>%
  mutate(suma_key_words = sum(c_across(starts_with("key")))) %>%
  ungroup()

bd_clean <- bd_clean %>% 
  mutate(fecha = case_when(str_detect(fecha.vol.num, "2024") == T ~ 2024,
                           str_detect(fecha.vol.num, "2023") == T ~ 2023,
                           str_detect(fecha.vol.num, "2022") == T ~ 2022,
                           str_detect(fecha.vol.num, "2021") == T ~ 2021,
                           str_detect(fecha.vol.num, "2020") == T ~ 2020,
                           str_detect(fecha.vol.num, "2019") == T ~ 2019,
                           str_detect(fecha.vol.num, "2018") == T ~ 2018,
                           str_detect(fecha.vol.num, "2017") == T ~ 2017,
                           str_detect(fecha.vol.num, "2016") == T ~ 2016,
                           str_detect(fecha.vol.num, "2015") == T ~ 2015))

tabla_date <- bd_clean %>%
  count(fecha, sort = T)

tabla_porc <- bd_clean %>% 
  group_by(fecha) %>% 
  summarise(total_obs=n(),
            key_no_cero = sum(suma_key_words != 0, na.rm = T),
            menciones_no_cero = sum(suma_menciones != 0, na.rm = T),
            porc_key = sum(suma_key_words != 0, na.rm = T) / total_obs * 100,
            porc_menc = sum(suma_menciones != 0, na.rm = T) / total_obs * 100)

library(ggplot2)
ggplot(tabla_porc,
       aes(x = fecha, y = menciones_no_cero))+
  geom_col()+
  geom_line(aes(x = fecha, y = porc_menc))

ggplot(tabla_porc,
       aes(x = fecha, y = log10(total_obs)))+
  geom_col()+
  geom_line(aes(x = fecha, y = log10(porc_menc)))

#EXPLORACION DE FRECUENCIA DE MENCIONES

tabla_key_words <- bd_clean %>% 
  count(suma_key_words, sort = T)

tabla_dicc <- bd_clean %>% 
  count(suma_menciones, sort = T)


#CONFORMACION DE SUBSET

art_relevantes <- bd_clean %>% 
  filter(suma_menciones != 0)

art_clave <- bd_clean %>% 
  filter(suma_key_words != 0)

#DISPERSION TEMPORAL

tabla_date <- fechas %>%
  count(fecha, sort = T)

library(ggplot2)
ggplot(fechas,
       aes(x = fecha))+
  geom_bar()
