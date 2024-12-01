#ANÁLISIS PRELIMINAR
library(readr)
library(tidyverse)
library(dplyr)
library(stringr)
articulos_scielo <- read_csv("C:/Users/Chia/Desktop/Martín/Martín/UNCuyo/SIIP - Big Data/Beca SIIP/revistas/articulos_scielo.csv")
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
  bd_revistas_arg[[paste0("menciones_", gsub(" ", "_", palabra))]] <- str_count(bd_revistas_arg$contenido, palabra)
}

# Crear una columna con la sumatoria de las menciones del diccionario en cada nota
bd_revistas_arg <- bd_revistas_arg %>%
  rowwise() %>%
  mutate(suma_menciones = sum(c_across(starts_with("menciones_")))) %>%
  ungroup()


# Crear una columna para cada key_word
for (palabra in key_words) {
  bd_revistas_arg[[paste0("key", gsub(" ", "_", palabra))]] <- str_count(bd_revistas_arg$contenido, palabra)
}

# Crear una columna con la sumatoria de las menciones del diccionario en cada nota
bd_revistas_arg <- bd_revistas_arg %>%
  rowwise() %>%
  mutate(suma_key_words = sum(c_across(starts_with("key")))) %>%
  ungroup()


#EXPLORACION DE FRECUENCIA DE MENCIONES

tabla_key_words <- bd_revistas_arg %>% 
  count(suma_key_words, sort = T)

tabla_dicc <- bd_revistas_arg %>% 
  count(suma_menciones, sort = T)


#CONFORMACION DE SUBSET

art_relevantes <- bd_revistas_arg %>% 
  filter(suma_menciones != 0)

art_clave <- bd_revistas_arg %>% 
  filter(suma_key_words != 0)
