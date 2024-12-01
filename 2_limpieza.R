# CONFORMACIÓN DE LA BASE DE METADATOS

# LIBRERIAS ----

library (rvest)
library (tidyverse)
library (openxlsx)
library (stringi)
library (stringr)

#PREPARACIÓN DE LA BASE PARA LA REALIZACIÓN DEL SCRAPING SOBRE EL CONTENIDO DE LOS ARTÍCULOS

# Armamos la base sobre 38102 observaciones
md_revistas <- data.frame(titulo, autores, revista, link)


# Manejo y limpieza de los metadatos
md_revistas <- md_revistas %>% 
  mutate(pais = case_when(stri_detect_fixed(link, 'http://www.scielo.org.mx/') == TRUE ~ 'México',
                          stri_detect_fixed(link, 'http://www.scielo.org.ar/') == TRUE ~ 'Argentina',
                          stri_detect_fixed(link, 'http://www.scielo.cl/') == TRUE ~ 'Chile'))

#Revisamos cantidad de artículos por país
md_revistas %>%
  group_by(pais) %>% 
  summarise(freq = n())


#EXPLORAMOS LA EXISTENCIA DE OBSERVACIONES DUPLICADAS
clean_duplicados <- which(duplicated(md_revistas$link) == TRUE)
# Crear un vector lógico para todas las filas duplicadas (incluyendo primeras apariciones)
duplicados_logico <- duplicated(md_revistas) | duplicated(md_revistas, fromLast = TRUE)
# Crear el data frame con solo las filas duplicadas, incluyendo la primera ocurrencia
duplicados <- md_revistas[duplicados_logico, ]
# Añadir columna que indique si la fila es la primera ocurrencia o un duplicado
duplicados$Tipo <- ifelse(duplicated(md_revistas[duplicados_logico, ]), "Duplicado", "Primera aparición")

#LIMPIAMOS LOS DUPLICADOS
md_revistas <- md_revistas %>% 
  distinct()



# CONTROL SOBRE LA COLUMNA REVISTA 
md_revistas$revista <- stri_replace_first_fixed(md_revistas$revista,
                                                "Métricas del periódico Sobre o periódico SciELO Analytics", ";")


md_revistas <- md_revistas %>% 
  separate_wider_delim(revista, delim = ";", names = c("revista", "fecha/vol/num"), too_few = "debug", too_many = "debug")

revision <- md_revistas[which(md_revistas$revista_ok == F), ]

revision_arg <- revision %>% 
  filter(pais == "Argentina")

#Existen 285 entradas que no se corresponden exáctamente con artíclos. En general,
#son notas editoriales o de algún tipo similar. Al evaluar los 65 casos que corresponden
#a revistas argentinas, he identificado que muchos de ellos pertenecen a la
#"Revista Iberoamericana de Tecnología en Educación y Educación en Tecnología",
#por este motivo he decidido conservar los mismos, a pesar de que sus metadatos
#tienen una estructura diferente.

#RECORTO LA BASE CON LOS METADATOS DE REVISTAR ARGENTINAS
md_revistas_arg <- md_revistas %>% 
  filter(pais == "Argentina")

write.csv(md_revistas_arg, "md_revistas_arg.csv", row.names = FALSE)
