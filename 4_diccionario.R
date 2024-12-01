#CONSTRUCCIÓN DE DICCIONARIO----
library(pdftools)
library(dplyr)
library(tibble)

# Obtener la lista de archivos PDF en la carpeta
carpeta <- "bibliografia"
archivos_pdf <- list.files(carpeta, pattern = "\\.pdf$", full.names = TRUE)

# Crear una función para leer texto de un PDF
leer_pdf <- function(archivo) {
  texto <- pdf_text(archivo)  # Extrae el texto de cada página
  texto_completo <- paste(texto, collapse = " ")  # Combina todas las páginas
  return(texto_completo)
}

# Extraer el texto de cada PDF y guardar en un data frame
articulos <- tibble(
  archivo = archivos_pdf,  # Nombre del archivo PDF
  texto = sapply(archivos_pdf, leer_pdf)  # Texto completo del PDF
)

write.csv(articulos, "articulos.csv", row.names = FALSE)

#LIMPIEZA DE LOS TEXTOS
library(tidytext)
#library(dplyr)
library(stringr)

#El artículo 4 está vacío. El motivo es que era un texto escaneado y no he utilizado
#el enfoque necesario para recuperar ese tipo de textos. Lo remuevo.
#articulos <- articulos[-c(4), ]

# Cargar stopwords en español
stopwords_es <- get_stopwords(language = "es")

# Limpieza del texto
articulos_limpios <- articulos %>%
  mutate(
    texto_limpio = texto %>%
      str_replace_all("[^\\w\\s]", "") %>%  # 1) Eliminar caracteres especiales
      str_replace_all("\\d+", "") %>%      # 2) Eliminar números
      str_replace_all("\\s+", " ") %>%     # 3) Reemplazar múltiples espacios y saltos de línea por un espacio
      str_to_lower()                       # 5) Convertir todo a minúscula
  ) %>%
  rowwise() %>%
  mutate(
    texto_limpio = str_c(
      setdiff(unlist(str_split(texto_limpio, " ")), stopwords_es$word),
      collapse = " "
    )  # 4) Remover stopwords
  )

#BIGRAMAS
bigramas_full <- articulos_limpios %>% 
  unnest_tokens(input = texto_limpio,
                output = "bigrama",
                token = "ngrams", n = 2, drop = TRUE) %>% 
  count(bigrama, sort = TRUE)

bigramas_full <- bigramas_full %>% 
  mutate(orden_full = list(seq_along(bigramas_full$bigrama)))%>% 
  rename(frec_full = n)

#PALABRAS TOP
tokens <- articulos_limpios %>%
  unnest_tokens(word, texto_limpio)  # 'word' será la nueva columna con palabras

# Ver las palabras más frecuentes
frecuencias <- tokens %>%
  count(word, sort = TRUE)


#CONFORMACIÓN MANUAL DE DICCIONARIO
#Elaboración semi-artesanal de un primer diccionario. El mismo contiene palabras
#vinculadas con la temática de estudio y, en su mayoría, relevantes dentro de
#los textos utilizados para calcular bigramas y topwords.

key_words <- c("computacionales", "humanidades digitales")
dicc_scc <- c("mineria texto", "text mining", "topic modeling", "modelado topicos",
              "r project", "big data", "datos masivos", "lectura distante",
              "distant reading", "lenguaje natural", "natural lenguage",
              "lenguage processing", "lenguajes programacion", "machine learning",
              "scrape", "scraping", "data analysis", "analisis datos")

