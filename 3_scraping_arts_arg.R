#Testeamos con un conjunto de urls de prueba para asegurarnos que el código reacciona bien
#en ambos casos propuestos.
urls_prueba <- md_revistas$link[c(1, 2, 3, 37574, 37575)]

library(tidyverse)
library(rvest)
library(polite)
library (openxlsx)
library (stringi)
library (stringr)

#Scraping artículos

md_revistas_arg <- md_revistas %>% 
  filter(pais != "Chile" & pais != "México")

urls_arg <- md_revistas %>% 
  filter(pais != "Chile" & pais != "México") %>% 
  pull(link)

# Inicializar data frame para almacenar los resultados
articulos <- data.frame(
  url = character(),
  contenido = character(),
  solo_texto_articulo = logical(),  # Nueva columna lógica
  consulta = as.POSIXct(character()),
  stringsAsFactors = FALSE
)
errores <- c()

# Iterar sobre los enlaces
for (i in urls_arg[1259:6329]) {
  tryCatch({
    # Intentar extraer el texto del artículo utilizando el identificador XPath
    articulo <- read_html(i) %>% 
      html_elements(xpath = '//*[@id="article-body"]') %>% 
      html_text2()
    
    # Verificar si el artículo fue extraído solo del texto o de todo el body
    if (length(articulo) == 0) {
      # Extraer todo el <body> si el XPath del artículo no devuelve texto
      articulo <- read_html(i) %>% 
        html_elements("body") %>% 
        html_text2()
      es_solo_texto <- FALSE  # Indica que se extrajo todo el body
    } else {
      es_solo_texto <- TRUE  # Indica que se extrajo solo el texto del artículo
    }
    time_consulta <- Sys.time()
    # Agregar el artículo al data frame con la columna lógica
    articulos <- rbind(articulos, data.frame(url = i, contenido = articulo,
                                             solo_texto_articulo = es_solo_texto,
                                             consulta = time_consulta))
    
    # Pausar para evitar sobrecarga al servidor
    Sys.sleep(0.5)
  }, error = function(e) {
    # En caso de error, guardar el enlace y el mensaje del error
    print(paste0("Link caído: ", i, " - ", e$message))
    errores <<- append(errores, i)
  })
}

# REVISIÓN Y CONTROL DE ERRORES
#El scraping se realizó en dos corridas: una el 18/11 y otra el 20/11. Durante la primera
#los errores no se alojaron en un objeto, por lo que tuve que tomarlos de la consola para
#pasarlos a un excel.
#Para realizar la revisión y control de errores propongo crear un data frame que contenga
#una columna con los errores y otra con el link que ha arrojado error (se podría sumar una
#tercera que contenga el tipo de error).
#Luego, volver a pasar el código de scraping para poder identificar si es que el problema
#con dichos enlaces fue circunstancial o es que efectivamente se encuentran caídos.

errores <- dplyr::data_frame(error=errores)

errores <- errores %>% 
  mutate(urls_caidos = str_extract(error, "http://.*?lang=es"),
         fecha_scraping = as_date("2024-11-20"))

errores <- errores %>% 
  bind_rows(errores_join)

urls_arg <- dplyr::data_frame(url = urls_arg, ID = paste("urls_arg", seq_along(along.with = urls_arg),
                                                         sep = "-"))

errores <- errores %>% 
  left_join(urls_arg, by = join_by(urls_caidos == url))

# Inicializar data frame para almacenar los resultados
articulos_caidos <- data.frame(
  url = character(),
  contenido = character(),
  solo_texto_articulo = logical(),  # Nueva columna lógica
  consulta = as.POSIXct(character()),
  stringsAsFactors = FALSE
)
errores_2 <- c()

# Iterar sobre los enlaces
for (i in errores$urls_caidos) {
  tryCatch({
    # Intentar extraer el texto del artículo utilizando el identificador XPath
    articulo <- read_html(i) %>% 
      html_elements(xpath = '//*[@id="article-body"]') %>% 
      html_text2()
    
    # Verificar si el artículo fue extraído solo del texto o de todo el body
    if (length(articulo) == 0) {
      # Extraer todo el <body> si el XPath del artículo no devuelve texto
      articulo <- read_html(i) %>% 
        html_elements("body") %>% 
        html_text2()
      es_solo_texto <- FALSE  # Indica que se extrajo todo el body
    } else {
      es_solo_texto <- TRUE  # Indica que se extrajo solo el texto del artículo
    }
    time_consulta <- Sys.time()
    # Agregar el artículo al data frame con la columna lógica
    articulos_caidos <- rbind(articulos_caidos, data.frame(url = i, contenido = articulo,
                                             solo_texto_articulo = es_solo_texto,
                                             consulta = time_consulta))
    
    # Pausar para evitar sobrecarga al servidor
    Sys.sleep(0.5)
  }, error = function(e) {
    # En caso de error, guardar el enlace y el mensaje del error
    print(paste0("Link caído: ", i, " - ", e$message))
    errores_2 <<- append(errores_2, i)
  })
}

articulos <- articulos %>% 
  bind_rows(articulos_caidos)

errores_2 <- errores_2 %>% 
  rename(error = ...1) %>% 
  mutate(urls_caidos = str_extract(error, "http://.*?lang=es"),
         fecha_scraping = as_date(today()))

#DESCRIPCIÓN DE LOS OBJETOS RESULTADOS
#artículos -> contiene la totalidad de los artículos scrapeados de scielo argentina
#(url, contenido, fecha de consulta y forma en que se ha obtenido el contenido)
#articulos_caidos -> igual estructura que el anterior, pero contiene solamente los
#artículos que fueron obtenidos a partir de pasar el scraping por segunda vez
#errores_join -> errores resultantes del primer scraping (18/11)
#errores -> errores totales resultantes de los dos días de scraping (18/11 y 20/11)
#errores_2 -> errores resultantes de pasar el scraping por segunda vez
#md_revistas_arg -> df con metadatos de los artículos scrapeados del buscador de scielo, solo arg
#md_revistas -> igual que el anterior, pero para mx, ch y arg


# EXPLORACIÓN Y LIMPIEZA DE LA BASE ----

duplicados_logico <- duplicated(articulos)
which(duplicados_logico==T)

articulos2 <- articulos %>% 
  distinct()


summary(articulos)

#Cálculo del tiempo de scrapign
articulos %>% 
  group_by()