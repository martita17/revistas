# El presente código busca sentar las bases para la construcción de un bot que
# nos permita realizar búsquedas de la forma lo más automática posible sobre
# la web de scielo. El objetivo detrás de esta automatización es facilitar la
# realización de scraping sobre el sitio, dado que el sistema de su buscador
# resulta, cuanto menos, incómodo.
# En una segunda instancia, intentaremos volcar los resultados a una función.
# En lo personal espero que el código me permita avanzar sobre el scraping de artículos
# de Argentina, Chile y México, de las Área Temáticas "Ciencias Humanas" y "Ciencias
# Sociales Aplicadas", del período que va de 2015 a 2024.

# Cargamos librerías ----
library(tidyverse)
library(wdman)
library(RSelenium)

# Iniciamos la conexión usando Firefox ----

remote_driver_object <- rsDriver(browser = "firefox",
                                 chromever = NULL,
                                 verbose = F,
                                 port = 3066L)



# Creammos un acceso del lado del cliente
remDr <- remote_driver_object$client
# Navegamos hasta nuestro sitio de interés
remDr$navigate("https://www.scielo.org")
remDr$maxWindowSize()

busqueda_object <- remDr$findElement(using = 'link text', 'BÚSQUEDA AVANZADA')
busqueda_object$clickElement()

lenguage_es <- remDr$findElement(using = 'class name', 'lang-es')
lenguage_es$clickElement()

cookies_botton <- remDr$findElement(using = 'xpath', '/html/body/div[1]/a[2]')
cookies_botton$clickElement()

# PRIMER FILTRO - PAÍS ----

opciones_pais <- remDr$findElement(using = 'xpath', '//*[@id="openSelectItens_in"]')
remDr$executeScript("arguments[0].scrollIntoView(true);", list(opciones_pais))
opciones_pais$clickElement()

Sys.sleep(0.5)

mx_checkbox <- remDr$findElement(using = 'xpath', '/html/body/section/div/div/div[2]/div/div/form/div[1]/div[3]/ul/li/div[1]/label[.="México"]')
mx_checkbox$clickElement()

arg_checkbox <- remDr$findElement(using = 'xpath', '/html/body/section/div/div/div[2]/div/div/form/div[1]/div[3]/ul/li/div[1]/label[.="Argentina"]')
arg_checkbox$clickElement()

ch_checkbox <- remDr$findElement(using = 'xpath', '/html/body/section/div/div/div[2]/div/div/form/div[1]/div[3]/ul/li/div[1]/label[.="Chile"]')
ch_checkbox$clickElement()

aplicar_pais <- remDr$findElement(using = 'xpath', '/html/body/section/div/div/div[2]/div/div/form/div[2]/input')
aplicar_pais$clickElement()


# SEGUNDO FILTRO - AÑO ----
# Para que abra "OPCIONES" es necesario scrollear hasta el botón antes de hacer click

opciones_ano <- remDr$findElement(using = 'xpath', '//*[@id="openSelectItens_year_cluster"]')
remDr$executeScript("arguments[0].scrollIntoView(true);", list(opciones_ano))
opciones_ano$clickElement()
Sys.sleep(0.5)

checkbox_2024 <- remDr$findElement(using = 'xpath', '/html/body/section/div/div/div[2]/div/div/form/div[1]/div[3]/ul/li/div[1]/label[.="2024"]')
checkbox_2024$clickElement()

checkbox_2023 <- remDr$findElement(using = 'xpath', '//html/body/section/div/div/div[2]/div/div/form/div[1]/div[3]/ul/li/div[1]/label[.="2023"]')
checkbox_2023$clickElement()

checkbox_2022 <- remDr$findElement(using = 'xpath', '/html/body/section/div/div/div[2]/div/div/form/div[1]/div[3]/ul/li/div[1]/label[.="2022"]')
checkbox_2022$clickElement()

checkbox_2021 <- remDr$findElement(using = 'xpath', '/html/body/section/div/div/div[2]/div/div/form/div[1]/div[3]/ul/li/div[1]/label[.="2021"]')
checkbox_2021$clickElement()

checkbox_2020 <- remDr$findElement(using = 'xpath', '/html/body/section/div/div/div[2]/div/div/form/div[1]/div[3]/ul/li/div[1]/label[.="2020"]')
checkbox_2020$clickElement()

checkbox_2019 <- remDr$findElement(using = 'xpath', '/html/body/section/div/div/div[2]/div/div/form/div[1]/div[3]/ul/li/div[1]/label[.="2019"]')
checkbox_2019$clickElement()

checkbox_2018 <- remDr$findElement(using = 'xpath', '/html/body/section/div/div/div[2]/div/div/form/div[1]/div[3]/ul/li/div[1]/label[.="2018"]')
checkbox_2018$clickElement()

checkbox_2017 <- remDr$findElement(using = 'xpath', '/html/body/section/div/div/div[2]/div/div/form/div[1]/div[3]/ul/li/div[1]/label[.="2017"]')
checkbox_2017$clickElement()

checkbox_2016 <- remDr$findElement(using = 'xpath', '/html/body/section/div/div/div[2]/div/div/form/div[1]/div[3]/ul/li/div[1]/label[.="2016"]')
checkbox_2016$clickElement()

checkbox_2015 <- remDr$findElement(using = 'xpath', '/html/body/section/div/div/div[2]/div/div/form/div[1]/div[3]/ul/li/div[1]/label[.="2015"]')
checkbox_2015$clickElement()

aplicar_year <- remDr$findElement(using = 'xpath', '/html/body/section/div/div/div[2]/div/div/form/div[2]/input')
aplicar_year$clickElement()


# FILTRO ÁREA TEMÁTICA
opciones_area <- remDr$findElement(using = 'xpath', '//*[@id="openSelectItens_subject_area"]')
remDr$executeScript("arguments[0].scrollIntoView(true);", list(opciones_area))
opciones_area$clickElement()
Sys.sleep(0.5)

serch_area <- remDr$findElement(using = 'xpath', '//*[@id="mainSubjectAreaSelect-selectized"]')
serch_area$sendKeysToElement(list('Ciencias Sociales Aplicadas', key = 'enter'))

aplicar_area <- remDr$findElement(using = 'xpath', '/html/body/section/div/div/div[2]/div/div/form/div[2]/input')
aplicar_area$clickElement()


# PASAR PÁGINA ----
#pag_button <- remDr$findElement(using = 'xpath', '//*[@id="ResultArea"]/div[1]/div[2]/a')
pag_button <- remDr$findElement(using = "css selector", ".col-md-6.notTablet.right .pageNext")
#pag_button$clickElement()

# COMIENZA EL JUEGO

url_actual <- remDr$getCurrentUrl()[[1]]

titulo <- c()
revista <- c()
autores <- c()
link <- c()
fecha_consulta <- c()

max_iteraciones <- 2542  # Número máximo de páginas que deseas procesar
contador <- 1

# Bucle while que se ejecuta hasta que alcanzamos el número máximo de iteraciones
while (contador <= max_iteraciones) {
  tryCatch({
    # Leer el HTML de la página actual
    pagina <- read_html(url_actual)
  
    # Extraer los datos necesarios de la página
    titulo <- append(titulo, pagina %>% 
                       html_elements('.col-md-9.col-sm-8 .results .line .title') %>% 
                       html_text2())
  
    revista <- append(revista, pagina %>% 
                        html_elements(xpath = '/html/body/section/div/div/div[1]/div[2]/div[3]/div/div[2]/div[3]') %>% 
                        html_text2())
  
    autores <- append(autores, pagina %>% 
                        html_elements(xpath = '/html/body/section/div/div/div[1]/div[2]/div[3]/div/div[2]/div[2]') %>% 
                        html_text2())
  
    link <- append(link, pagina %>% 
                     html_elements(xpath = '/html/body/section/div/div/div[1]/div[2]/div[3]/div/div[2]/div[1]/a[1]') %>% 
                     html_attr('href'))
    
    # Registramos el día y la hora en que se recuperó la información del artículo
    fecha_consulta <- append(fecha_consulta, Sys.time())
    
    # Volver a localizar el botón de "siguiente página" antes de hacer clic
    pag_button <- remDr$findElement(using = "css selector", ".col-md-6.notTablet.right .pageNext")
  
    # Interactuar con Selenium para hacer clic en el botón de "siguiente página"
    # y actualizar la URL actual
    pag_button$clickElement()  # Suponiendo que `pag_button` esté previamente definido
  
    # Esperar unos momentos para permitir la carga de la siguiente página
    Sys.sleep(0.5)
  
    # Actualizar `url_actual` para la siguiente iteración
    url_actual <- remDr$getCurrentUrl()[[1]]
  
    }, error = function(e) {
      # Acción para el manejo del error
      print(paste0("Link caido: ", contador, " - ", e$message))
    })
  
  # Incrementar el contador de iteraciones
  contador <- contador + 1
}

# Al finalizar, por la forma que está configurado el código para pasar la página,
# arroja un error debido a que no puede encontrar el botón para llevar adelante esa
# accción por última vez. El error arrojado es el siguiente:
# Selenium message:Unable to locate element: .col-md-6.notTablet.right .pageNext
# For documentation on this error, please visit: https://www.seleniumhq.org/exceptions/no_such_element.html
# Build info: version: '4.0.0-alpha-2', revision: 'f148142cf8', time: '2019-07-01T21:30:10'
# System info: host: 'DESKTOP-0N4BR0T', ip: '192.168.1.41', os.name: 'Windows 10', os.arch: 'amd64', os.version: '10.0', java.version: '1.8.0_432'
# Driver info: driver.version: unknown
# 
# [1] "Link caido: 2542 - \t Summary: NoSuchElement\n \t Detail: An element could not
# be located on the page using the given search parameters.\n \t class: org.openqa.selenium.NoSuchElementException\n\t
# Further Details: run errorDetails method"

# Control general de filtro y limpieza de filtros ----

filtrar <- remDr$findElement(using = 'xpath', '//*[@id="apply_filters_button"]')
filtrar$clickElement()

limpiar <- remDr$findElement(using = 'xpath', '/html/body/section/div/div/div[1]/div[1]/div[2]/div[2]/div[1]/a[2]')
limpiar$clickElement()

# CIERRE DE SESIÓN ----
remote_driver_object$server$stop()
