paquetes <- c("magrittr", "furrr", "tidyverse", "rvest", "stringr", "robotstxt")
sapply(paquetes, require, character.only = TRUE)
#options(future.globals.onReference = "error")
options(timeout = 400000) 
if (dir.exists("computrabajo") == FALSE) {
  dir.create("computrabajo")
}
if (dir.exists("computrabajo/csv") == FALSE) {
  dir.create("computrabajo/csv")
}

# Creo funciones a utilizar posteriormente.
# ultimos_n_dias fija la cantidad de dias a scrapear
# n_paginas fija la cantidad de páginas que se van a scrapear
# pagina_link obtiene los links de los avisos a nivel individual
# avisos_func itera sobre los link y recupera información de los avisos (no la información detallada)
# El uso de las funciones es secuencial.
ultimos_n_dias <- function(.dias = 30) {
  # La función asume que el paquete xml2 y rvest están instalados.
  # Solamente funciona para la página web de computrabajo (posibilidad de generalizarla, e incluir
  # un parámetro para cada página web que se desee)
  # El parámetro a elegir son los días que se desean obtener
  # La función devuelve por defecto un string con el formato de computrabajo, 
  # el cual contiene la cantidad de avisos totales en esos días.
  
  url = paste("https://www.computrabajo.com.uy/ofertas-de-trabajo/?p=1&pubdate=", .dias, sep = "")
  download.file(url, destfile = "scrapedpage.html", quiet = TRUE)
  pagina <- xml2::read_html("scrapedpage.html")
  texto_con_avisos <- rvest::html_nodes(pagina, ".pg_grid > span") %>% 
                      rvest::html_text(.)
  file.remove("scrapedpage.html")
  closeAllConnections()
  return(texto_con_avisos)
  # url <- paste("https://www.computrabajo.com.uy/ofertas-de-trabajo/?p=1&pubdate=",dias, sep = "")
  # pagina <- xml2::read_html(url)
  # texto_con_avisos <- rvest::html_nodes(pagina, ".pg_grid > span") %>% rvest::html_text(.)
  # return(texto_con_avisos)
}
n_paginas <- function(dias) {
  # La función asume que el paquete stringr esta instalado.
  # La función recibe un texto con el formato computrabajo y extra los últimos dígitos que corresponden
  # a los avisos totales. Luego lo divide por 20, que son el total de avisos en cada página, y finalmente
  # redondea al mayor número. Ello da el número de páginas sobre el cual hay que iterar para obtener 
  # todos los avisos.
  # retorna una lista con :
  # a) La cantidad de avisos en el periodo
  # b) El número de páginas sobre el cual iterar.
  texto <- ultimos_n_dias(.dias = dias)
  avisos_totales <- stringr::str_extract(texto, pattern = "\\d{1,2},\\d{2,4}|\\d{3,4}") %>% 
    stringr::str_remove(., ",") %>% 
    as.numeric(.)
  nro_paginas <- avisos_totales %>% 
                  `/`(20) %>% 
                  ceiling(.)
  return(list(avisos = avisos_totales, paginas = nro_paginas))
}
pagina_link <- function(n = NULL, dias = NULL, todo = FALSE) {
  # La función recibe el parámetro n tiene dos objetivos:
  # 1) Determinar la cantidad de filas de la matrix que se va a crear
  # 2) Dar el número de páginas máximo sobre el cual se va a iterar
  # La función recibe el parámetro dias que tiene un objetivo:
  # Determina sobre cuantos días para atrás se van a obtener avisos.
  # Es importante destacar que la elección de "n" y "dias" se realizó previamente.
  # SOLAMENTE se puede elegir n entre 1 y 30.
  links <- matrix(nrow = n, ncol = 1)
  colnames(links) <- "link"
  for (pagina in seq(1,n,1)) {
    if(todo | dias > 30){
      links[pagina,'link'] <- paste('https://www.computrabajo.com.uy/ofertas-de-trabajo/?p=',pagina, sep = "")
    } else {
      links[pagina,'link'] <- paste('https://www.computrabajo.com.uy/ofertas-de-trabajo/?p=',pagina,'&pubdate=',dias, sep = "")
    }
  }
  return(links)
}
avisos_func <- function(webpage, paginas, .links_individuales) {
  avisos <- matrix(nrow = paginas[["avisos"]], ncol = 4)
  colnames(avisos) <- c("ID", "puesto", "EmpDepCiu", "fecha")
  i <- 0
  contador <- 0
  for (web in webpage) {
    contador = contador + 1
    if (contador != paginas[["paginas"]]) {
      j = i + 1
      i = i + 20
    } else if (contador == paginas[["paginas"]]) {
      n_0 <- (paginas[["paginas"]] - 1)*20
      n_1 <- paginas[["avisos"]]
      diff <- n_1 - n_0
      j = i + 1
      i = i + diff }
    avisos[j:i,'puesto']    <- rvest::html_nodes(web, ".js-o-link") %>% rvest::html_text(.)
    avisos[j:i,'EmpDepCiu'] <- rvest::html_nodes(web, ".lT") %>% rvest::html_text(.)
    avisos[j:i,'fecha']     <- rvest::html_nodes(web, ".dO") %>% rvest::html_text(.)
    avisos[j:i,'ID']        <- .links_individuales[j:i] 
  }
  return(avisos)
}

# Elijo la cantidad de días (los últimos) que deseo obtener, entre 1 y 30
dias = 30
# A partir de esos días elegidos obtengo el total de avisos y cantidad de páginas
paginas <- n_paginas(dias = dias)
webs <- pagina_link(n = paginas[["paginas"]], dias = dias)

# Problema al paralelizar con furrr por pointers (en Linux o mac debería andar)
#future::plan(multiprocess)
#webpage <- furrr::future_map(.x = webs, .f = xml2::read_html)
f <- function(i) {
  url = i
  print(i)
  download.file(url, destfile = "scrapedpage.html", quiet = TRUE)
  pagina <- xml2::read_html("scrapedpage.html")
  # Esta linea no es necesaria
  file.remove("scrapedpage.html")
  pagina
}
webpage <- purrr::map(.x = webs, .f = f)


# Paralel prueba
# library(parallel)
# n_cores = detectCores() - 1
# cl <- makeCluster(n_cores)
# f <- function(i) {
#   url = i
#   print(i)
#   download.file(url, destfile = "scrapedpage.html", quiet = TRUE)
#   pagina <- xml2::read_html("scrapedpage.html")
#   pagina
#   file.remove("scrapedpage.html")
# }
# status <- parLapply(cl, webs, fun = f)
# stopCluster(cl)

# otra prueba
# future::plan(multiprocess)
# links_individuales <- furrr::future_map(.x = webpage_purr, .f = rvest::html_nodes, ".js-o-link") %>% 
#                       furrr::future_map(.x = ., .f = rvest::html_attrs) %>% 
#                       purrr::map_depth(.x = ., 2, .f = ~.x[["href"]]) %>% 
#                       unlist(.) 
# https://cran.r-project.org/web/packages/future/vignettes/future-4-issues.html
# http://r-pkgs.had.co.nz/data.html#data-sysdata
# https://community.rstudio.com/t/future-multiprocess-purrr-xml-parse-error-external-pointer-is-not-valid-on-windows/6396

links_individuales <- purrr::map(.x = webpage, .f = rvest::html_nodes, ".js-o-link") %>%
  # purrr::map_df(.id = "a", .x = ., .f = rvest::html_attr, name = "href")
  purrr::map(.x = ., .f = rvest::html_attrs) %>%
  purrr::map_depth(.x = ., 2, .f = ~.x[["href"]]) %>%
  unlist(.)

if (dir.exists("html-computrabajo") == FALSE) {
  dir.create("html-computrabajo")
}

# Descargar el html de los avisos individuales y guardarlo en un archivo zip.
descargar_html <- function(x = links_individuales) {
  for (i in x) {
    temp <- paste0("https://www.computrabajo.com.uy", i)
    i = gsub(x = i, pattern = "/ofertas-de-trabajo/oferta-de-trabajo", replacement = "__", fixed = TRUE)
    # i = gsub(x = i, pattern = "-{1,2}", replacement = "+", fixed = FALSE)
    print(i)
    download.file(url = temp, destfile = paste("html-computrabajo/", i, ".html", sep = ""), quiet = TRUE)
  }
  # Read all html and compress
  # list.files(path = "./html-computrabajo", pattern = ".html$")
  # Read the 2 CSV file names from working directory
  Zip_Files <- list.files(path = "./html-computrabajo", pattern = ".html$", full.names = TRUE)
  # Zip the files
  zip::zipr(zipfile = paste("./html-computrabajo/AvisosComputrabajo", format(Sys.time(), "%F"), ".zip", sep = ''), 
            compression_level = 9, files = Zip_Files)
  file.remove(Zip_Files)
}
descargar_html(x = links_individuales)


#### Extracción de información de las páginas a nivel 'global' ####

avisos <- avisos_func(webpage, paginas, .links_individuales = links_individuales)  # ERROR EN ESTA PARTE, NECESITO QUE SEA MÁS ROBUSTA
avisos <- data.frame(avisos, stringsAsFactors = FALSE)
avisos <- cbind(avisos, fecha_scraping = as.POSIXct(Sys.time()))

avisos$EmpDepCiu <-  gsub(avisos$EmpDepCiu, pattern = "\\s{2,}", replacement =  " ") %>% 
                      gsub(., pattern = ",", replacement = "-") %>% 
                      trimws()
avisos <- tidyr::separate(avisos, col = EmpDepCiu, sep = "-", into = c("empresa","dpto","ciudad"), extra = "merge", fill = "right")
nrow(dplyr::distinct(avisos, ID)) == nrow(avisos) 
which(base::duplicated(avisos)) 
avisos$ID <- paste("https://www.computrabajo.com.uy",avisos$ID, sep = "")

# Limpio avisos repetidos en el data frame "avisos" y en el vector "links_individuales"
avisos <- dplyr::distinct(avisos, ID, .keep_all = TRUE)
links_individuales <- links_individuales[!base::duplicated(links_individuales)] %>% 
  paste('https://www.computrabajo.com.uy',. ,sep = "")
dim(avisos)[1] == length(links_individuales)

#### Paso a realizar la extracción de los avisos individuales ####

# page <- xml2::read_html(links_individuales)
# safe_links <- purrr::safely(page)
getAvisosIndividuales <- function(.links_individuales) {
  nombres <- c("categorias", "desc", "resumen", "link")
  avisos_ind <- matrix(nrow = length(.links_individuales), ncol = 4)
  colnames(avisos_ind) <- nombres
  i = 0
  for (link in .links_individuales) {
    try(
      {download.file(link, destfile = "scrapedpage.html", quiet = TRUE)
        web <- read_html("scrapedpage.html")}
    )
    i = i + 1
    temp = rvest::html_nodes(web, '.breadcrumb .breadcrumb') %>%
            rvest::html_text(.)
    if (length(temp) == 0) {
      avisos_ind[i, 'categorias'] <- NA
    } else {
      avisos_ind[i, 'categorias'] <- temp
    }
    temp = rvest::html_nodes(web, '.bWord ul') %>%
            rvest::html_text(.)
    if (length(temp) == 0) {
      avisos_ind[i, 'desc'] <- NA
    } else {
      avisos_ind[i, 'desc']       <- temp
    }
    temp = rvest::html_nodes(web, 'h2+ ul') %>%
            rvest::html_text(.)
    if (length(temp) == 0) {
      avisos_ind[i, 'resumen'] <- NA  
    } else {
      avisos_ind[i, 'resumen']    <- temp
    }
    avisos_ind[i, 'link']         <- link
    print(i)
  }
  closeAllConnections()
  return(avisos_ind)
}

avisos_ind <- getAvisosIndividuales(.links_individuales = links_individuales)

merg <- merge(x = avisos, y = avisos_ind, by.x = 'ID', by.y = 'link', all.x = TRUE, all.y = TRUE)
nrow(dplyr::distinct(merg, ID, empresa, puesto, categorias, desc, fecha))
merg <- dplyr::distinct(merg, ID, empresa, puesto, categorias, desc, fecha, .keep_all = TRUE)

ruta1 <- here::here("computrabajo")
ruta2 <- here::here("computrabajo", "csv")
saveRDS(merg, file = paste(ruta1, paste0("computrabajo_", format(Sys.time(), "%F"), ".rds"), sep = '/'))
write.csv(x = merg ,file = paste(ruta2, paste0("computrabajo_", format(Sys.time(), "%F"), ".csv"), sep = '/'), 
          row.names = FALSE, quote = TRUE)
# %F is Equivalent to %Y-%m-%d (the ISO 8601 date format).
