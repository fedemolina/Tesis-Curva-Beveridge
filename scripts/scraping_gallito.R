# Re-iniciar sesión
# Incluir a continuación para filtrar los últimos días
# https://trabajo.gallito.com.uy/buscar/fecha-publicacion/ultimo-mes/page/1 
# https://trabajo.gallito.com.uy/buscar/fecha-publicacion/ultima-quincena
# https://trabajo.gallito.com.uy/buscar/fecha-publicacion/ultima-semana/page/1
# https://trabajo.gallito.com.uy/buscar/fecha-publicacion/hace-2-dias   
# The gallito

# To solve this problem:
# Error in open.connection(x, "rb") : 
#   Timeout was reached: Resolving timed out after 10000 milliseconds 
# Increase options(timeout)
options(timeout = 400000) 
paquetes <- c("magrittr","rvest", "tidyverse", "robotstxt", "plyr", "data.table")
sapply(X = paquetes, FUN = require, character.only=T)

# Create directory if it does not exist
if (dir.exists("gallito") == FALSE) {
  dir.create("gallito")
}
# Page 1
url <- 'https://trabajo.gallito.com.uy/buscar/page/1'
url_dpto <- 'https://trabajo.gallito.com.uy/buscar/ubicacion/'
# Can we download the data? If not, doesn't matter je
robotstxt::paths_allowed(url)

getPageDpto <- function(.dpto, ..url_dpto) {
  # .dpto es un vector con los nombres de los departamentos
  # ..url_dpto es la estructura de la pagina para departamento
  
  # Number of pages we need to scrape by dpto.
  n_dpto <- vector(mode = "list")
  for (i in .dpto) {
    url = paste0(..url_dpto, i)
    try({
      download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
      content <- read_html("scrapedpage.html")  
    })
    n_dpto[i] <- html_nodes(content, ".paginate-gallito-number") %>%   
      html_text(.) %>% 
      `[`(length(.)) %>% 
      as.integer(.)
  }
  # on.exit(close(url))
  return(n_dpto %>% unlist(., use.names = FALSE) %>% as.vector())
}
getAvisosDpto <- function(.webpage) {
    avisos <- html_nodes(.webpage, ".filtro-gallito-title:nth-child(7) .cont-filter-number") %>% 
              html_text(.) %>% 
              gsub(pattern = "[^\\d]+", replacement = "", x = ., perl = TRUE) %>% 
              as.integer()
    return(avisos)
}
getNameDpto <- function(.webpage) {
    name <- html_nodes(.webpage, ".filtro-gallito-title:nth-child(7) .cont-filter-text") %>% 
            html_text(.) %>%
            `[`(1:(length(.) - 1)) %>% 
            sub("[[:space:]]", "", .) %>% 
            gsub(" ", "-", .) %>% 
            tolower(.)
    return(name)
}

# Total number of avisos to be scraped by dpto
# getDpto return a data.table with dptos, number of pages by dpto and number of avisos by dpto
url <- 'https://trabajo.gallito.com.uy/buscar/page/1'
url_dpto <- 'https://trabajo.gallito.com.uy/buscar/ubicacion/'
getDpto <- function(.url = url, .url_dpto = url_dpto) {
  # Read the webpage
  try({
    download.file(.url, destfile = "scrapedpage.html", quiet=TRUE)
    webpage <- xml2::read_html("scrapedpage.html")  
  })
  # Get the información of dpto
  dpto <- getNameDpto(.webpage = webpage)
  # Get the number of 'avisos' by dpto
  avisos_dpto <- getAvisosDpto(.webpage = webpage)
  # Get the number of pages by dpto
  paginas <- getPageDpto(.dpto = dpto, ..url_dpto = .url_dpto)
  
  DT <- data.table::data.table(dpto = dpto, n_avisos = avisos_dpto, n_paginas = paginas)
  setkey(DT, dpto)
  closeAllConnections()
  return(DT)
}
dpto <- getDpto(.url = url, .url_dpto = url_dpto)

# Descargar todos los html para luego scrapearlos de forma que coincida la cantidad de avisos
getHtml <- function() {
  if (dir.exists("html-gallito") == FALSE) {
    dir.create("html-gallito")
  }
  for (departamento in dpto[, dpto]) {
    for (n in seq.int(from = 1, by = 1, to = dpto[departamento, n_paginas])) {
      url = paste('https://trabajo.gallito.com.uy/buscar/ubicacion/', departamento,'/page/',n, sep = "")
      archivo = paste("gallito",departamento, n,format(Sys.time(), "%F"), sep = "_")
      try({
        download.file(url, destfile = paste("html-gallito/",archivo,".html", sep = ""), quiet = TRUE)
      })
      # webpage <- xml2::read_html("scrapedpage.html")
    }
  }
  closeAllConnections()
}
getHtml()

# Esto esta falalndo. ¿Para qué lo quería usar?
getLink <- function(.dpto = dpto, .url_dpto = url_dpto) {
  # .dpto es un data.frame con 3 columnas: dpto n_avisos n_paginas
  # .url_dpto es la estructura de la página web de los departamentos "https://trabajo.gallito.com.uy/buscar/ubicacion/"
    options(timeout = 400000)
    links <- lapply(1:NROW(.dpto), function(i) {
                  vector(mode = "list", length = .dpto[[i,3]])
                }
              )
    names(links) <- .dpto[["dpto"]]
  for (departamento in names(links)) {
    for (n in seq.int(from = 1, by = 1, to = .dpto[departamento, n_paginas])) {
      # i = 1
      # j = 20
      print(paste(.url_dpto, departamento, '/page/', n, sep = ""))
      
      url = paste(.url_dpto, departamento, '/page/', n, sep = "")
      try({
        download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
        content <- xml2::read_html("scrapedpage.html")
        
        link_avisos <-  html_nodes(content, ".col-md-9 .smB") %>% 
          html_attr(., "href")
        i = 1
        j = NROW(link_avisos)
        links[[departamento]][[n]][i:j] <-  link_avisos
      })
    }
  } 
    links <-  links %>% 
              unlist(., use.names = TRUE) %>% 
              data.table::as.data.table(., keep.rownames = TRUE)
    setnames(links, new = c("dpto", "link_aviso"), old = c("rn", "."))
    links[, `:=`(dpto = stringr::str_extract(dpto, pattern = "[A-Z-]+|[a-z-]+"),
                 link_aviso = paste("https://trabajo.gallito.com.uy", link_aviso, sep = ""))]
    closeAllConnections()
    file.remove("scrapedpage.html")
    return(links)
}
# link <- getLink()

getInfo <- function(.webpage, .clase, .link = FALSE) {
  if (.link) {
    return(html_nodes(.webpage, .clase) %>% 
             html_attr(., name = "href"))
  } else {
    return(html_nodes(.webpage, .clase) %>%
             html_text(.))
  } 
}
# Parámetros para la función getWebPage necesario para el scraping
puesto_codigo  = "h2"
empresa_codigo = ".bloque-start-nombre"
nivel_codigo   = ".i-flecha-derecha+ .link-post"
area_codigo    = ".i-check+ .link-post"
fecha_codigo   = ".time-text"
detalle_codigo = ".bloque-start-texto"
link_codigo    = ".col-md-9 .smB"
n_total_avisos = dpto[, sum(n_avisos)]
# Falta agregar el tiempo

getWebPage <- function(.puesto_codigo = puesto_codigo, .empresa_codigo = empresa_codigo, .nivel_codigo = nivel_codigo,
                       .area_codigo = area_codigo, .fecha_codigo = fecha_codigo, .detalle_codigo = detalle_codigo,
                       .link_codigo = link_codigo, .n = n_total_avisos) {
  
  total_rows = .n
  DT <- matrix(nrow = total_rows, ncol = 8, data = NA_character_, 
               dimnames = list(NULL, c("puesto", "empresa", "nivel", "area", "fecha", "detalle", "link", "departamento"))) # 7 + departamento

  .puesto_codigo  = puesto_codigo
  .empresa_codigo = empresa_codigo
  .nivel_codigo   = nivel_codigo
  .area_codigo    = area_codigo
  .fecha_codigo   = fecha_codigo
  .detalle_codigo = detalle_codigo
  .link_codigo    = link_codigo
  
  i = 1
  j = 0
  for (departamento in dpto[, dpto]) {
    for (n in seq.int(from = 1, by = 1, to = dpto[departamento, n_paginas])) {
      
      # url = paste('https://trabajo.gallito.com.uy/buscar/ubicacion/', departamento,'/page/',n, sep = "")
      # download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
      # webpage <- xml2::read_html("scrapedpage.html")
      try({
        archivo = paste("gallito",departamento, n,format(Sys.time(), "%F"), sep = "_")
        archivo = paste("html-gallito/",archivo,".html", sep = "")
        webpage <- xml2::read_html(archivo)    
      })
      
      puesto_vector  = getInfo(webpage, .puesto_codigo)
      empresa_vector = getInfo(webpage, .empresa_codigo)
      nivel_vector   = getInfo(webpage, .nivel_codigo)
      area_vector    = getInfo(webpage, .area_codigo)
      fecha_vector   = getInfo(webpage, .fecha_codigo)
      detalle_vector = getInfo(webpage, .detalle_codigo)
      link_vector    = getInfo(webpage, .link_codigo, .link = TRUE)
      
      k = NROW(puesto_vector)
      j = (k + i) - 1
      
      print(departamento)
      print(n)
      print(i)
      print(j)
      print(k)
      
      DT[i:j, "puesto"]  <- puesto_vector
      DT[i:j, "empresa"] <- empresa_vector
      DT[i:j, "nivel"]   <- nivel_vector
      DT[i:j, "area"]    <- area_vector
      DT[i:j, "fecha"]   <- fecha_vector
      DT[i:j, "detalle"] <- detalle_vector
      DT[i:j, "link"]    <- link_vector
      DT[i:j, "departamento"] <- departamento
      i = j + 1
    }
  }
  
  DT <- data.table::as.data.table(DT)
  DT[, link := paste("https://trabajo.gallito.com.uy", link, sep = "")]
  DT[, fecha_scraping := as.POSIXct(Sys.time())]
  # DT[, detalle := (detalle %>% gsub("\n\n"," ",.) %>% gsub("\n"," ",.))]
  closeAllConnections()
  return(DT)
}

dt = getWebPage()

ruta1 <- here::here("gallito")
ruta2 <- here::here("gallito", "csv")
saveRDS(dt, file = paste(ruta1,"/data_gallito_", format(Sys.time(), "%F"), ".rds", sep = ''))
write.csv(x = dt ,file = paste(ruta2, "/data_gallito_", format(Sys.time(), "%F"),'.csv', sep = ''), 
          row.names = FALSE, quote = TRUE)
# usar data.table::fwrite 

# Read all html and compress
list.files(path = "./html-gallito")
# Read the 2 CSV file names from working directory
Zip_Files <- list.files(path = "./html-gallito", pattern = ".html$", full.names = TRUE)
# Zip the files and place the zipped file in working directory
# zip::zipr(zipfile = "./html-gallito/TestZip.zip", files = Zip_Files, compression_level = 9)
zip::zipr(zipfile = paste("./html-gallito/TestZip", format(Sys.time(), "%F"), ".zip", sep = ''), 
          compression_level = 9, files = Zip_Files)
file.remove(Zip_Files)


# Scraping individual aviso por aviso -------------------------------------

# Scraping individual aviso por aviso
# Idealmente debería hacer todo junto. 

# Create directory if it does not exist
if (dir.exists("gallito/detallado") == FALSE) {
  dir.create("gallito/detallado")
}
if (dir.exists("gallito/detallado/csv") == FALSE) {
  dir.create("gallito/detallado/csv")
}
# dir('gallito')[9]
# data <- readRDS(paste(getwd(),'/gallito/',dir('gallito')[9],sep = ""))

# n = NROW(dt)
# link_totales = matrix(nrow = n, ncol = 2, dimnames = list(NULL, c("link", "status")))
# getStatus <- function(data) {
#   n = NROW(data)
#   link_totales = matrix(nrow = n, ncol = 2, dimnames = list(NULL, c("link", "status")))
#   j = 0
#   for(i in data[, link]) {
#     j = j + 1
#     res <- httr::GET(i)
#     link_totales[j, "status"] <- res$status_code    
#     link_totales[j, "link"]   <- i 
#     print(j)
#   }
# link_totales <- data.table::as.data.table(link_totales)
#   return(link_totales)
# }
# link_correctos <- getStatus(data = dt)

# Paralel link_correctos
library(parallel)
n_cores = detectCores() - 4
cl <- makeCluster(n_cores)
getStatus <- function(i) {
  res <- httr::GET(i)
  res$status_code
}
options(timeout = 999999)
status <- parallel::parLapply(cl, dt$link, fun = getStatus)
parallel::stopCluster(cl)
link_correctos <- data.table::data.table(status = status, link = dt$link)
link_correctos <- link_correctos[status == 200,]
closeAllConnections()

contenedor1 <- matrix(ncol = 4, nrow = NROW(link_correctos)) %>% as.data.frame(.)
contenedor2 <- matrix(ncol = 4, nrow = NROW(link_correctos)) %>% as.data.frame(.)
contenedor3 <- matrix(ncol = 4, nrow = NROW(link_correctos)) %>% as.data.frame(.)
i = 0
options(timeout = 9999999)
for (links in link_correctos[, link]) {
  i = i + 1
  print(i)
  try({
    webpage <- xml2::read_html(links)
  })
  temp <- t(
            rvest::html_text(
              rvest::html_nodes(webpage, ".max-ficha .cuadro-aviso-text")
            )
          )
  # rvest::html_nodes(webpage, ".max-ficha .cuadro-aviso-text") %>% 
  #       rvest::html_text(.) %>% 
  #       t(.)
  k = ncol(temp)
  if (k == 1) {
    contenedor1[i,] <- cbind(temp,NA, NA, links)
  }
  if (k == 2) {
    contenedor2[i,] <- cbind(temp, NA, links)
  }
  if (k == 3) {
    contenedor3[i,] <- cbind(temp,links)
  }
  Sys.sleep(1)
}
closeAllConnections()

# prueba. Aún no esta funcionando.
# clusterExport(cl, c("i", "contenedor1", "contenedor2", "contenedor3"))
# clusterEvalQ(cl, library(magrittr))
# f_avisos <- function(l) {
#   i = i + 1
#   webpage <- l %>% xml2::read_html(.)
#   temp <- rvest::html_nodes(webpage, ".max-ficha .cuadro-aviso-text") %>% 
#     rvest::html_text(.) %>% 
#     t(.)
#   k = ncol(temp)
#   if (k == 1) {
#     contenedor1[i,] <- cbind(temp,NA, NA, l)
#   }
#   if (k == 2) {
#     contenedor2[i,] <- cbind(temp, NA, l)
#   }
#   if (k == 3) {
#     contenedor3[i,] <- cbind(temp,l)
#   }
# }
# dd <- parLapply(cl, link_correctos[1:50, link], f_avisos)
# stopCluster(cl)


# combinar y remover filas vacias
df <- plyr::rbind.fill(contenedor1, contenedor2, contenedor3)
df <- df[!apply(is.na(df), 1, all),]

# Nombres de las variables
names(df) <- c("Responsabilidades", "Funciones", "Requisitos", "link")

# Merge de la información de los avisos. En este paso se logra juntar toda la información del aviso. link = id
merg <- merge(dt, df, by = 'link', all.x = TRUE, all.y = TRUE) 

ruta1 <- here::here("gallito", "detallado")
ruta2 <- here::here("gallito", "detallado", "csv")
saveRDS(merg, file = paste(ruta1,"/data_gallito_", format(Sys.time(), "%F"), ".rds", sep = ''))
write.csv(x = merg ,file = paste(ruta2, "/data_gallito_", format(Sys.time(), "%F"),'.csv', sep = ''), 
          row.names = FALSE, quote = TRUE)

