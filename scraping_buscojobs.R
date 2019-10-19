# Re-iniciar R
# Cargo las librerias
library(robotstxt) # Con esta libreria corrobo si los datos "se pueden descargar".
library(here)
library(rvest)
library(tidyverse)
library(data.table)
library(magrittr)

# Este scrip obtiene los datos de busco jobs

# Create directory if it does not exist
if (dir.exists("buscojobs") == FALSE) {
  dir.create("buscojobs")
}
if (dir.exists("buscojobs/csv") == FALSE) {
  dir.create("buscojobs/csv")
}

# Total de avisos publicados en buscojobs
avisosPagina = 15L
dias = 4L
totalAvisos <- function(.avisosPagina = 15L, .dias = 3L) {
  # Si dias = 1 se restringe a los avisos publicados en el día
  #    dias = 2 se restringe a los avisos publicados desde ayer
  #    dias = 3 se restringe a los avisos publicados últimos 7 días
  #    dias = 4 no hay restricción
  
  archivo = "scrapedpage.html"
  url = paste0("https://www.buscojobs.com.uy/ofertas?fechainicio=", .dias)
  download.file(url, destfile = archivo, quiet = TRUE)
  webpage <- xml2::read_html(archivo)
  n_avisos  <-  rvest::html_node(webpage, "h2") %>% 
                rvest::html_text(.) %>% 
                stringr::str_extract(., "[[:digit:]]{1,4}") %>% 
                as.numeric(.)
  n_paginas <-  base::ceiling(n_avisos/.avisosPagina)
  closeAllConnections()
  file.remove(archivo)
  return(list(n_avisos = n_avisos, n_paginas = n_paginas))
}

# Extraigo cantidad de avisos y páginas
busco_Jobs_Info <- totalAvisos(.avisosPagina = avisosPagina, .dias = dias)
n_avisos  <- busco_Jobs_Info[["n_avisos"]]
n_paginas <- busco_Jobs_Info[["n_paginas"]] 

# Loop extraigo información de avisos en cada página
extractPaginas <- function(.n_avisos = n_avisos, .n_paginas = n_paginas, .dias = dias) {
  data        <- data.table::as.data.table(matrix(nrow = .n_avisos, ncol = 3L, NA_character_))
  setnames(data, old = c("V1", "V2", "V3"), new = c("ID","fecha_pub","puesto"))
  i <- 0L
  archivo = "scrapedpage.html"
  for (pagina in 1L:.n_paginas) {
    if (pagina != .n_paginas) {
      j = i + 1L
      i = i + 15L
    } else if (pagina == .n_paginas) {
      n_0 <- (.n_paginas - 1L)*15L
      n_1 <- .n_avisos
      diff <- n_1 - n_0
      j = i + 1L
      i = i + diff
    }
    
    url = paste0("https://www.buscojobs.com.uy/ofertas/", pagina, "?fechainicio=", .dias)
    download.file(url, destfile = archivo, quiet = TRUE)
    webpage <- xml2::read_html(archivo)
    
    # ID de cada aviso
    data.table::set(x = data, i = (j:i), 1L, value = base::unlist(webpage %>% rvest::html_nodes(., ".link-header h3 a") %>% rvest::html_attrs(.)))
    # fecha
    data.table::set(x = data, i = (j:i), 2L, value = base::unlist(webpage %>% rvest::html_nodes(., "#page-content-wrapper .pull-right") %>% rvest::html_text(.)))
    # Puesto
    data.table::set(x = data, i = (j:i), 3L, value = base::unlist(webpage %>% rvest::html_nodes(., "h3 a") %>% rvest::html_text(.)))
    print(pagina)
  }
  data[, fecha_scraping := as.POSIXct(Sys.time())]
  file.remove(archivo)
  closeAllConnections()
  return(data)
}
DT_paginas <- extractPaginas(.n_avisos = n_avisos, .n_paginas = n_paginas, .dias = dias)
# Test link individuales válidos. Remover los que fallan (PENDIENTE). Debe ser el input de extractAvisos


# Itero a nivel individual de aviso. Luego se matchean las tablas por ID (link)
extractAvisos <- function(.link = DT_paginas[["ID"]], .n_avisos = n_avisos) {
  DT <- data.table::as.data.table(matrix(nrow = n_avisos, ncol = 9L, NA_character_))
  col_names <- c('empresa', 'lugar', 'area', 'n_puestos', 'jornada', 'detalles', 'subareas', 'requisitos', 'ID')
  data.table::setnames(x = DT, old = paste0(rep("V",9L),1L:9L), new = col_names)
  i <- 0L
  archivo = "scrapedpage.html"
  for(link in .link) {
    i <- i + 1L
    
    url = paste0('https:', link)
    download.file(url, destfile = archivo, quiet = TRUE)
    webpage <- xml2::read_html(archivo)
    
    temp <- webpage %>% rvest::html_nodes(., 'h2') %>% rvest::html_text(.)  
    
    set(DT, i = i, j = col_names[1L], value = temp[1])
    set(DT, i = i, j = col_names[2L], value = temp[2])
    set(DT, i = i, j = col_names[3L], value = temp[3])
    
    temp2 <- webpage %>% rvest::html_nodes(., '.col-sm-12 span') %>% rvest::html_text(.)
    largo = length(temp2)
    
    if (largo == 0L) {
      set(DT, i = i, j = col_names[4L], value = NA_character_)
      set(DT, i = i, j = col_names[5L], value = NA_character_)
    } else if (largo == 1L) {
      set(DT, i = i, j = col_names[4L], value = NA_character_)
      set(DT, i = i, j = col_names[5L], value = temp2)
    } else if (largo == 2L) {
      set(DT, i = i, j = col_names[4L], value = temp2[1L])
      set(DT, i = i, j = col_names[5L], value = temp2[2L])
    }
    set(DT, i = i, j = col_names[6L], value = webpage %>% rvest::html_nodes(., '.descripcion-texto p') %>% rvest::html_text(.))
    set(DT, i = i, j = col_names[7L], value = webpage %>% rvest::html_nodes(., '.text-container .oferta-contenido ul') %>% rvest::html_text(.))
    set(DT, i = i, j = col_names[8L], value = webpage %>% rvest::html_nodes(., '.subheader+ .oferta-contenido') %>% rvest::html_text(.))
    set(DT, i = i, j = col_names[9L], value = link)
  }
  DT[, n_puestos := as.integer(n_puestos)]
  file.remove(archivo)
  closeAllConnections()
  return(DT)
}
DT_avisos <- extractAvisos(.n_avisos = n_avisos)

# Junto los datos de 'data' y 'avisos' consolidando la información del aviso.
setkey(DT_avisos,  ID)
setkey(DT_paginas, ID)
DT <- DT_avisos[DT_paginas, nomatch = 0]
n_duplicados <- sum(duplicated(DT_paginas[["ID"]]))

# Remuevo duplicados? Pierdo información de duplicados. Mantenerlos? Si se repiten, en el inner quedan más q duplicados
# merg <- merg[which(!duplicated(merg)),]
# dplyr::distinct(merg, ID)

# Limpieza parcial de datos.
DT[, fecha_pub := gsub("Publicado hace ","", fecha_pub) %>% 
                  gsub("días", "dia", .) %>% 
                  gsub("día", "dia", .) %>% 
                  gsub("horas", "hora", .) %>% 
                  gsub("semanas", "semana", .) %>% 
                  gsub("meses", "mes", .) %>% 
                  gsub("un", "1", .)]
DT[, c("cantidad", "unidad") := data.table::tstrsplit(fecha_pub, " ", type.convert = TRUE)]

# Convierto a segundos
minuto = 60
hora = 60*minuto
dia = 24*hora
semana = 7*dia
mes = 30*dia

DT[, unidad_seg := mes
  ][unidad == "minutos", unidad_seg := minuto
  ][unidad == "hora",    unidad_seg := hora
  ][unidad == "dia",     unidad_seg := dia
  ][unidad == "semana",  unidad_seg := semana]
DT[, fecha_pub := fecha_scraping - cantidad * unidad_seg]

DT[, c("fecha_pub", "h_pub") := data.table::tstrsplit(fecha_pub, " ", type.convert = TRUE)]
DT[, fecha_pub := as.Date(fecha_pub, format = "%Y-%m-%d")]
DT[, `:=`(cantidad   = NULL,
          unidad     = NULL,
          unidad_seg = NULL)]

unwanted_array = list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                          'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                          'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                          'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                          'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

DT[, dia_pub := factor(weekdays(fecha_pub) %>% 
                       chartr(paste(names(unwanted_array), collapse=''),
                              paste(unwanted_array, collapse=''),
                              .), 
                       levels = c("lunes","martes","miercoles","jueves","viernes",
                                  "sabado","domingo"), ordered = TRUE)]
DT[, c("barrio_ciudad", "dpto") := data.table::tstrsplit(lugar, ",", fill = NA_character_)]
DT[is.na(dpto), dpto := barrio_ciudad]
DT[, lugar := NULL]
DT[, barrio_ciudad := trimws(barrio_ciudad)]

DT[, dpto := chartr(paste(names(unwanted_array), collapse=''),
                   paste(unwanted_array, collapse=''),
                   dpto) %>% 
              trimws() %>% 
              tolower() %>% 
              as.factor()]
DT[, barrio_ciudad := tolower(barrio_ciudad) %>% 
                      chartr(paste(names(unwanted_array), collapse=''),
                             paste(unwanted_array, collapse=''),
                             .) %>% 
                     trimws()]

DT[, requisitos := gsub(pattern = "\n|\t", replacement = "", x = requisitos)]
DT[, subareas   := gsub(pattern = "\n|\t", replacement = "", x = subareas)] 
DT[, detalles   := gsub(pattern = "•\t|\n|\t", replacement = "", x = detalles)
  ][, detalles  := gsub(pattern = ".-", replacement = ". ", x = detalles)]
DT[, ID := gsub(pattern = "//", replacement = "", x = ID)]
DT[, puesto := puesto %>% gsub(., pattern = "\\['|'\\]|[1-9]|\\/|'|,", replacement = "") %>% trimws(.)]

# Me falta arreglar los llamados estatales, pasarlos a formato UDELAR, OSE, UTE, etc.(????)
# Guardo los datos.

ruta1 <- here::here("buscojobs")
ruta2 <- here::here("buscojobs", "csv")
saveRDS(DT, file = paste(ruta1, paste0("data_buscojobs_", format(Sys.time(), "%F"),".rds"), sep = '/'))
write.csv(x = DT ,file = paste(ruta2, paste0("data_buscojobs_", format(Sys.time(), "%F"),'.csv'), sep = '/'), 
          row.names = FALSE, quote = TRUE)
# %F is Equivalent to %Y-%m-%d (the ISO 8601 date format).