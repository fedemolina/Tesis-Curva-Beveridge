
# Cargo las librerias
library(robotstxt) # Con esta libreria corrobo si los datos "se pueden descargar".
library(here)
library(rvest)
library(tidyverse)

# Este scrip obtiene los datos de busco jobs

# Create directory if it does not exist
if (dir.exists("buscojobs") == FALSE) {
  dir.create("buscojobs")
}
if (dir.exists("buscojobs/csv") == FALSE) {
  dir.create("buscojobs/csv")
}

url <- paste("https://www.buscojobs.com.uy/ofertas/",1, sep = "")
webpage <- xml2::read_html(url)
robotstxt::paths_allowed(url)

# Total de avisos publicados
n <- rvest::html_node(webpage, "h2") %>% 
  rvest::html_text() %>% 
  stringr::str_extract(., "[[:digit:]]{1,4}") %>% 
  as.numeric(.)
# Hay 15 avisos por página, por lo tanto n/15 es el total.
tot_pag <- base::ceiling(n/15)
# Creo el data frame
data <- matrix(nrow = n, ncol = 3) %>% data.frame(.)
names(data) <- c("puesto","fechaPub","ID")

i <- 0
for (pagina in 1:tot_pag) {
  if (pagina != tot_pag) {
  j = i + 1
  i = i + 15
  } else if (pagina == tot_pag) {
  n_0 <- (tot_pag - 1)*15
  n_1 <- n
  diff <- n_1 - n_0
  j = i + 1
  i = i + diff
  }
  url <- paste("https://www.buscojobs.com.uy/ofertas/",pagina, sep = "")
  webpage <- xml2::read_html(url) 
  # ID de cada aviso
  data$ID[j:i] <- webpage %>% rvest::html_nodes(., ".link-header a") %>% rvest::html_attrs(.)
  # fecha
  data$fechaPub[j:i] <-  webpage %>% rvest::html_nodes(., "small.pull-right") %>% rvest::html_text(.)
  # Puesto
  data$puesto[j:i] <- webpage %>% rvest::html_nodes(., ".link-header a") %>% rvest::html_text(.)
  print(pagina)
}

data$fechaScraping <- as.POSIXct(Sys.time())
data$ID <- data$ID %>% base::unlist(.) %>% gsub("//", "", .)

# Itero a nivel individual de aviso. Luego se matchean las tablas por ID (link)
avisos <- matrix(nrow = n, ncol = 8) %>% data.frame(.)
names(avisos) <- c('empresa', 'lugar', 'area', 'puestos', 'jornada', 'detalles', 'subareas', 'requisitos', 'ID')
i <- 0
for (link in data$ID) {
  i <- i + 1
  url <- paste('https://',link, sep = "")
  webpage <- xml2::read_html(url)
  temp <- webpage %>% rvest::html_nodes(., 'h2') %>% rvest::html_text(.)
  avisos$empresa[i] <- temp[1]
  avisos$lugar[i] <- temp[2]
  avisos$area[i] <- temp[3]
  temp2 <- webpage %>% rvest::html_nodes(., '.col-sm-12 span') %>% rvest::html_text(.)
  if (length(temp2) == 0) {
    avisos$puestos[i] <- NA
    avisos$jornada[i] <- NA
  } else if (length(temp2) == 1) {
    avisos$puestos[i] <-  NA
    avisos$jornada[i] <- temp2
  } else if (length(temp2) == 2) {
    avisos$puestos[i] <- temp2[1]
    avisos$jornada[i] <- temp2[2]
  }
  avisos$detalles[i] <- webpage %>% rvest::html_nodes(., '.descripcion-texto p') %>% rvest::html_text(.)
  avisos$subareas[i] <- webpage %>% rvest::html_nodes(., '.text-container .oferta-contenido ul') %>% rvest::html_text(.)
  avisos$requisitos[i] <- webpage %>% rvest::html_nodes(., '.subheader+ .oferta-contenido') %>% rvest::html_text(.)
  
}

avisos$ID <- data$ID

# Junto los datos de 'data' y 'avisos' consolidando la información del aviso.
# Luego limpio si hay duplicados basandome en el link (ID)
which(base::duplicated(data))
which(base::duplicated(avisos))
merg <- dplyr::inner_join(data, avisos, by = 'ID')
merg <- merg[which(!duplicated(merg)),]

dplyr::distinct(merg, ID)

# Limpieza parcial de datos.

# Análisis de fecha y categorización de variables.
merg$fechaPub <- merg$fechaPub %>% gsub("Publicado hace ","", .) %>% 
  gsub("días", "dia", .) %>% 
  gsub("día", "dia", .) %>% 
  gsub("horas", "hora", .) %>% 
  gsub("semanas", "semana", .) %>% 
  gsub("meses", "mes", .) %>% 
  gsub("un", "1", .)

merg <- merg %>% tidyr::separate(., col = fechaPub, sep = " ", into = c("cantidad","unidad"))
merg$cantidad <- as.numeric(merg$cantidad)
# Convierto a segundos
minuto = 60
hora = 60*minuto
dia = 24*hora
semana = 7*dia
mes = 30*dia
merg <- merg %>% dplyr::mutate(unidad_seg = ifelse(.$unidad == "minutos",minuto,
                                        ifelse(.$unidad == 'hora',hora,
                                               ifelse(.$unidad == 'dia',dia,
                                                      ifelse(.$unidad == 'semana',semana,
                                                             mes)))),
                       fecha_pub = .$fechaScraping - (.$cantidad*unidad_seg))
merg <- merg %>% tidyr::separate(., col = fecha_pub, into = c("fpub","hpub"), sep = " ")
merg$fpub <- base::as.Date(merg$fpub, format = "%Y-%m-%d")
merg$cantidad <- NULL
merg$unidad <- NULL
merg$unidad_seg <- NULL
merg$hpub <- NULL
merg$dia <- factor(weekdays(merg$fpub), levels = c("lunes","martes","miércoles","jueves","viernes",
                                               "sábado","domingo"), ordered = TRUE)
# Separo en barrio-ciudad y departamento. Convierto los tipos de datos
merg <- merg %>% tidyr::separate(., col = lugar, into = c("barrioCiudad","dpto"), sep = ",", fill = 'left')
merg$dpto <- merg$dpto %>% gsub(" ", "", .)
merg$barrioCiudad <- merg$barrioCiudad %>% gsub(" ", "", .) 
merg$puestos <- merg$puestos %>% as.integer(.)
merg$barrioCiudad <- merg$barrioCiudad %>% factor(.) %>% head(.)
merg$dpto <- ifelse(merg$dpto == "Uruguay", "Exterior", merg$dpto) 
merg$dpto <- merg$dpto %>% factor(., levels = c("Artigas", "Canelones", "CerroLargo", "Colonia", "Durazno",
                                                "Flores", "Florida", "Lavalleja", "Maldonado", "Montevideo",
                                                "Paysandú", "RíoNegro", "Rivera", "Rocha", "Salto", 
                                                "SanJosé", "Soriano", "Tacuarembó", "TreintayTres"))
# Me falta arreglar los llamados estatales, pasarlos a formato UDELAR, OSE, UTE, etc.
# "" "" todo el trabajo de texto con los campos subareas, requisitos, detalles y puesto.
dplyr::glimpse(merg)
# Guardo los datos.

ruta1 <- "C:/Users/Usuario/Documents/MAESTRIA/scraping/buscojobs"
ruta2 <- "C:/Users/Usuario/Documents/MAESTRIA/scraping/buscojobs/csv"
saveRDS(merg, file = paste(ruta1,"/data_buscojobs_", format(Sys.time(), "%F"), sep = ''))
write.csv(x = merg ,file = paste(ruta2, "/data_buscojobs_", format(Sys.time(), "%F"),'.csv', sep = ''), 
          row.names = FALSE, quote = TRUE)
# %F is Equivalent to %Y-%m-%d (the ISO 8601 date format).