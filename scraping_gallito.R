
rm(list = ls())
# The gallito
library(rvest)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(robotstxt) # Con esta libreria corrobo si los datos "se pueden descargar".
library(plyr)

# Create directory if it does not exist
if (dir.exists("gallito") == FALSE) {
  dir.create("gallito")
}
# Page 1
url <- 'https://trabajo.gallito.com.uy/buscar/page/1'
url_dpto <- 'https://trabajo.gallito.com.uy/buscar/ubicacion/'
# Can we download the data? If not, doesn't matter je
robotstxt::paths_allowed(url)

# Reading the html code from the website
webpage <- read_html(url)

# Create data frame that will be filled
data0 <- data.frame(puesto = character(), empresa = character(), nivel = character(), area = character(), 
                    fecha = character(), detalle = character(), dpto = character(), link = character(), stringsAsFactors = FALSE)
data <- data.frame(puesto = character(), empresa = character(), nivel = character(), area = character(), 
                   fecha = character(), detalle = character(), dpto = character(), link = character(), stringsAsFactors = FALSE)

# Total number of pages to be scraped by dpto
dpto <- html_nodes(webpage, ".filtro-gallito-title:nth-child(7) .cont-filter-text") %>% 
        html_text(.) %>%
        `[`(1:(length(.) - 1)) %>% 
        sub("[[:space:]]", "", .) %>% 
        gsub(" ", "-", .) %>% 
        tolower(.)
n_dpto <- vector()
for (i in dpto) {
  url <- paste(url_dpto, i, sep = "") %>% read_html(.)
  n_dpto[i] <- html_nodes(url, ".paginate-gallito-number") %>% 
            html_text(.) %>% 
            `[`(length(.)) %>% 
            as.numeric(.)
}

# Scraping :)
for (departamento in names(n_dpto)) {
  for (n in 1:n_dpto[[departamento]]) {
    #Url to scrapped and converting to html
    webpage <- paste('https://trabajo.gallito.com.uy/buscar/ubicacion/', departamento,'/page/',n, sep = "") %>% 
      read_html(.)
    #Puesto publicado
    puesto <- html_nodes(webpage, "h2") %>% html_text(.)
    #Getting the bussines
    empresa <- html_nodes(webpage, ".bloque-start-nombre") %>% html_text(.)
    # Getting the level according to el pais.
    nivel <- html_nodes(webpage, ".i-flecha-derecha+ .link-post") %>% html_text(.)
    # Getting the area according to el país
    area <- html_nodes(webpage, ".i-check+ .link-post") %>% html_text(.)
    #Getting the date when the data was post
    fecha <- html_nodes(webpage, ".time-text") %>% html_text(.)
    #Detalle
    detalle <- html_nodes(webpage, ".bloque-start-texto") %>% html_text(.) %>% gsub("\n\n"," ",.) %>% gsub("\n"," ",.)
    # link para obtener toda la descripción
    link <-  html_nodes(webpage, '.smB') %>% html_attrs(.)
    link[[1]] <- NULL
    link <- lapply(X = link, FUN = `[[`,(1)) %>% unlist(.)
    # Testear que tengan el mismo número de filas. Sino...Poner NA
    puesto[!str_detect(string = puesto, pattern = "")] = NA
    empresa[!str_detect(string = empresa, pattern = "")] = NA
    nivel[!str_detect(string = nivel, pattern = "")] = NA
    area[!str_detect(string = area, pattern = "")] = NA
    fecha[!str_detect(string = fecha, pattern = "")] = NA
    detalle[!str_detect(string = detalle, pattern = "")] = NA
    link[!str_detect(string = link, pattern = "")] = NA
    
    data0 <- data.frame(puesto = puesto, empresa = empresa, nivel = nivel, area = area, fecha = fecha, detalle = detalle,
                        dpto = departamento, link = link, stringsAsFactors = FALSE)
    data <- rbind(data0,data)
    Sys.sleep(1)
  }
}
data <- cbind(data,fecha_scraping = as.POSIXlt(Sys.time()))
ruta1 <- "C:/Users/Usuario/Documents/MAESTRIA/scraping/gallito/"
ruta2 <- "C:/Users/Usuario/Documents/MAESTRIA/scraping/gallito/csv/"
saveRDS(data, file = paste(ruta1,"/data_gallito_", str_replace_all(Sys.time(),":","-"), sep = ''))
write.csv(x = data ,file = paste(ruta2, "/data_gallito_", str_replace_all(Sys.time(),":","-"),'.csv', sep = ''), 
          row.names = FALSE, quote = TRUE)

# Scraping individual aviso por aviso
dir('gallito')[9]
data <- readRDS(paste(getwd(),'/gallito/',dir('gallito')[9],sep = ""))
data$link %>% head()

contenedor1 <- matrix(ncol = 4, nrow = nrow(data)) %>% as.data.frame(.)
contenedor2 <- matrix(ncol = 4, nrow = nrow(data)) %>% as.data.frame(.)
contenedor3 <- matrix(ncol = 4, nrow = nrow(data)) %>% as.data.frame(.)
i = 0
for (links in data$link) {
  i = i + 1
  webpage <- paste('https://trabajo.gallito.com.uy',links, sep = "") %>% read_html(.)
  temp <- html_nodes(webpage, ".max-ficha .cuadro-aviso-text") %>% html_text(.) %>% t(.)
  if (ncol(temp) == 1) {
    contenedor1[i,] <- cbind(temp,NA, NA, links)
  }
  if (ncol(temp) == 2) {
    contenedor2[i,] <- cbind(temp, NA, links)
  }
  if (ncol(temp) == 3) {
    contenedor3[i,] <- cbind(temp,links)
  }
  print(i)
}
# remover filas vacias
df <- plyr::rbind.fill(contenedor1, contenedor2, contenedor3)
df <- df[!apply(is.na(df), 1, all),]


# Combinar los contenedores
names(df) <- c("Responsabilidades", "Funciones", "Requisitos", "link")
# Observaciones para mejorar:
  # 1. Notar que webpage <- paste('https://trabajo.gallito.com.uy/buscar/ubicacion/', departamento,'/page/',n, sep = "") %>% 
  #                          read_html(.)
  # se esta corriendo dos veces, y es lo que más demora en correr:
    # Una opción es guardar el objeto y luego reutilizarlo
    # Otra opción es correr todo dentro del mismo loop.