
# El gallito
library(rvest)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(robotstxt) # Con esta libreria corrobo si los datos "se pueden descargar".

dir.create("gallito") # Esto solo una vez. AGREGAR CÓDIGO PARA TESTEAR SI YA EXISTE Y ENTONCES SALTEARLO.
# página 1
url <- 'https://trabajo.gallito.com.uy/buscar/page/1'
url_dpto <- 'https://trabajo.gallito.com.uy/buscar/ubicacion/'
# Corrobo que los datos "se pueden" descargar.
robotstxt::paths_allowed(url)

#Reading the html code from the website
webpage <- read_html(url)

#Número total de páginas a scrapear
data0 <- data.frame(puesto = character(), empresa = character(), nivel = character(), area = character(), 
                    fecha = character(), detalle = character(), dpto = character(), stringsAsFactors = FALSE)
data <- data.frame(puesto = character(), empresa = character(), nivel = character(), area = character(), 
                    fecha = character(), detalle = character(), dpto = character(), stringsAsFactors = FALSE)

# Número total de páginas a scrapear por dpto
dpto <- html_nodes(webpage, ".filtro-gallito-title:nth-child(7) .cont-filter-text") %>% 
        html_text(.) %>%
        `[`(1:(length(.)-1)) %>% 
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
    data0 <- data.frame(puesto = puesto, empresa = empresa, nivel = nivel, area = area, fecha = fecha, detalle = detalle,
                        dpto = departamento ,stringsAsFactors = FALSE)
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
