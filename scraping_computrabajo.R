
library(rvest)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(robotstxt) # Con esta libreria corrobo si los datos "se pueden descargar".
data <- data.frame(puesto = character(), EmpDepCiud = character(),fecha = character())
data0 <- data.frame(puesto = character(), EmpDepCiud = character(),fecha = character(),stringsAsFactors = FALSE)

url <- paste("https://www.computrabajo.com.uy/ofertas-de-trabajo/?p=",1, sep = "")
webpage <- read_html(url)
tot.avisos <- html_nodes(webpage, ".pg_grid > span") %>% html_text(.)
right <- function(text, num_char) {
  substr(text, nchar(text) - (num_char - 1), nchar(text)) %>% 
    str_replace(., pattern = ",", replacement = ".") %>% 
    as.numeric(.) %>%
    `*`(1000) %>%
    `/`(20) %>%
    ceiling(.)
}

n <- right(tot.avisos,5)

webpage <- data.frame(link = 1:n)
for (pagina in 1:n) {
  webpage[pagina, 'link'] <- paste("https://www.computrabajo.com.uy/ofertas-de-trabajo/?p=",pagina, sep = "")
}
webpage <- apply(X = webpage, MARGIN = 1, FUN = read_html)
links <- lapply(X = lapply(X = webpage, FUN = html_nodes, ".js-o-link"), html_attrs) %>% unlist(.)
links <- links[seq(2,length(links),3)]
for (webpage in webpage) {
  #Url to be scraped
  # url <- paste("https://www.computrabajo.com.uy/ofertas-de-trabajo/?p=",i, sep = "")
  # #Reading the html code from the website
  # webpage <- read_html(url)
  #Using CSS selectors to scrap
  puesto <- html_nodes(webpage, ".js-o-link") %>% html_text(.)
  #Empresa-Ciudad-Departamento
  EmpDepCiud <- html_nodes(webpage, ".lT") %>% html_text(.)
  #Data-Preprocessing: removing '\r\n'
  EmpDepCiud <- gsub("\r\n ","",EmpDepCiud) %>% gsub(" ","",.) %>% gsub(",","-",.)
  #Using CSS selectors to scrap the time
  fecha <- html_nodes(webpage, ".dO") %>% html_text(.)
  
  data0 <- data.frame(puesto = puesto, EmpDepCiud, fecha, stringsAsFactors = FALSE)
  data <- rbind(data,data0, stringsAsFactors = FALSE)
}
data <- cbind(data, fecha_scraping = as.POSIXlt(Sys.time()))
data <- cbind(data, links)
#
pattern = '.*-.*-.*-.*'
remplazar <- grep(pattern, data$EmpDepCiud)
data$EmpDepCiud[remplazar] <- sub('-', ' ', data$EmpDepCiud[remplazar])

data <- separate(data, col = EmpDepCiud, sep = "-", into = c("Empresa","Departamento","Ciudad"), extra = "merge", fill = "right")

### Obtengo información individual
detalle0 <- data.frame(cate = character(), hora = character(), des_req = character(), resumen = character(), 
                       stringsAsFactors = FALSE)
detalle <- detalle0
contador = 0
for (link in links) {
  try(web <- paste('https://www.computrabajo.com.uy',link, sep = "") %>% 
        read_html(.))
  cate <- html_nodes(web, '.breadcrumb .breadcrumb') %>% html_text(.)
  hora <- html_nodes(web, '.box_image p') %>% html_text(.)
  des_req <- html_nodes(web, '.bWord ul') %>% html_text(.)
  resumen <- html_nodes(web, 'h2+ ul') %>% html_text(.)
  detalle0 <- data.frame(cate, hora, des_req, resumen, stringsAsFactors = FALSE)
  detalle <- rbind(detalle, detalle0, stringsAsFactors = FALSE)
  #Sys.sleep(1)
  contador = contador + 1
  print(contador)
}
# Combino información individual y global.
detalle <- cbind(detalle, fecha_scrapping = as.POSIXlt(Sys.time()))

ruta1 <- "C:/Users/Usuario/Documents/MAESTRIA/scraping/computrabajo/"
ruta2 <- "C:/Users/Usuario/Documents/MAESTRIA/scraping/computrabajo/csv/"
saveRDS(data, file = paste(ruta1,"/data_computrabajo_", str_replace_all(Sys.time(),":","-"), sep = ''))
write.csv(x = data ,file = paste(ruta2, "/data_computrabajo_", str_replace_all(Sys.time(),":","-"),'.csv', sep = ''), 
          row.names = FALSE, quote = TRUE)

### Falta ir a cada aviso de forma individual para obtener:
  # Categoría
  # Descripción
  # 

