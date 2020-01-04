# Scraping de opciónempleo.com.uy

# Librerias 
library(rvest)
library(tidyverse)
library(robotstxt) # Con esta libreria corrobo si los datos "se pueden descargar".

# Datos que contiene la página:
# Titulo = .title
# Empresa = .company_compact
# Ubicación (Departamento) = .locations_compact .locations_compact
# Fecha (dia - mes) = .date_compact
# Descripción = .advertise_compact
# Salario (solo en algunos avisos) = .price
# Accediendo al llamado se puede obtener la descripción completa.

# páginas inferior : #heart .selected
# .display_inline nobr
data <- data.frame(puesto = character(), empresa = character(),ubicacion=character(), fecha = as.character(), 
                   descripcion = as.character(), salario = as.numeric(), stringsAsFactors = FALSE)
data0 <- data.frame(puesto = character(), empresa = character(),ubicacion=character(), fecha = as.character(), 
                   descripcion = as.character(), salario = as.numeric(), stringsAsFactors = FALSE)
url <- paste("https://www.opcionempleo.com.uy/wbuscar/empleos?l=Uruguay&lid=114373&b=",1, sep = "")
# Ver si estos bananas me 'permiten' sacar datos.
paths_allowed(url)
webpage <- read_html(url)
tot.avisos <- html_nodes(webpage, ".display_inline nobr") %>% html_text(.)
n <- stringr::str_split(tot.avisos, pattern = " ") %>%  unlist(.) %>% '[['(5) %>% as.numeric(.)
secuencia <- seq(1,n,20)

for (i in secuencia) {
  #Url to be scraped
  url <- paste("https://www.opcionempleo.com.uy/wbuscar/empleos?l=Uruguay&lid=114373&b=",i, sep = "")
  
  #Reading the html code from the website
  webpage <- read_html(url)
  # Paso extra para parar cuando no avance más la página
  if (i == 1){
    verificador <- 1
  } else {
    verificador <- stringr::str_split(tot.avisos, pattern = " ") %>%  unlist(.) %>% '[['(3) %>% as.numeric(.)
  }
  tot.avisos <- html_nodes(webpage, ".display_inline nobr") %>% html_text(.)
  contador <- stringr::str_split(tot.avisos, pattern = " ") %>%  unlist(.) %>% '[['(3) %>% as.numeric(.)
  if (verificador == contador) {
    break
  } else {
  #Using CSS selectors to scrap
  puesto <- html_nodes(webpage, ".title") %>% html_text(.) %>% as.data.frame()
  #Empresa-Ciudad-Departamento
  #Using CSS selectors to scrap & #Converting the .... data to text
  empresa <- html_nodes(webpage, ".company_compact") %>% html_text(.) %>% as.data.frame()
  # Ubicación (Departamento)
  ubicacion <- html_node(webpage, ".locations_compact .locations_compact") %>% html_text(.) %>% as.data.frame()
  #Using CSS selectors to scrap the time
  fecha <- html_nodes(webpage, ".date_compact") %>% html_text(.) %>% as.data.frame()
  # salario
  salario <- html_nodes(webpage, ".price") %>% html_text(.) %>% as.data.frame()
  # Descripción resumida
  descripcion <- html_nodes(webpage, ".advertise_compact") %>% html_text(.) %>% as.data.frame()
  
  data0 <- data.frame(puesto, empresa, ubicacion, fecha, descripcion, salario)
  data <- rbind(data,data0, stringsAsFactors = FALSE)
  Sys.sleep(1)
  }
}

data <- cbind(data,fecha_scraping = as.POSIXlt(Sys.time()))
data2 <- separate(data, col = EmpDepCiud, sep = "-", into = c("Empresa","Departamento","Ciudad"), extra = "merge", fill = "right")

ruta1 <- "C:/Users/Usuario/Documents/MAESTRIA/scraping/computrabajo/"
ruta2 <- "C:/Users/Usuario/Documents/MAESTRIA/scraping/computrabajo/csv/"
saveRDS(data2, file = paste(ruta1,"/data_computrabajo_", str_replace_all(Sys.time(),":","-"), sep = ''))
write.csv(x = data2 ,file = paste(ruta2, "/data_computrabajo_", str_replace_all(Sys.time(),":","-"),'.csv', sep = ''), 
          row.names = FALSE, quote = TRUE)



