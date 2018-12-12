
library(rvest)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)

data <- data.frame(puesto = character(), EmpDepCiud = character(),fecha=character())
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

for (i in 1:n) {
  #Url to be scraped
  url <- paste("https://www.computrabajo.com.uy/ofertas-de-trabajo/?p=",i, sep = "")
  
  #Reading the html code from the website
  webpage <- read_html(url)
  
  #Using CSS selectors to scrap
  puesto <- html_nodes(webpage, ".js-o-link") %>% html_text(.)
  
  #Empresa-Ciudad-Departamento
  #Using CSS selectors to scrap & #Converting the .... data to text
  EmpDepCiud <- html_nodes(webpage, ".lT") %>% html_text(.)
  #Data-Preprocessing: removing '\r\n'
  EmpDepCiud <- gsub("\r\n ","",EmpDepCiud) %>% gsub(" ","",.) %>% gsub(",","-",.)
  
  #Using CSS selectors to scrap the time
  fecha <- html_nodes(webpage, ".dO") %>% html_text(.)
  
  data0 <- data.frame(puesto = puesto, EmpDepCiud, fecha, stringsAsFactors = FALSE)
  data <- rbind(data,data0, stringsAsFactors = FALSE)

}

data <- cbind(data, fecha_scraping = as.POSIXlt(Sys.time()))
data2 <- separate(data, col = EmpDepCiud, sep = "-", into = c("Empresa","Departamento","Ciudad"), extra = "merge", fill = "right")

ruta1 <- "C:/Users/Usuario/Documents/MAESTRIA/scraping/computrabajo/"
ruta2 <- "C:/Users/Usuario/Documents/MAESTRIA/scraping/computrabajo/csv/"
saveRDS(data2, file = paste(ruta1,"/data_computrabajo_", str_replace_all(Sys.time(),":","-"), sep = ''))
write.csv(x = data2 ,file = paste(ruta2, "/data_computrabajo_", str_replace_all(Sys.time(),":","-"),'.csv', sep = ''), 
          row.names = FALSE, quote = TRUE)

