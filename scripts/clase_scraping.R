library(tidyverse)
library(rvest)
read_html("ejemplo.html") %>%
html_nodes("div.output") %>% html_text()

read_html("ejemplo.html") %>%
  html_nodes("div > pre") %>% html_text()

pesca <- html("https://www.imdb.com/title/tt1388402/?ref_=nv_sr_1")
pesca %>% html_node("strong span") %>% html_text() %>% as.numeric()

### Verificar que es legal, en el sentido que la página lo permite.
library(robotstxt)
paths_allowed("https://www.imdb.com/title/tt1388402/?ref_=nv_sr_1")

#### Leer conveniios colectivos

#IMPO / decreto n°103//009
link <- "www.impo.com.uy/convenios/109-009.pdf"

# Traemos todos los link que tienen formato pdf
link2 <- "http://www.impo.com.uy/bancodatos/v01.htm#el"
prueba <- link2 %>% read_html() %>% 
                    html_nodes("a") %>% 
                    html_attr("href") %>% 
                    as_data_frame() %>% 
                    filter(str_detect(value, '.pdf'))
#Obs: html_attrs seleciona TODOS los atributos
#Obs: html_attr selecciona el atributo que yo le indico.
links.info <- prueba %>% 
  mutate(aux = str_extract_all(value, pattern = '[[:digit:]]+')) %>% #aux tiene clase lista por eso uso map.Podría usarse lapply.
  mutate(doc = map(aux, paste0,collapse="-")) %>%
  slice(1:2)

links.info <- prueba %>% 
  mutate(aux = str_extract_all(value, pattern = '[[:digit:]]+')) %>% #aux tiene clase lista por eso uso map.Podría usarse lapply.
  mutate(doc = map(aux, paste0,collapse="-")) %>%
  unnest(doc) %>%
  mutate( destino = paste0("convenios/", doc, "pdf", sep = "")) %>%
  select(-aux, -doc)

# En vez de hacer eso, se puede usar basename(links)
basename(prueba[[1]][1])

# Podemos agregar una pausa entre cada documento
for(i in 1:3){
  Sys.sleep(2)
  if(!file.exist(links.info$destino[i]))
    download.file(url = links.info$value[i],
                  destfile = links.info$destino[i])
  
}

# Texto como datos
library(pdftools)
library(tidytext)

#xx <- pdf_text(paste("../", links.info$destino[2], sep = ""))
xx <- pdf_text(links.info$destino[2])
