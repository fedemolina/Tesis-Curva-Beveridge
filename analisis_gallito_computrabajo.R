library(tidyverse)
library(dplyr)
# Análisis de datos obtenidos por scraping de El galito y de Computrabajo
  # 1) galito
  # 2) Computrabajo

theme_set(theme_minimal())
#### 1. Análisis galito ####
if (dir.exists("analisis_galito") == FALSE) {
  dir.create("analisis_galito")
}
if (dir.exists("analisis_computrabajo") == FALSE) {
  dir.create("analisis_computrabajo")
}

ga <- readRDS(paste(getwd(),'/gallito/',dir('gallito')[9],sep = ""))

ga$num <- 1

## EDA ###
# Análisis de fecha
ga$fecha <- gsub("Hace ","", ga$fecha) %>% 
             gsub("días", "día", .) %>% 
             gsub("horas", "hora", .) %>% 
             gsub("semanas", "semana", .)

ga <- separate(ga, col = fecha, sep = " ", into = c("cantidad","unidad"))
ga$cantidad <- as.numeric(ga$cantidad)
# Convierto a segundos
minuto = 60
hora = 60*minuto
dia = 24*hora
semana = 7*dia
mes = 30*dia
ga <- ga %>% mutate(unidad_seg = ifelse(.$unidad == "minutos",minuto,
                            ifelse(.$unidad == 'hora',hora,
                            ifelse(.$unidad == 'día',dia,
                            ifelse(.$unidad == 'semana',semana,
                                   mes)))),
                      fecha_pub = .$fecha_scraping - (.$cantidad*unidad_seg))
ga <- ga %>% separate(., col = fecha_pub, into = c("fpub","hpub"), sep = " ")
ga$fpub <- as.Date(ga$fpub, format = "%Y-%m-%d")
ga$cantidad <- NULL
ga$unidad <- NULL
ga$unidad_seg <- NULL
ga$hpub <- NULL
ga$dia <- factor(weekdays(ga$fpub), levels = c("lunes","martes","miércoles","jueves","viernes",
                                               "sábado","domingo"), ordered = TRUE)
ga$nivel <- factor(ga$nivel)
ga$area <- factor(ga$area)
ga$dpto <- factor(ga$dpto)
glimpse(ga)
summary(ga)

dir('galito')[9] # 12/12/2018
ruta <- "C:/Users/Usuario/Documents/MAESTRIA/scraping/analisis_galito/"
# Guardo la base modificada
saveRDS(ga, file = paste(ruta,'gallito_2018-12-12',sep = ""))
saveRDS(ga, file = 'entrega_final_gallito_2018-12-12') # Guardo para la entrega 

# Sigo el EDA (respecto a fecha)
ga %>% ggplot(., aes(x = dia, y = ..count..)) +
  geom_bar(aes(fill= ..count..)) +
  labs(x = "", y = "cantidad de avisos", 
       title = "Avisos laborales por día de la semana",
       caption = "Cantidad de avisos laborales publicados en portal web 'El Galito' 12/Nov - 12/Dic 2018",
       fill = "Cantidad\n avisos")

ga %>% count(fpub) %>% ggplot(., aes(x=fpub, y = n)) +
  geom_line(color = "#FC4E07", size = 2) +
  labs(x = 'fecha de publicación', y = 'cantidad de avisos', 
       title = "Avisos laborales por fecha de publicación",
       caption = "Cantidad de avisos laborales publicados en portal web 'El Galito' 12/Nov - 12/Dic 2018. La fecha de publicación no es exacta")

# Análisis de Área
ga %>% ggplot(., aes(x = fct_infreq(f = area, ordered = TRUE))) +
  geom_bar(aes(fill = ..count..)) +
  labs(y = "cantidad de avisos", x = "Áreas", fill = "cantidad", 
       title = "Áreas de actividad solicitadas",
       caption = "Cantidad de avisos laborales publicados en 'El Galito' 12/Nov - 12/Dic 2018") +
  coord_flip() +
  geom_hline(yintercept = c(50,100,150), linetype = "dotted", 
             color = "red", size = 1)

# Análisis de nivel
ga %>% ggplot(., aes(nivel)) +
  geom_bar()+
  coord_polar(theta = "x",direction = 1, start = -50) +
  labs(y = "Cantidad de avisos", x = "Nivel" , title = "Avisos laborales portal web 'El Gallito'",
       caption = "Avisos laborales 'El Gallito' 12/Nov - 12/Dic 2018. Nivel técnico solicitado por empresas")

# area y nivel
ga  %>%  
  ggplot(., aes(area, nivel)) +
  geom_count(aes(color = ..n.., size = ..n..)) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y = "Nivel", x = "Área", title = "Nivel técnico y Área laboral", 
       size = "cantidad\n avisos", color = "cantidad\n avisos",
       caption = "Avisos laborales publicados en portal web 'El Gallito' entre 12/Nov - 12/Dic de 2018. Área y nivel técnico solicitado")

# dpto 
table(ga$dpto)
ga %>% ggplot(., aes(dpto)) +
  geom_bar() +
  coord_polar()

#### 2. Análisis computrabajo ####

ct <- 