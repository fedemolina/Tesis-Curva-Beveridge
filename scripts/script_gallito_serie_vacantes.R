library(dplyr)
library(tidyverse)
library(data.table)
library(here)
library(ggplot2)
library(zoo)
library(lubridate)
library(ggfortify)


input.file.csv <- "Historico avisos trabajo.xlsx"
output_file <- "C:/Users/Usuario/Documents/MAESTRIA/Tesis/GallitoModificado"
path = paste("C:/Users/Usuario/Documents/Bases_Datos_Originales/Gallito_El_Pais",input.file.csv, sep = "/")
df <- data.table::fread(file = input.file.csv, na.strings = c("","-","."))
df <- readxl::read_excel(input.file.csv)
# Cargo el work space
load("work_space.RData")


########### Exploratory Data Analysis ########################

# Fixing data
glimpse(df)
# fecha a formato fecha
df$Fecha <- as.Date(df$Fecha, format = "%d-%m-%Y")
# Renombre variables
colnames(df)[c(4,8,9)] <-  c("PuestoAnunciado","NivelJerarquico","PuestoClase")
# ordeno por fecha
df <- (df[order(df$Fecha)])
# Creo mes-año
df$MesAno <- zoo::as.Date(zoo::as.yearmon(df$Fecha)) #podría dejar con clase yearmon.
# Cantidad de missing
summary(sapply(df, is.na))
# Ahora paso a transformar a factor las variables. Pero necesito saber cuántos casos diferente tiene cada c/u
# Las que tengan demasiados casos no vale la pena porque no las puedo analizar como factor. Salvo que haga una agrupación.
a <- apply(df, MARGIN = 2, FUN = function(x) length(unique(x))) < 25
# Variables a ser transformadas en factor
names(a[a])  
which(a)
# Acá quiero hacer una linea que dados los que cumplen la condición de tener menos de 25 casos, luego los retorne les
# modifique la clase a factor sin tener que escribirlos manualmente.
# df[names(a[a])] <- lapply(df[], FUN = as.factor)# Estudio
glimpse(df)                                       # NivelJerarquico
                                                  # PuestClase
                                                  # Descripcion
                                                  # Ubicación
                                                  # Experiencia
                                                  # Usando los que tienen menos de 25 casos.

df$Area <- as.factor(df$Area)
# "Puesto" debería ser un factor pero tiene demasiados valores
# 

# Tablas
round(prop.table(ftable(lubridate::year(df$Fecha),df$PuestoClase,df$Experiencia)),2)
prop.table(table(lubridate::year(df$Fecha),df$PuestoClase,df$Experiencia), margin = 3)
prop.table(table(lubridate::year(df$Fecha),df$PuestoClase,df$Experiencia), margin = c(1,3))
table(df$Area)

# Gráficos

# Avisos ordenados
(table(df$Area, deparse.level = 2, dnn = "Area") %>% 
  as.data.frame(., responseName = "Avisos") %>%
  ggplot(., aes( x = reorder(Area, +Avisos), y = Avisos)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 6)) +
  labs(x = "Área", y = "Cantidad de Avisos")) %>% 
  plotly::ggplotly()
# La mayoria se centra en servicios y oficios.
# Luego ventas - comercial
# Lo mismo del gráfico anterior pero en formato tabla, retornando los n primeros a elegir en la función head.
table(df$Area, deparse.level = 2, dnn = "Area") %>% 
  as.data.frame(., responseName = "Avisos") %>% 
  arrange(desc(Avisos)) %>% head(., n=5)

# Evolución de nivel jerarquico.
df %>% 
  group_by(MesAno, NivelJerarquico) %>% 
  summarise(avisos = n())

## Serie temporal de avisos
df$uno = 1 
df$id = seq.int(nrow(df))
serie = df %>% 
  group_by(Fecha = MesAno) %>%
  summarise(avisos = sum(uno))
saveRDS(serie, "serie_avisos.Rds")

# Finish of EDA, save the work space.
save.image(file = "work_space.RData")
