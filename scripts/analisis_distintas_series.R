# Análisis de las distintas series de vacantes laborales obtenidas.
library(readxl)
library(ggplot2)
library(ggfortify)
library(tidyverse)
library(plotrix)
library(imputeTS)
theme_set(theme_bw())

# El repositorio de datos se encuentra en C:/Users/Usuario/Documents/Bases_Datos_Originales allí hay subdirectorios.
repositorio <-  here::here("Datos", "Originales")
urrestarazu_1981_1995 <- paste(repositorio,"vacantes-1981-1995.xlsx", sep = "/") # Serie trimestral
gallito_2013_2018 <- paste(repositorio,"FMolina-Gallito-1996-1998.xlsx", sep = "/") # Datos a nivel de aviso laboral
ceres_1998_2014 <- paste(repositorio,"ICDL-1998-2014.xls", sep = "/")
fmolina_1996 <- paste(repositorio, "Gallito-1996-1998.xlsx", sep = "/")
iecon_2000_2010 <- paste(repositorio, "Gallito-2000-2009.dta", sep = "/")

# Cargo los distintos conjuntos de datos
urrestarazu <- readxl::read_xlsx(urrestarazu_1981_1995, col_names = TRUE)
urrestarazu_ts <- ts(data = urrestarazu[,2:3], start = c(1981,3), frequency = 4)
# Gráfico número índice de vacantes
autoplot(urrestarazu_ts[,1]) +
    ggtitle("Serie vacantes y desempleo 1981-1995") +
    labs(x = "fecha", y = "Índice de vacantes")
    # año base de urrestarazu? 1980 igual que en Rama.

# Datos recolectados por Molina
molina <- readxl::read_xlsx(fmolina_1996, col_names = TRUE, sheet = "avisos_puestos", na = c(" ", ""))
molina$X__1 <- NULL; molina$X__2 <- NULL; molina$X__3 <- NULL ; molina$X__4 <- NULL
molina$ano <- lubridate::year(molina$f_fin)
molina$mes <- lubridate::month(molina$f_fin)
molina$dia <- lubridate::day(molina$f_fin)
molina$semana <- lubridate::week(molina$f_fin)
molina$puestos_tot <- ifelse(molina$subseccion != "avisos destacados", molina$avisos, molina$total_puestos)
#molina$avisos_cfiltro <- ifelse(molina$subseccion != )
#molina %>% dplyr::filter()

# Crear la serie de avisos con filtro y sin filtro, y la de puestos laborales.
molina_ts <- molina %>% dplyr::group_by(ano,mes) %>% summarise(avisos_f = sum(avisos_filtro, na.rm = TRUE),
                                                               puestos = sum(puestos_tot, na.rm = TRUE),
                                                               avisos_sf = sum(avisos, na.rm = TRUE))
molina_ts <- ts(data = molina_ts[,c("avisos_sf", "puestos")], start = c(1996,1), frequency = 12)
autoplot(molina_ts, colour = TRUE) +
    ggtitle("Avisos y puestos 1996-1997") +
    labs(x = "fecha", y = "Avisos", legend = "", colour = "") # Octubre no se encuentra en la biblioteca.
plot(molina_ts[,"puestos"])
lines(molina_ts[,"avisos_sf"])

# Datos CERES
ceres <- readxl::read_xls(ceres_1998_2014, col_names = TRUE, sheet = "serie", 
                          col_types = c("date","numeric"),
                          cell_cols("A:B"))
ceres_ts <- ts(data = ceres[,2], start = c(1998,3), frequency = 12)
ceres_ano <- ceres %>% 
                group_by(fecha = lubridate::make_date(lubridate::year(fecha))) %>%
                summarise (ind_vacantes = mean(vacantes, na.rm = TRUE))
autoplot(ceres_ts) +
    ggtitle("Índice Ceres de Demanda Laboral 1998-2014") +
    labs(x = "fecha", y = "ICDL")
ggplot(ceres_ano, aes(x = fecha, y = ind_vacantes)) +
    geom_line()
# Datos IECON
iecon <- haven::read_dta(iecon_2000_2010)
iecon$fecha <- as.Date(paste(iecon$aniog,iecon$mesg, iecon$diag, sep = "-"))
iecon_ts <- iecon %>% 
                group_by(aniog, mesg) %>% 
                summarise(puestos = sum(puestos, na.rm = TRUE),
                          avisos = n())
iecon_ts$fecha <- as.Date(paste(iecon_ts$aniog, iecon_ts$mesg,"1", sep = "-"))
iecon_ano <- iecon %>% 
                group_by(fecha = lubridate::make_date(aniog)) %>% 
                summarise(puestos = sum(puestos, na.rm = TRUE),
                          avisos = n())

ggplot(data = iecon_ts, aes(x = fecha, y = avisos)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = puestos)) +
    geom_point(aes(y = puestos)) +
    ggtitle("Avisos y puestos laborales IECON 2000-2009") +
    labs(y = "Cantidad")

# Datos Iecon y Ceres conjuntos a nivel anual (El gráfico es simplemente para ver cierta "correlación", claro prob de escala
#                                           posibilidad de calcular correlación (lineal o no lineal) y graficar en tasas de variación)
twoord.plot(ceres_ano$fecha[1:length(ceres_ano$fecha)-1], diff(ceres_ano$ind_vacantes, 1), 
            iecon_ano$fecha[1:length(ceres_ano$fecha)-1], diff(iecon_ano$puestos,1), ylab = "Índice Ceres Demanda Laboral", rylab = "Puestos solicitados (iecon)", lcol = 4, xlim = c(1998,2015),
            main = "Índice de Demanda Laboral y puestos solicitados", do.first="plot_bg(\"white\");grid(col=\"gray\",lty=1)")

twoord.plot(ceres_ano$fecha, ceres_ano$ind_vacantes, iecon_ano$fecha, iecon_ano$puestos, ylab = "Índice Ceres Demanda Laboral", rylab = "Puestos solicitados (iecon)", lcol = 4, xlim = c(1998,2015),
            main = "Índice de Demanda Laboral y puestos solicitados", do.first="plot_bg(\"white\");grid(col=\"gray\",lty=1)")
plot(ceres_ano, pch = 16)
par(new = TRUE)
plot(iecon_ano[,1:2], col = "red")
# Correlación lineal entre iecon ano y ceres_ano
cor(ceres_ano[3:12,2],iecon_ano[,2]) # Puestos 0.97 de correlación lineal
cor(ceres_ano[3:12,2],iecon_ano[,3]) # Avisos 0.97 de correlación lineal

# Lo mismo del paso previo pero con tasas de variación.
y1 = log(ceres_ts) %>% diff(.)
y2 = log(iecon_ts$puestos) %>% diff(.)
x1 = length(y2)
x2 = length(y2)
plot(x1,y1,ylim=range(c(y1,y2)),xlim= 1:x1, type="l",col="red")
lines(x2,y2,col="green")

largo = 1:(log(ceres_ts) %>% diff(.) %>% length())
log(ceres_ts) %>% diff(.) %>% plot(., type = "l")
par(new = TRUE)
log(iecon_ts$puestos) %>% diff(.) %>% plot(., pch = 16, col = "red")

a = iecon_ano$avisos %>% log(.) %>%  diff(.)
b = ceres_ano$ind_vacantes[3:12] %>% log(.) %>% diff(.)
cor(a,b) # Casi 0.9 de correlación
ceres_ano$ind_vacantes[3:12] %>% log(.) %>% diff(.) %>% 
    plot(., pch = 16, ylim = c(min(a,b),max(a,b)), main = "Tasas de crecimiento anual", 
         xaxt = "n", xlab = "fecha 2000-2009", ylab = "tasa de crecimiento anual")
#par(new = TRUE)
iecon_ano$avisos %>% log(.) %>%  diff(.) %>% points(., col = "red", pch = 16, type = "l")
iecon_ano$puestos %>% log(.) %>%  diff(.) %>% points(., col = "blue", type = "l")
#plot(., col = "red", ylim = c(min(a,b),max(a,b)), xlab = "", xaxt = "n")
legend("bottomright", c("ceres","iecon_avisos","iecon_puestos"), 
       fill = c("black","red","blue"))
# Datos Gallito 2013-2018

#### Imputación ####

molina_ts <- molina %>% dplyr::group_by(semana,ano) %>% summarise(puestos = sum(puestos_tot, na.rm = FALSE),
                                                               avisos = sum(avisos, na.rm = FALSE),
                                                               ) %>% arrange(ano, semana)
molina_ts <- ts(data = molina_ts$avisos, start = c(1996,1), frequency = 52)

imputeTS::plotNA.distribution(molina_ts)
statsNA(molina_ts)
impute_interpolation <- vector()
impute_interpolation$spline <- na.interpolation(molina_ts, option = "spline")
impute_interpolation$lineal <- na.interpolation(molina_ts, option = "linear")
impute_interpolation$stine <-  na.interpolation(molina_ts, option = "stine")
impute_kalman_arima <- na.kalman(molina_ts, model = "auto.arima")
impute_kalman_struc <- na.kalman(molina_ts, model = "StructTS", smooth = TRUE)
#impute_seasonal <- na.seadec(molina_ts, algorithm = "kalman")

par(mfrow = c(1,3))
plotNA.imputations(molina_ts, impute_kalman_arima)
par(new = T)
plotNA.imputations(molina_ts, impute_kalman_struc)
par(new = T)
plotNA.imputations(molina_ts, impute_interpolation$lineal)
par(mfrow = c(1,1));par(new = F)
# En base a la imputación ahora debería pasar la serie a frecuencia mensual y fijar un mes base.
# Luego cambio la base con la serie de CERES. (Posteriormente las junto con el País y puedo imputar para ir viendo)

molina_imputada <- impute_kalman_arima
molina_imputada <- as.matrix(molina_imputada, nrow = length(molina_imputada), ncol = 3) %>% as.data.frame()
molina_imputada$semana <- c(seq(01:52),1:(length(impute_kalman_arima)-52))
molina_imputada$ano <- NA
molina_imputada$ano[1:52] <- 1996
molina_imputada$ano[53:nrow(molina_imputada)] <- 1997
names(molina_imputada)[1] <- "Avisos"
molina_imputada$Avisos <- ceiling(molina_imputada$Avisos)
molina_imputada$fecha <- paste(molina_imputada$ano, molina_imputada$semana, sep = "")
# for(i in 1:9){
#     molina_imputada$fecha[i] <- paste("1996","0",i, sep = "")
# }
# for(i in 53:61){
#     j = i - 52
#     molina_imputada$fecha[i] <- paste("1996","0",j, sep = "")
# }
molina_imputada$fecha <- as.Date(paste(molina_imputada$ano, molina_imputada$semana,1),"%Y %U %u")
molina_imputada$fecha
molina_imputada$mes <- lubridate::month(molina_imputada$fecha)
molina_mensual <- molina_imputada %>% group_by(ano, mes) %>% summarise(avisos = sum(Avisos))

# Serie mensual
# molina_mensual <- ts(molina_imputada, start = c(1996,1), frequency = 12)
# índice molina de avisos base agosto del 1996
indice_molina <- molina_mensual$avisos / molina_mensual$avisos[molina_mensual$ano==1996 & molina_mensual$mes==8]*100
indice_molina <- cbind(indice_molina, molina_mensual$ano, molina_mensual$mes) %>% as.data.frame()
colnames(indice_molina) <- c("indice_molina","ano","mes")
indice_molina <- indice_molina[1:nrow(indice_molina)-1,] # Borro el último mes porque tiene pocas observaciones.

# Ahora lo paso a base agosto del 98 al indice_molina
ceres$fecha <- lubridate::ymd(ceres$fecha)
# Necesito saber el valor de CERES año base (año 98) en la base agosto 96.
#    #1998-08 / #1998-08*100 = 100
# Yo quiero:
#    #1998-08/#1996-08*100 = 1998 en base agosto96.
# Por lo tanto, necesito la cantidad de avisos de agosto del 98.
# Datos de cantidad de avisos gallito agosto del 98
puestos98 <- "C:/Users/Usuario/Documents/Bases_Datos_Originales/F_Molina_Gallito_1998.xlsx"
puestos98 <- readxl::read_excel(puestos98, sheet = 3)
puestos98 <- puestos98[,c("f_fin","avisos")]
sum(puestos98$avisos) # 4300 avisos publicados.
puestos98$mes <- lubridate::month(puestos98$f_fin)

indice_molina <- molina_mensual$avisos / sum(puestos98$avisos) * 100
indice_molina <- cbind(indice_molina, molina_mensual$ano, molina_mensual$mes) %>% as.data.frame()
colnames(indice_molina) <- c("indice_molina","ano","mes")
indice_molina <- indice_molina[1:nrow(indice_molina)-1,] # Borro el último mes porque tiene pocas observaciones.
indice_molina$fecha <- lubridate::as_date(paste(indice_molina$ano,indice_molina$mes,1))

# Como aún me faltan datos futuros para unir las series, voy a imputarlos para poder ver todo el periodo
# Luego tengo que trimestralizar.
lubridate::as_date(paste(urrestarazu$fecha,1))

# Paso el índice de Ceres a cantidad de puestos
ceres$NumVacantes <- ceres$vacantes*sum(puestos98$avisos)/100

# Ahora si puedo unir ceres y molina.
    # Actualmente falta Noviembre, Diciembre, Enero, Febrero y Marzo. Por lo tanto los tengo que generar NA e imputarlo
    # Cuando tenga esos datos, obviamente esta parte desaparece.




# Gallito 2013-2014
gallito <- readxl::read_xlsx(gallito_2013_2018)
ga <- readRDS("C:/Users/Usuario/Documents/MAESTRIA/scraping/analisis_gallito/gallito_2018-12-12.rds")
ga <- readRDS("C:/Users/Usuario/Documents/MAESTRIA/scraping/serie_avisos.rds") # Serie creada previamente.
ga$Fecha <- lubridate::as_date(ga$Fecha)
ggplot(ga, aes(x = Fecha, y = avisos)) + geom_line() + geom_point()
ggplot(ceres, aes(x = fecha, y = vacantes)) + geom_line()
# Comparación ceres y gallito periodo que se superponen.
(ceres %>% filter(fecha < "2014-10-01", fecha > "2013-05-01") %>% dplyr::bind_cols(., 
ga %>% filter(Fecha < "2014-10-01", Fecha > "2013-05-01")) %>% ggplot(., aes(x = fecha, y = NumVacantes)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = avisos, col = "red"), show.legend = F) +
    geom_point(aes(y = avisos, col = "red"), show.legend = F) +
    labs( title = "Avisos laborales",
          subtitle = "El Gallito construcción propia v/s CERES",
          y = "Avisos laborales",
          caption = "Serie construida en base a información de El País con color rojo. Índice de CERES reescalado en base a los avisos de agosto de 1998 con color negro")) %>% 
    plotly::ggplotly(p = .)



# Comparación series con PIB

# save.image("trabajo_guardado")
load("trabajo_guardado")
