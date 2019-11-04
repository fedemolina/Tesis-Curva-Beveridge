# Datos IECON
paquetes <- c("ggplot2", "magrittr", "dplyr", "data.table")
sapply(paquetes, require, character.only = TRUE)

# En este script genero una serie a nivel 'trimestral' y analizo los datos.

dt <- haven::read_dta(here::here("Datos", "Originales","Gallito-2000-2009.dta")) %>% as.data.table

# Estructura de los datos
dplyr::glimpse(dt)

# Cantidad de NA
sapply(dt, function(x) sum(is.na(x)))       # Hay 24 NA en puestos, despreciable.

# Cantidad de avisos
dt[, .N, by = .(aniog, mesg)][order(aniog,mesg)] %$% plot(x = as.Date(paste(aniog,mesg,1, sep ="-")), 
                                                          y = N, col = "red", type = "o", xlab = "fecha")
# Cantidad de puestos
dt[, sum(puestos, na.rm = TRUE), by = .(aniog, mesg)][order(aniog,mesg),] %$% 
    lines(x = as.Date(paste(aniog,mesg,1, sep ="-")), y = V1, col = "black") +
# Evidente comovimiento como era de esperar. Lo cual da más fuerza al hecho de trabajar con avisos como proxy a los puestos

# Genero la serie de avisos laborales en base a los avisos mensual y trimestral.
    # Por más que con la serie mensual ya se crea la trimestral.


iecon$fecha <- as.Date(paste(iecon$aniog,iecon$mesg, iecon$diag, sep = "-"))
iecon_ts <- iecon %>% group_by(aniog, mesg) %>% summarise(puestos = sum(puestos, na.rm = TRUE),
                                                          avisos = n())
iecon_ts$fecha <- as.Date(paste(iecon_ts$aniog, iecon_ts$mesg,"1", sep = "-"))
iecon_ano <- iecon %>% group_by(fecha = lubridate::make_date(aniog)) %>% summarise(puestos = sum(puestos, na.rm = TRUE),
                                                                                   avisos = n())

ggplot(data = iecon_ts, aes(x = fecha, y = avisos)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = puestos), col = "red") +
    geom_point(aes(y = puestos), col = "red") +
    ggtitle("Avisos y puestos laborales IECON 2000-2009") +
    labs(y = "Cantidad")
