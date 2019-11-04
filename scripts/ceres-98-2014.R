libs = c("readxl", "ggplot2", "ggfortify", "dplyr", "plotrix", "imputeTS", "lubridate", "inspectdf", "magrittr", "plotly", "data.table")
# libs1=c("tidyverse", "fpp2", "gridExtra", "xts", "seasonal", "lmtest", "tsoutliers")
lib_nuevas <- libs[!(libs %in% installed.packages()[,"Package"])] 
if(length(lib_nuevas)) install.packages(lib_nuevas)
load_libs <- function(libraries = libs) for (lib in libraries) 
    require(lib, character.only=T)
load_libs(libs)
theme_set(theme_bw())

datos    <- here::here("Datos", "Originales", "ICDL-1998-2014.xls")
ceres    <- readxl::read_excel(datos, sheet = "serie", range = cell_cols("A:B")) %>% 
                 as.data.table(.)
str(dt)
# Avisos mes base: agosto 98
dt_agosto <- readxl::read_excel(here::here("Datos", "Originales", "Gallito-1998-08.xlsx"), 
                                sheet = "avisos_puestos", range = "A1:I31") %>% 
                as.data.table(.)

# Nota metodológica de ceres dice:
# Para la construcción del Índice CERES de Demanda Laboral (ICDL) se hace un relevamiento de las solicitudes de trabajo
# publicados en la sección de clasificados de la prensa uruguaya.
# Con esta información se elabora un indicador mensual que compara el número de PEDIDOS de trabajo en un mes dado,
# con el respectivo número de PEDIDOS de trabajo de un mes considerado como base. El mes base seleccionado para
# nuestro índice es agosto de 1998
# 
# Entonces la pregunta clave es que entienden por número de PEDIDOS de trabajo. Avisos laborales? Puestos laborales?
# Debería trabajar con las dos hipótesis y ver cual ajusta mejor.

# Lo que puedo hacer es comparar gallito 2013 v/s ceres 2013 para ver cuanta diferencia entre las cantidad de avisos tengo.
# Puedo comparar con puestos, lo cual es más probable.
# En cualquier caso las diferencias las puedo ajustar
# O compararlo con los avisos recolectados en biblioteca Nacional para los períodos que coinciden
# Lo correcto sería probar ambas.
avisos <- dt_agosto[, .(f_ini, f_fin, avisos)]
# Hay 1 día de agosto al comienzo
# Hay 2 días de agosto al final
avisos[, avisos_d := avisos/7]
avisos[, avisos_c := avisos]

avisos[month(f_ini) < month(f_fin) & lubridate::month(f_fin) <= min(lubridate::month(f_fin)), avisos_c := avisos_d * 1]
avisos[month(f_ini) < month(f_fin) & lubridate::month(f_fin) > min(lubridate::month(f_fin)), avisos_c := avisos_d * 2]

# Avisos totales por día
base98 <- avisos[, sum(avisos_c)] # 4756
# Avisos totales sin corregir
avisos[, .(avisos = sum(avisos))]   # 6424
# avisos_a: avisos recolectados por ceres (recordar que dice NUMERO DE PEDIDOS DE TRABAJO)
ceres[, avisos_a := (vacantes * base98)/100]

# avisos_p: avisos de puestos laborales
    # pendiente
#

# Serie mensual del 13-18 del gallito.
serie13 <- readRDS(here::here("Datos", "Intermedias", "s_mensual13-18.rds"))

# Fechas de coincidencia entre ceres y los avisos del gallito.
summary(ceres)   # Máximo fecha en 2014-10
summary(serie13) # Mínimo es 2013-05. Hay que tomarlo desde 2013-06 porque en mayo faltan días.

ceres[, fecha := as.Date(fecha)]
serie13[, fecha := as.Date(fecha)]

setkey(ceres, "fecha")
setkey(serie13, "fecha")
names(ceres)
names(serie13)
serie13[ceres, on = "fecha", `:=`(vacantes_ceres = vacantes,
                                  avisos_ceres = avisos_a)] #, vacantes_ceres := vacantes]
# between(fecha, "2013-06-01", "2014-10-01")
# Me quedo solamente con 2013-06 al 2014-10
serie13[, ] %>% 
    ggplot(., aes(x = fecha, y = avisos_ceres)) +
    geom_line() +
    geom_line(aes(y = avisos))
# La diferencia entre una serie y la otra puede tener las siguientes explicaciones:
# 1. CERES tomaba mas fuentes de información
# 2. CERES contabilizaba PUESTOS no avisos laborales.
    # Si este fuese el caso y solamente tomaran datos del gallito, las diferencias deberían ser notoriamente menores.

# Obtengo un ratio en un año entero de la diferencia entre las series de avisos
serie13[between(fecha, "2013-06-01", "2014-10-01"), .(fecha, cociente = avisos_ceres/avisos)]

# Desestacionalizar avisos del gallito para probar
ga_ts <- ts(serie13[fecha > as.Date("2013-05-01"), avisos], start = c(2013, 6), frequency = 12)
plot(ga_ts)

ga_adj <- seasonal::seas(ga_ts, transform.function = "none") # Sin transformación para ver la cantidad de avisos del estacional
plot(ga_adj)
ga_adj$x %>% plot()
ga_adj$series
ga_adj$data
serie13[, avisos_ceres*(ga_adj$data[, "seasonal"] %>% as.data.frame)] %>% 
    plot(.)
ga_adj$data %>% plot
ga_adj$data[, "seasonal"]
serie13[, .(avisos, fecha)] 
monthplot(ga_adj)
ga_adj[["data"]][, "seasonaladj"]
ga_adj[["data"]][, "adjustfac"]
round(serie13[fecha > as.Date("2013-05-01"), avisos] - ga_adj[["data"]][, "seasonaladj"]) == round(ga_adj[["data"]][, "adjustfac"])
# Ok. Entonces la serie original - adjustfac me da la serie final desestacionalizada
# Donde el adjustfac = S + I + algo más
# Entonces una aproximación a la serie de avisos de ceres va a ser restarle el promedio mensual del adjustfac 

# Genero los promedios mensuales
c(rep(NA,5),ga_adj[["data"]][, "adjustfac"], rep(NA, 3)) %>% length()
prom_mensual <- c(rep(NA,5),ga_adj[["data"]][, "adjustfac"], rep(NA, 3)) %>%
                matrix(., ncol = 12, byrow = TRUE) %>% 
                apply(., MARGIN = 2, mean, na.rm = TRUE) %>% 
                data.table(adj_mensual = ., mes = seq.int(1,12,1))

setkeyv(serie13, c("fecha", "mes"))
setkey(prom_mensual, "mes")

serie13[, .(avisos, fecha)]
serie13[prom_mensual, on = "mes", `:=`(avisos_ceres_c = (avisos_ceres + adj_mensual),
                                       adj_mensual = adj_mensual)]

# Agregar a la serie de ceres y dejar avisos_a_c (avisos_avisos_corregidos) por estacionalidad
# Le podría agregar un ruido blanco gausiano (pese a que estoy agregando en adj_mensua el componente irregular)?
ceres[, mes := month(fecha)]
setkeyv(ceres, c("fecha", "mes"))
ceres[prom_mensual, on = "mes", `:=`(avisos_a_c = avisos_a + adj_mensual,
                                     adj_mensual = adj_mensual)]
# Observación:
# Queda una estacionalidad determinística agregada a a serie. Discutible
# Esa estacionalidad no es capaz de generar quiebres estructurales que afecten las conclusiones del trabajo.

saveRDS(ceres, here::here("Datos", "Finales", "ceres_corregido.rds"))








# IMPORTANTE
# Preguntar a Silvuia/Buscar como se calcula exactamente con la función seas
# Porque si fuese Serie = T + S + C + I
# Prueba desestacionalizar con dummies a ver como queda.

# Obtengo un ratio pero con los valores desestacionalizados del gallito


# Análisis funcional
# dtc[, .(diff(vacantes), diff(vacantes,2))] %$% 
#     plot(x = V1, y = V2, main = "Índice de vacantes laborales", xlab = "velocidad", ylab = "aceleración"); lines(x = -10:10, y = rep(0,21));lines(x=rep(0,25), y = -12:12)
