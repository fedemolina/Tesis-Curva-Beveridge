# Datos y paquetes --------------------------------------------------------
libs=c("readxl", "ggplot2", "ggfortify", "dplyr", "lubridate", "inspectdf", "magrittr", "plotly", "data.table")
lib_nuevas <- libs[!(libs %in% installed.packages()[,"Package"])] 
if(length(lib_nuevas)) install.packages(lib_nuevas)
sapply(libs, require, character.only = TRUE)
theme_set(theme_bw())
options(max.print = 1000)

# En este script se trabaja con las series de:
# Urrestarazu 81-95 Trimestral              dtu
# Molina      96-98 Semanal                 dtm
# Molina    Agos-98 Semanal                 dta
# Ceres       98-14 Mensual                 dtc
# Iecon       00-09 sin frecuencia fija     dti
# ElPais      13-18 Frecuencia semanal      dte

# Se van a unir las serie de Urrestarazu, Molina, Ceres y ElPais para lograr vacantes desde 1981 hasta 2018.
# Iecon solamente se utiliza para analizar el movimiento y correlación con la serie de ceres en el periodo que coinciden
# La serie de Agosto-98 se usa porque es el año base del índice de ceres y puede usarse para comparar la cantidad de avisos.

repo_o <- here::here("Datos", "Originales")
repo_i <- here::here("Datos", "Intermedias")
repo_f <- here::here("Datos", "Finales")

# Datos

dtu <- readxl::read_excel(paste0(repo_o,"/vacantes-1981-1995.xlsx")) %>% as.data.table()
# dtm <- readRDS(paste0(repo_i,"/s_mensual95-98.rds"))
dtm <- readRDS("./Datos/Finales/serie_mensual_95-01.rds")
dtm_trim <- readRDS("./Datos/Finales/serie_trim_95-01.rds")
dtc <- readxl::read_excel(paste0(repo_o,"/ICDL-1998-2014.xls"), range = readxl::cell_cols("A:B"), sheet = "serie") %>% 
  as.data.table(.)
ceres <- readRDS(paste0(repo_f, "/ceres_corregido.rds"))        # Script ceres-98-2014.R
dti <- haven::read_dta(paste0(repo_o,"/Gallito-2000-2009.dta")) %>% as.data.table()
dte <- readRDS(paste0(repo_f,"/serie_trimestral_ga_13-19.rds"))         # Script serie_gallito_2013_2019.R
dta <- readRDS(paste0(repo_i, "/agosto98.rds"))
dt <- readRDS(paste0(repo_f, "/estimacionPEA.rds"))


# Obtención avisos en t=0, calcular base 1980 y obtención avisos 80-95 -------------------------------------------------

# Paso 1. Obtener a_t en algún punto: a_1995_q4
# Paso 2. Imputar la PEA hacia atrás -> Obtengo PEA_t=0
# Paso 3. Obtener a_t=0:
# v_t = (a_t/pea_t)/(a_t=0/pea_t=0)*100
# Paso 4. Obtener la base alpha = a_t0/PEA_t0
# Paso 5. Imputar las vacantes hacía atras
# Paso 6. Calcular la serie de avisos

# Paso 1.
a_95_q4 = dtm[ano_c == 1995 & q_c == 4, sum(avisos)]


# Paso 2.
temp <- ts(c(rep(NA, 6), dtu[, PEA]), start = c(1980, 1), frequency = 4)
imputeTS::plotNA.distribution(temp)
imputeTS::statsNA(temp)

impute_interpolation <- vector()
impute_interpolation$spline <- imputeTS::na_interpolation(temp, option = "spline")
impute_kalman_arima         <- imputeTS::na_kalman(temp, model = "auto.arima")
impute_kalman_struc         <- imputeTS::na_kalman(temp, model = "StructTS", smooth = TRUE)

# Me tomo el promedio de los 3 tipos de imputaciones
extrac_imp <- function(x) {
  window(x, start = c(1980,1), end = c(1981,2))
}
est1 <- extrac_imp(impute_kalman_arima)
est2 <- extrac_imp(impute_kalman_struc)
est4 <- extrac_imp(impute_interpolation$spline)

estimaciones80 <- matrix(c(est1[1:4], est2[1:4], est4[1:4]), ncol = 4, nrow = 3, byrow = TRUE) %>% 
  apply(., MARGIN = 2, mean, na.rm = TRUE)
estimaciones81 <- matrix(c(est1[5:6], est2[5:6], est4[5:6]), ncol = 2, nrow = 3, byrow = TRUE) %>% 
  apply(., MARGIN = 2, mean, na.rm = TRUE)

series = data.table(pea = c(estimaciones80, estimaciones81, dtu[, PEA]), 
                    fecha = seq.Date(from = as.Date("1980/01/01"), to = as.Date("1995/10/01"), by = "quarter"))
series[, plot(y = pea, x = fecha, type = "l")]

# Paso 3.
# v_t = (a_t/pea_t)/(a_t=0/pea_t=0)*100    Para t=1995, q4
# despejo a_t=0:
# a_t0 = (a_t * PEA_t0 / v_t * PEA_t) * 100
a_t    = a_95_q4
pea_t0 = series[year(fecha) == 1980, sum(pea)]*1000
v_t    = dtu[NROW(dtu), vacantes]
pea_t  = series[fecha == as.Date("1995-10-01"), pea]*1000

a_to   = ((a_t * pea_t0) / (v_t * pea_t)) * 100

# Paso 4.
# alpha = a_to/pea_t0 * 100
pea_t80 = series[year(fecha) == 1980, sum(pea)]*1000
alpha = a_to/pea_t80

# Paso 5.
temp <- ts(c(rep(NA, 6), dtu[, vacantes]), start = c(1980, 1), frequency = 4)
imputeTS::plotNA.distribution(temp)

impute_interpolation <- vector()
impute_interpolation$spline <- imputeTS::na_interpolation(temp, option = "spline")
impute_kalman_arima         <- imputeTS::na_kalman(temp, model = "auto.arima")
impute_kalman_struc         <- imputeTS::na_kalman(temp, model = "StructTS", smooth = TRUE)

est1 <- extrac_imp(impute_kalman_arima)
est2 <- extrac_imp(impute_kalman_struc)
est4 <- extrac_imp(impute_interpolation$spline)
par(mfrow = c(1,1))
minimo = min(est1, est2, est4)
maximo = max(est1, est2, est4)
plot(est1, ylim = c(minimo, maximo))
lines(est2)
lines(est4)
# Lo más sensato considerando que en el 82 es la crisis de la tabita parece ser est1, la estimación por kalman.
# O una combinación de kalman + arima

estimaciones80 <- matrix(c(est1[1:4], est2[1:4], est4[1:4]), ncol = 4, nrow = 3, byrow = TRUE) %>% 
  apply(., MARGIN = 2, mean, na.rm = TRUE)
estimaciones81 <- matrix(c(est1[5:6], est2[5:6], est4[5:6]), ncol = 2, nrow = 3, byrow = TRUE) %>% 
  apply(., MARGIN = 2, mean, na.rm = TRUE)
plot(est1, ylim = c(minimo, maximo))
lines(ts(estimaciones80, start = c(1980, 1), frequency = 4))
lines(ts(estimaciones81, start = c(1981, 1), frequency = 4))

estimaciones80 <- matrix(c(est1[1:4], est2[1:4]), ncol = 4, nrow = 2, byrow = TRUE) %>% 
  apply(., MARGIN = 2, mean, na.rm = TRUE)
estimaciones81 <- matrix(c(est1[5:6], est2[5:6]), ncol = 2, nrow = 2, byrow = TRUE) %>% 
  apply(., MARGIN = 2, mean, na.rm = TRUE)
plot(est1, ylim = c(minimo, maximo))
lines(ts(estimaciones80, start = c(1980, 1), frequency = 4))
lines(ts(estimaciones81, start = c(1981, 1), frequency = 4))

series[, vacantes := c(estimaciones80, estimaciones81, dtu[, vacantes])]
# series[, vacantes := c(est1, dtu[, vacantes])] # La caída de esta forma es muy importante desde el 80,
# opto por una combinación de los 2 métodos que muestra una caída relevante pero más suave.

# Paso 6. Calcular la serie de avisos
series[, avisos := (vacantes/100)*alpha*pea*1000]

# Análisis de la serie obtenida.
series[, {
  par(mfrow = c(2,1))
  plot(x = fecha, y = vacantes, type = "l")
  plot(x = fecha, y = avisos, type = "l")
}] # Bien!!
series[, plot_ly(.SD) %>% 
         add_trace(y = avisos,
                   x = fecha,
                   mode = "lines+markers")]

# Paso a trabajar y recabar la información en el objeto dt que contiene la pea estimada para 1980-2018
# Dicha PEA no se debía usar para calcular los avisos, puesto que las vacantes fueron calculados con la PEA usada en 
# el trabajo de urrestarazu y rama.
dt[between(year(fecha), 1980, 1995), `:=`(avisos = series[, avisos],
                                          vacantes = series[, vacantes])]

# Unión serie Urrestarazu (80-95) y Molina (95-98) ----------------------------------------------
dt[fecha == as.Date("1995-10-01"), ]
dtm[ano_c == 1995 & q_c == 4, sum(avisos)]
dtm[, sum(avisos), by = .(ano_c, q_c)]
# Observar la estacionalidad
dtm[fecha > "1995-09-01" & fecha < "1998-07-01" , plot_ly(.SD) %>% 
      add_trace(x = fecha, y = avisos, mode = "lines+markers")]
# Gráficar los trimestres
dtm[, q_c_fecha := lubridate::quarter(fecha)
    ][q_c == 2, q_c_fecha := 4
      ][q_c == 3, q_c_fecha := 7
        ][q_c == 4, q_c_fecha := 10
          ][, fecha_q := as.Date(paste(ano_c, q_c_fecha, 01, sep = "-"))]
dtm[fecha > "1995-09-01" & fecha < "1998-07-01" , .(avisos = sum(avisos)), by = .(fecha_q)
    ][, plot_ly(.SD) %>% 
        add_trace(x = fecha_q, y = avisos, mode = "lines+markers")]

# Los valores coinciden, porque utilice los avisos de molina(95-q4) para poder calcular los avisos a partir de
# las vacantes de urrestarazu, por lo tanto se pueden unir las series.

# dt[data.table::between(fecha, as.Date("1996/01/01"), as.Date("1998/04/01")), 
#    avisos := dtm[, .(avisos = sum(avisos)), by = .(ano_c, q_c)
#                  ][, .(avisos, fecha = seq.Date(as.Date("1995/07/01"), as.Date("1998/07/01"), "quarter"))
#                    ][data.table::between(fecha, as.Date("1996/01/01"), as.Date("1998/04/01")), avisos]
#    ]
dt[data.table::between(fecha, as.Date("1996/01/01"), as.Date("2001/01/01")),
   avisos := dtm[data.table::between(fecha, as.Date("1996/01/01"), as.Date("2001/03/01")),
                 .(avisos = sum(avisos)), by = .(ano_c, q_c)
                 ][, avisos]]
dt[, plot_ly(.SD) %>% 
     add_trace(x = fecha, y = avisos, mode = "lines+markers")]

# Unión serie 80-98 y Ceres (98-2014) -------------------------------------

# Coinciden en 1998-q2
dt[fecha == as.Date("1998-04-01"), avisos]
ceres_q <- ceres[between(fecha, as.Date("1998-04-01"), as.Date("2014/09/01")), 
                 .(season_kal = sum(season_kal),
                   season_kal_adj = sum(season_kal_adj),
                   season_arima = sum(season_arima),
                   season_arima_adj = sum(season_arima_adj),
                   avisos = sum(avisos_a)), by = .(year(fecha), quarter(fecha))]
ceres_q[, fecha := seq.Date(as.Date("1998/04/01"), as.Date("2014/07/01"), "quarter")]
ceres_q

# NUEVO!
# Agrego la serie corregida y sin corregir al objeto dt, con el fin de tener todas las serie en un solo lugar.
# Genere las estacionalidad y estacionalidades + irregulares + outliers, con kalman y con arima.
setkey(ceres_q, "fecha")
setkey(dt, "fecha")
setnames(ceres_q, old = "avisos", new = "avisos_ceres")
cols <- names(ceres_q)[grepl(names(ceres_q), pattern = "season|avisos")]
dt[ceres_q, (cols) := mget(cols)]
# dt[fecha %in% ceres_q$fecha, `:=`(avisos_ceres = ceres_q$avisos,
#                                   season_kal = ceres_q$season_kal,
#                                   season_kal_adj = ceres_q$season_kal_adj,
#                                   season_arima = ceres_q$season_arima,
#                                   season_arima_adj = ceres_q$season_arima_adj)]

dt[, plot_ly(.SD) %>% 
     add_trace(x = fecha, y = avisos, mode = "lines+markers", type = "scatter", name = "UM") %>% 
     add_trace(x = fecha, y = avisos_ceres, mode = "lines+markers", type = "scatter", name = "ceres") %>% 
     add_trace(x = fecha, y = avisos_ceres + season_kal, mode = "lines+markers", type = "scatter", name = "ceres_kal") %>% 
     add_trace(x = fecha, y = avisos_ceres + season_kal_adj, mode = "lines+markers", type = "scatter", name = "ceres_kal_adj") %>% 
     add_trace(x = fecha, y = avisos_ceres + season_arima, mode = "lines+markers", type = "scatter", name = "ceres_kal") %>% 
     add_trace(x = fecha, y = avisos_ceres + season_arima_adj, mode = "lines+markers", type = "scatter", name = "ceres_kal_adj") %>% 
     add_trace(data = dtm_trim, x =~ fecha, y =~ avisos_dest, mode = "lines+markers", type = "scatter", name = "ceres")
   ]


# Comentado porque pienso hacerlo al final, en el script generacion_serie_vacantes, ya que, ahora tengo 4 estacionalidades.

# Genero un ponderador para unirlas. Recalculo la serie de ceres, ya que, conozco mejor la serie de urrestarazu.
# ponderador_sin_c = dt[fecha == as.Date("1998-04-01"), avisos]/ceres_q[fecha == as.Date("1998-04-01"), avisos]
# ponderador_con_c = dt[fecha == as.Date("1998-04-01"), avisos]/ceres_q[fecha == as.Date("1998-04-01"), avisos_corregidos]
# 
# # ponderador_sin_c = dt[fecha == as.Date("1998-07-01"), avisos]/ceres_q[fecha == as.Date("1998-07-01"), avisos]
# # ponderador_con_c = dt[fecha == as.Date("1998-07-01"), avisos]/ceres_q[fecha == as.Date("1998-07-01"), avisos_corregidos]
# # Son lo mismo, esta clarísimo.
# 
# dt[data.table::between(fecha, "1998/04/01", "2014/07/01"), avisos_union := ceres_q[, avisos*ponderador_sin_c]
#    ][fecha < "1998/04/01", avisos_union := avisos]
# dt[between(fecha, as.Date("1998/04/01"), as.Date("2014/07/01")), avisos_union_c := ceres_q[, avisos_corregidos*ponderador_con_c]
#    ][fecha < "1998/04/01", avisos_union_c := avisos]
# dt[, plot_ly(data = .SD) %>% 
#        add_lines(x = fecha, y = avisos) %>% 
#        add_lines(x = fecha, y = avisos_ceres) %>%
#        add_lines(x = fecha, y = avisos_ceres_c) %>%
#        add_lines(x = fecha, y = avisos_union) %>% 
#        add_lines(x = fecha, y = avisos_union_c)
#    ]
# La caída comienza en el año 94, no es un problema de unión de series y los datos desde 89 al 95 son confiables dado que fueron
# suministrados directamente por el país.
# Recordar que la serie de Ceres puede estar sobrecalculada si el valor usado en el año base es muy grande.
# Serie buena practica probar y calcular con más de 1 valor. Tener 3 series de ceres y realizar el análisis para las 3
# Agregar eso aquí:

#  P E N D I E N T E

# Guardo la serie de avisos desde 80-14 para analizar diferencias con los avisos del gallito.
# saveRDS(dt, here::here("Datos", "Intermedias", "avisos_80-2014.rds")) 
saveRDS(dt, here::here("Datos", "Intermedias", "avisos_80-2014.rds"))  # CAMBIAR NOMBRE LUEGO! Ya lo cambie

# Unión serie 80-2014 y datos Gallito (2013-2018) -------------------------

# No tomar en cuenta en 2013 el mes 5. Por tanto, arrancar en 2013-q3
# 2018-q3 esta completo. ---> 2013-q3 al 2018-q3
# Voy a reponderar para que quede acorde a los avisos de 2013-2018

# Ceres: vt = a_t/a0*100
# Quiero ver cuánto debería ser a0 para que coincidan las series en 2013-q3? Pero ceres es mensual (y desestacionalizado)
temp <- readRDS(here::here("Datos", "Finales", "serie_mensual_ga_13-19.rds"))   # Esta serie se podría calcular en este mismo script.
temp[data.table::between(fecha, "2013-06-01", "2014-10-01"), ]
temp[, {par(mfrow = c(1,1))
  y = c(min(avisos_s_dup, avisos_c_dup, na.rm = T), max(avisos_s_dup, avisos_c_dup, na.rm = T))
  plot(x = fecha, y = avisos_c_dup, ylim = y, type = "l")
  par(new = T)
  plot(x = fecha, y = avisos_s_dup, ylim = y, type = "l")
  # par(new = T)
  # plot(x = fecha, y = avisos_c_dup_dest  , ylim = y, type = "l")
  # par(new = T)
  # plot(x = fecha, y = avisos_s_dup_dest  , ylim = y, type = "l")
}]
# Llamativo que se agranda con los años.

# "2013-05-01" hasta 2014-10-01
v_t = ceres[data.table::between(fecha, "2013-06-01", "2014-10-01"), vacantes] 
a_t = temp[data.table::between(fecha, "2013-06-01", "2014-10-01"), avisos_c_dup]
a_0 = a_t/v_t*100
a_0     

# Recodar que CERES dice estar desestionalizado, cuando en realidad es más bien una tendencia.
# El resultado de la supuesta base varía.
# Tiene sentido, porque la serie de vacantes de CERES es desestacionalizada (o es la tendencia)
# Por lo que no se observa el verdadero a_t

# Ahora pasa a la unión y análisis conjunto.
# dte es la serie trimestral de gallito 2013-2018
# dte <- readRDS(paste0(repo_i,"/s_trimestral13-18.rds"))
# dt  es la serie de 80-2014 compatibilizada.

dte[data.table::between(fecha, "2013-07-01", "2018/07/01"), {
  y = c(min(avisos_c_dup, dt[, avisos_union], na.rm = TRUE), max(avisos_c_dup, dt[, avisos_union], na.rm = TRUE))
  plot(x = fecha, y = avisos_c_dup, type = "l", ylim = y)
  par(new = TRUE)
  plot(x = fecha, y = avisos_c_dup_dest, type = "l", ylim = y)
  par(new = TRUE)
  dt[data.table::between(fecha, "2013-07-01", "2018/07/01"), plot(x = fecha, y = avisos_union, type = "l", col = "red", ylim = y, ylab = "")]
  par(new = TRUE)
  dt[data.table::between(fecha, "2013-07-01", "2018/07/01"), plot(x = fecha, y = avisos_union_c, type = "l", col = "red", ylim = y, ylab = "")]
  # plot_ly(data = .SD, ) %>%
  #   add_lines(x = fecha, y = avisos, ymin = y[1], ymax = y[2]) %>% 
  #   add_lines()
}]
dte[data.table::between(fecha, as.Date("2013/07/01"), as.Date("2014/07/01")), {
  par(mfrow = c(3,1))
  plot(x = fecha, y = avisos_c_dup, type = "l")
  dt[data.table::between(fecha, as.Date("2013/07/01"), as.Date("2014/07/01")),
     plot(x = fecha, avisos_union, type = "l")]
  dt[data.table::between(fecha, as.Date("2013/07/01"), as.Date("2014/07/01")),
     plot(x = fecha, avisos_union_c, type = "l")]
}]
# Recordar que esa estacionalidad en avisos_union_c se agrego artificialmente a partir del gallito.
dt[fecha == "2013-07-01", avisos_union]
dt[fecha == "2013-07-01", avisos_union_c]
dte[fecha == "2013-07-01", avisos_c_dup]
dte[fecha == "2013-07-01", avisos_c_dup_dest]

# PONDERADOR para unir las series
(ponderador_sin_c_dest = dte[fecha == "2013-07-01", avisos_c_dup_dest]/dt[fecha == "2013-07-01", avisos_union])
(ponderador_con_c_dest = dte[fecha == "2013-07-01", avisos_c_dup_dest]/dt[fecha == "2013-07-01", avisos_union_c])
(ponderador_sin_c = dte[fecha == "2013-07-01", avisos_c_dup]/dt[fecha == "2013-07-01", avisos_union])
(ponderador_con_c = dte[fecha == "2013-07-01", avisos_c_dup]/dt[fecha == "2013-07-01", avisos_union_c])

# Análisis de las series
par(mfrow = c(1,1))
dte[data.table::between(fecha, "2013-07-01", "2018/07/01"), {
  y = c(min(avisos_c_dup, dt[, avisos], na.rm = TRUE), max(avisos_c_dup, dt[, avisos], na.rm = TRUE))
  plot(x = fecha, y = avisos_c_dup, type = "l", ylim = y)
  par(new = TRUE)
  dt[data.table::between(fecha, "2013-07-01", "2018/07/01"), 
     plot(x = fecha, y= avisos_union*ponderador_sin_c_dest, type = "l", col = "red", ylim = y, ylab = "")]
  par(new = TRUE)
  dt[data.table::between(fecha, "2013-07-01", "2018/07/01"), 
     plot(x = fecha, y= avisos_union_c*ponderador_con_c, type = "l", col = "red", ylim = y, ylab = "")]
  par(new = TRUE)
  dt[data.table::between(fecha, "2013-07-01", "2018/07/01"), 
     plot(x = fecha, y= avisos_union_c*ponderador_con_c, type = "l", col = "red", ylim = y, ylab = "")]
}]
dt[, {
  # y = c(min(avisos, avisos*ponderador), max(avisos, avisos*ponderador))
  # plot(x = fecha, y = avisos, type = "l", ylim = y)
  # par(new = TRUE)
  # plot(x = fecha, y= avisos*ponderador, type = "l", col = "red", ylim = y, ylab = "")
  plot_ly(data = .SD) %>% 
    add_lines(x = fecha, y = avisos_union) %>% 
    add_lines(x = fecha, y = avisos_union*ponderador_sin_c, showlegend = FALSE) %>% 
    add_lines(x = fecha, y = avisos_union_c) %>%
    add_lines(x = fecha, y = avisos_union_c*ponderador_con_c, showlegend = FALSE) %>% 
    add_lines(x = fecha, y = avisos_union*ponderador_sin_c_dest, showlegend = FALSE) %>% 
    add_lines(x = fecha, y = avisos_union_c*ponderador_con_c_dest, showlegend = FALSE)
}]
# Esta reponderación no tiene sentido de realizarse. Debido a que antes de año x (supongamos 2012) el gallito representa el 100% de los avisos laborales
# Pero a partir de dicho año x (2012) su participación decae de forma relevante. Por lo tanto, reponderar la serie hacia atrás es incorrecto.
# Lo que debe reponderarse es la serie hacia adelante de forma que represente ~ el 100% de los avisos.

##### ESTO NO VA! NO SE REPONDERA HACIA ATRÁS PORQUE ES INCORRECTO
# Mantengo la parte de agregar los gallitos

# # UNIÓN de las series
# # 1. Agrego los avisos 2013-2019
dt[, `:=`(ano = data.table::year(fecha),
          q   = data.table::quarter(fecha),
          mes = data.table::month(fecha))]
setnames(dt, old = names(dt), new = gsub(names(dt), pattern = "avisos$", replacement = "avisos_urr_mol"))
setkey(dt, "fecha")
setkey(dte, "fecha")
cols <- names(dte)[grepl(pattern = "avisos_.", x = names(dte))]
dt[dte, (cols) := mget(cols)]

dt <- data.table::rbindlist(list(dt, dte[fecha >= "2019-01-01"]), use.names = TRUE, fill = TRUE)
setnames(dt, old = names(dt), new = gsub(names(dt), pattern = "(avisos_)([cs]_dup.*)", replacement = "\\1ga_\\2"))
setnames(dt, old = names(dt), new = gsub(names(dt), pattern = "avisos", replacement = "av"))

# # 2. repondero la serie hacia atrás
# dt[, plot_ly(.SD) %>% 
#        add_lines(x = fecha, y = avisos_gallito)]
# # dt[fecha < "2013/07/01", avisos := avisos*ponderador_sin_c]
# dt[, plot_ly(.SD) %>% 
#        add_lines(x = fecha, y = avisos)]
# Importante analizar lo que se aquí. La forma de la curva es la misma, pero la escala cambia notoriamente
# Se pasa de 17 mil a 4 mil ó se pasa de 11 mil a 4 mil.

# Otra forma para unir la serie sería solamente reponderar un intervalo de ambas series de forma suave
# Es decir, que en el punto o puntos en el cual coinciden sean iguales y luego reponderar de forma suave y
# decreciente hasta que el ponderador desaparezca.
# Tengo que probar y averiguar cual sería la mejor forma.

# Guardo la serie momentaneamente:
setcolorder(dt, c("fecha", "ano", "mes", "q", "pea", "vacantes", "av_ceres"))
saveRDS(dt, here::here("Datos", "Intermedias", "avisos_80-2019.rds"))
####

# El gap 
# Posibles causas: (Esta discusión vale también para 1998)
# 1. El valor de avisos del año base es muy grande (sobre calcule)
# 2. Ceres no utiliza avisos, sino puestos de trabajo.
# 3. Ceres utiliza otras fuentes de información además del gallito
#     3.1 Si utiliza otras fuentes el gallito queda sub-representado.

# Sigo:
# Si CERES solo toma avisos (y no puestos), las series deberian ser iguales. Por qué no lo serían?
#     1. Avisos año base sobredimensionado, recalcular con un valor más bajo.
#     2. Ceres podría haber usado otras fuentes de información. Siempre? A partir de cuando?




# Análisis 80-2014 para corroborar cuando se separa CERES -----------------

paquetes = c("plotly", "data.table", "ggplot2", "magrittr")
sapply(paquetes, require, character.only = TRUE)
# Cargo la serie de avisos 80-2014
# serie de iecon que muestrea semanas de 2000 al 2009
# serie que recolecté de 2000 (enero a octubre tengo, agosto imputar, Q4 descartarlo por estar incompleto)
# serie que recolecté de 2001 (enero y casi completo marzo, imputar)
# serie que recolecte de 1999 (usar Q3)
# serie que recolecté de 2010 Q1. Completo
# serie que recolecté de 2011 Q1. Falta enero completo, imputar.
# serie que recolecté de 2012 Q1. Falta 3/18/2012	3/24/2012 imputar. Y falta 1ra sem Ene debe estar en Dic
# serie que recolecté de 2013 Q1. Faltan últimas 3 sem de marzo, imputar.

# Agregar trimestres
# Analizar en la serie que recolecte y en la serie de gallito 2013-2018 como es el comportamiento a nivel semanal
# El objetivo es ver a partir de que momento, ceres comenzaría a utilizar otras fuentes de información
# Otra idea sería utilizar la serie de avisos del iecon y llevarla a nivel mensual usando esos avisos.

clean_names <- function(DT) {
  setnames(DT, old = names(DT), new = c("ano", "num", "f_ini", "f_fin", "seccion", "subseccion", "avisos"))
}
dt   <- readRDS(here::here("Datos", "Intermedias", "avisos_80-2019.rds"))
# dt   <- readRDS(here::here("Datos", "Intermedias", "avisos_80-2014_completo.rds"))

archivos = list.files("./Datos/Originales", pattern = "Gallito-[0-9]{4}(-[0-9]{4})?(-[0-9]{2})?(-[0-9]{2})?(.xlsx)", full.names = FALSE)
archivos <- archivos[!archivos %in% c("Gallito-1995-09-12.xlsx", "Gallito-1996-1998.xlsx", "Gallito-1998-01-06.xlsx", 
                                      "Gallito-1998-08.xlsx", "Gallito-2013-2018.xlsx")]
archivos <- paste0("./Datos/Originales/", archivos)
lista = list()
for (file in archivos) {
  lista[[file]] = readxl::read_excel(file, range = readxl::cell_cols("A:G")) %>% 
    data.table::as.data.table(.)
}
for(tabla in lista) {
  clean_names(tabla)
}
DT <- data.table::rbindlist(lista, use.names = TRUE, fill = TRUE, idcol = "tabla")

iecon <- haven::read_dta(here::here("Datos", "Originales", "Gallito-2000-2009.dta")) %>% 
  as.data.table(.)

DT[, setdiff(colnames(DT), c("f_ini", "f_fin", "avisos", "subseccion")) := NULL]
DT[, subseccion := tolower(subseccion)]
DT[, sum(is.na(avisos)), by = subseccion] # Datos faltantes. Necesito imputarlos antes de mensualizar o trimestralizar la serie
DT[is.na(avisos),]
DT[is.na(f_ini), ]

# No puede haber missing en fecha eso es un error. Limpiar
DT <- DT[!is.na(f_ini),]
# Genero mes y ano.
DT[, `:=`(mes = month(f_ini),
          ano = year(f_ini),
          sem = week(f_ini),
          q = quarter(f_ini))]
DT[, .(avisos =sum(avisos)) , by = .(ano, mes, sem)] 
DT[, .(avisos =sum(avisos)) , by = .(ano, mes, sem)][is.na(avisos),] # Todos los casos son correctos. Son datos faltantes.

# genero los trimestres y días en el mes
DT[, `:=`(q_ini = quarter(f_ini),
          q_fin = quarter(f_fin))]
DT[, diames := lubridate::days_in_month(f_ini)]
DT[, f_ini-f_fin] # Como siempre son 7 días la diferencia es de 6 dias, ok.
DT[abs(f_ini-f_fin) > 6, .N]

DT[month(f_ini) != month(f_fin), .(lubridate::day(f_ini)-diames)] 

# Casos en los cuales no se corresponden los meses entre f_ini y f_fin
# Si es mayor a 3 entonces hay más días en f_fin.
DT[, mes_c := mes]
DT[month(f_ini) != month(f_fin) & abs(lubridate::day(f_ini)-diames) < 3, mes_c := month(f_fin)]

# Series agrupadas
DT[, sum(avisos), by = .(ano, mes_c)]
DT[, sum(avisos), by = .(ano, q_ini)]
DT[, sum(avisos), by = .(year(f_fin), q_fin)]

# Urrestarazu incluyo los avisos en el trimestre en el cual estuvieron más días.
DT[q_ini != q_fin,] # semanas problemáticos al trimestralizar
# Corrección de trimestres
DT[, q_c := q_ini][q_ini != q_fin & mes != mes_c, q_c := q_fin]
# Corrección de año
DT[, ano_c := ano]
DT[q_ini - q_fin > 1 & q_c == 1, ano_c := year(f_fin)]
# corección de semana (1ra semana del año)
DT[, sem_c := sem][q_ini - q_fin > 1 & q_c == 1, sem_c := 1]


# Año 1999-Q3
q3_99 <- data.table::data.table(fecha = as.Date("1999/07/01"), 
                                avisos = DT[ano_c == 1999, .(avisos = sum(avisos)), by = .(ano_c, mes_c)
                                            ][mes_c %in% c(7,8,9), sum(avisos)]
)
# Año 2000 
# enero a octubre tengo, agosto imputar. Q1-Q3
tsdt <- DT[ano_c == 2000, .(avisos = sum(avisos)), by = .(ano_c, mes_c, sem_c)] %$%
  ts(avisos, frequency = 365.25/7, start = c(2000, 1))
imputeTS::plotNA.distribution(tsdt)
# Imputación. Me quedo con las observaciones 1-38 que van de Q1-Q3
impKal <- imputeTS::na_kalman(tsdt, model = "StructTS", smooth = TRUE)
imputeTS::plotNA.imputations(x.withNA = tsdt, x.withImputations = impKal)

q1_00 <- data.table::data.table(fecha = seq.Date(as.Date("2000/01/01"), as.Date("2000/07/01"), "quarter"),
                                avisos = DT[ano_c == 2000, .(avisos = sum(avisos)), by = .(ano_c, mes_c, sem_c)
                                            ][, avisos := as.integer(impKal)
                                              ][, .(avisos = sum(avisos)) ,by = .(ano_c, quarter(as.Date(paste(ano_c, mes_c, 01, sep = "-"))))
                                                ][quarter %in% 1:3, avisos])

# Año 2001 q1 
# Serie de tiempo para imputar NA. Pero solamente los valores de marzo del 2001
DT[ano_c == 2001,]
tsdt <- DT[ano_c == 2001, .(avisos = sum(avisos)) , keyby = .(ano_c, mes_c, sem_c)] %$% 
  ts(avisos, frequency = 365.25/7, start = c(2001,1))
imputeTS::plotNA.distribution(tsdt, main = "Distribución de datos faltantes", ylab = "avisos", xlab = "fecha")
# Imputación
impKal <- imputeTS::na_kalman(tsdt, model = "StructTS", smooth = TRUE)
# plot
imputeTS::plotNA.imputations(x.withNA = tsdt, x.withImputations = impKal, main = "Valores imputados", xlab = "fecha")
# Cantidad de avisos en 2001-Q1
q1_01 <- data.table::data.table(fecha = as.Date("2001/01/01"), avisos = sum(impKal))

# Año 2010 q1 
imputar <- function(DT, int_ano, Q, int_frequency = 365.25/7, vec_start, modelo = "StructTS", string_date) {
  # DT[ano_c == int_ano,]
  tsdt <- DT[ano_c == int_ano & q_c == Q, .(avisos = sum(avisos)) , keyby = .(ano_c, mes_c, sem_c)] %$% 
    ts(avisos, frequency = int_frequency, start = vec_start)
  # imputeTS::plotNA.distribution(tsdt, main = "Distribución de datos faltantes", ylab = "avisos", xlab = "fecha")
  # Imputación
  impKal <- imputeTS::na_kalman(tsdt, model = modelo, smooth = TRUE)
  # plot
  imputeTS::plotNA.imputations(x.withNA = tsdt, x.withImputations = impKal, main = "Valores imputados", xlab = "fecha")
  # Cantidad de avisos en 2001-Q1
  return(data.table::data.table(fecha = as.Date(string_date), avisos = sum(impKal)))
}
# q1_10 <- data.table::data.table(fecha = as.Date("2010/01/01"), 
#                                 avisos = DT[ano_c == 2010, .(avisos = sum(avisos)), by = .(ano_c, mes_c)
#                                             ][mes_c %in% c(1,2,3), sum(avisos)])
q1_10 <- imputar(DT, int_ano = 2010, Q = 1, vec_start = c(2010, 1), string_date = "2010/01/01")

# Año 2011 q1 
# serie que recolecté de 2011 Q1. Falta enero completo, imputar.
q1_11 <- imputar(DT, int_ano = 2011, Q = 1, vec_start = c(2011, 1), string_date = "2011/01/01")

# Año 2012 q1 
# serie que recolecté de 2012 Q1. Falta 3/18/2012	3/24/2012 imputar. Y falta 1ra sem Ene debe estar en Dic
q1_12 <- imputar(DT, int_ano = 2012, Q = 1, vec_start = c(2012,1), string_date = "2012/01/01")

# Año 2013 q1 
# serie que recolecté de 2013 Q1. Faltan últimas 3 sem de marzo, imputar.
q1_13 <- imputar(DT, int_ano = 2013, Q = 1, vec_start = c(2013, 1), string_date = "2013/01/01")

# Año 2013 q4
q4_13 <- imputar(DT, int_ano = 2013, Q = 4, vec_start = c(2013, 4), string_date = "2013/10/01")

# Año 2014  
q1_14 <- imputar(DT, int_ano = 2014, Q = 1, vec_start = c(2014, 1), string_date = "2014/01/01")
q2_q4_14 <- data.table(fecha = as.Date(c("2014-04-01", "2014-07-01", "2014-10-01")), 
                       avisos = DT[ano_c == 2014 & q_c %in% c(2,3,4), .(avisos = sum(avisos)) , keyby = .(ano_c, q_c)][, avisos])

# Comparo con la serie 80-2014 
iecon_avisos <- iecon[, {
  avisos = .N*93/7
  fecha  = as.Date(paste(aniog, mesg, 01, sep = "-"))
  q      = quarter(fecha)
  # fechaQ = seq.Date(as.Date("2000/01/01"), as.Date("2009/07/01"), "quarter")
  .(avisos = avisos, fecha = fecha, q = q)
}, keyby = .(aniog, mesg)][q == 1, fechaQ := as.Date(paste(aniog, q, 01, sep = "-"))
                           ][q == 2, fechaQ := as.Date(paste(aniog, 04, 01, sep = "-"))
                             ][q == 3, fechaQ := as.Date(paste(aniog, 07, 01, sep = "-"))]

Q <- rbindlist(list(q3_99, q1_00, q1_01, q1_10, q1_11, q1_12, q1_13, q4_13, q1_14, q2_q4_14))
rm(list = ls()[grepl(pattern = "q\\d.*", x = ls())])
# Guardas los muestreos
saveRDS(Q, "./Datos/Finales/muestreos.rds")

# Gráfico todas las series de forma conjunta.
dt[, {
  plot_ly(.SD) %>% 
    # layout(p = ., scene = list(xaxis = list(title = "A", range = c(min(fecha), max(fecha))), 
    #                            yaxis = list(title = "B", range = c(-4,4)), zaxis = list(title = "C"))) %>% 
    add_trace(x = fecha, y = av_urr_mol, showlegend = TRUE, opacity = 1,
              mode = "lines+markers", name = "serie95-98") %>% 
    add_trace(x = fecha, y = av_ga_c_dup,
              mode = "lines+markers", color = I("blue"), name = "gallito") %>%
    add_trace(data = Q, x = ~fecha, y = ~avisos, xend = fecha, yend = ~avisos,
              mode = "markers", type = "scatter", showlegend = TRUE, color = I("orange"),
              name = "muestreos") %>% 
    add_trace(data = iecon_avisos, x = ~fechaQ, y = ~avisos, showlegend = TRUE, color = I("gray"),
              mode = "lines+markers", type = "scatter", name = "iecon") %>%
    add_trace(x = fecha, y = av_ceres, showlegend = TRUE, color = I("skyblue"),
              mode = "lines+markers", type = "scatter", opacity = 0.5, name = "ceres") %>% 
    add_trace(x = fecha, y = av_ceres, showlegend = TRUE, color = I("skyblue"),
              mode = "lines+markers", type = "scatter", opacity = 0.5, name = "ceres_c")
}]
# Los valores naranja son los trimestre que estoy muestreando de gallito y van en linea con serie de ceres.
# O sea, el problema esta arrancando de ahí en adelante.
# Urgente:
# Muestrear q1-2014 para verificar que coincida con la base de datos que me mandaron.
# Verificar los calculos de gallito que realice con dicha base ---> 
#     Calculos son correctos lo único que se filtro fueron avisos repetidos por link, aprox 6000 en 5 años, es decir,
#     un promedio de 100 avisos por mes, nada drámatico.
#
# La caída de 2014 en adelante es demasiado grave, recordar la caída en la representación del gallito.
# Podría tomarse a partir de este año.
# Fuentes externa a utilizar:
# Weyback machine
# Encuesta de radar.

# ¿Coinciden los avisos extraídos de los gallitos papel con los avisos de la base de datos facilitada?
# Pueden existir diferencias, leves o relevantes dependiendo los patrones en que se publican/ban los avisos.
# Si existen diferencias entre el gallito en papel y el gallito original, hay que elegir hacia donde reponderar la serie.
# En la medida que los avisos de la base de datos de El País coincide aproximadamente con los avisos scrapeados, es
# preferible reponderar la serie hacia atrás de forma de poder seguir extendiendola hacia el futuro.
# Aunque pueden realizarse ambas tareas, y guardar las series correspondientes en el objeto dt, de forma tal que todo quede
# en 1 solo objeto (facilidad para cambiar cosas).

# Cargo dichos gallitos y cambio el nivel de la serie
# gallito <- readxl::read_excel("./Datos/Originales/Gallito-2014.xlsx") # Cuándo la uso????? Parece que nunca.
# setDT(gallito)

DT[q == 1, fecha_trim := as.Date(paste(ano_c, 01, 01, sep = "-"))
   ][q == 2, fecha_trim := as.Date(paste(ano_c, 04, 01, sep = "-"))
     ][q == 3, fecha_trim := as.Date(paste(ano_c, 07, 01, sep = "-"))
       ][q == 4, fecha_trim := as.Date(paste(ano_c, 10, 01, sep = "-"))]

# Comparación. Objeto Q tiene los trimestres imputados, DT tiene los trimestres restantes.
# trimestres: 2013q4 a 2014q4, 5q
(comparacion_gallito <- data.table(
  avisos_bd_c_dup =dt[data.table::year(fecha) %in% c(2013, 2014) & fecha >= "2013-10-01", av_ga_c_dup],
  avisos_bd_s_dup =dt[data.table::year(fecha) %in% c(2013, 2014) & fecha >= "2013-10-01", av_ga_s_dup],
  avisos_papel = c(Q[fecha %in% c(as.Date("2013-10-01"), as.Date("2014-01-01")), avisos], 
                   DT[ano_c %in% c(2013, 2014) & fecha_trim >= "2013-10-01", 
                      .(avisos = sum(avisos)), 
                      by = .(q_c, ano_c)
                      ][4:6, avisos]
  ),
  q = c(4, 1:4),
  ano = c(2013, rep(2014, 4))
))
comparacion_gallito[, `:=`(ratio_c_dup = avisos_papel/avisos_bd_c_dup,
                           ratio_s_dup = avisos_papel/avisos_bd_s_dup,
                           diferencia_c_dup = as.integer(avisos_papel - avisos_bd_c_dup),
                           diferencia_s_dup = as.integer(avisos_papel - avisos_bd_s_dup),
                           pct_c_dup = avisos_bd_c_dup/avisos_papel,
                           pct_s_dup = avisos_bd_s_dup/avisos_papel)]
comparacion_gallito[, sapply(.(ratio_c_dup, pct_c_dup, diferencia_c_dup), mean)]
comparacion_gallito[, sapply(.(ratio_s_dup, pct_s_dup, diferencia_s_dup), mean)]
# O sea, los avisos reales son entre un 67-82% de los avisos observados a nivel trimestral.
# Lo que sería entre un 22 a un 50% de avisos repetidos por trimestre.
# El promedio es de 33% de repetidos, 
# Avisos reales un 75% de los observados y una diferencia prom de 2600 trimestral.
# En el análisis de texto, BORRANDO los avisos con link repetidos, y tomando un umbral de 0.8 se obtiene para gallito,
# aproximadamente un 10% de avisos repetidos promedio anual.
# Hay que volver a calcular la similaridad de avisos SIN borrar los link y ver que % da.
# Obviamente va a ser igual o mayor, lo cual puede llevar a que la diferencia disminuya.


# Relevantes diferencias por trimestres
# Cuanta diferencia promedio por semana aprox? 
comparacion_gallito[, .(avisos_bd_c_dup, avisos_papel)]/12
comparacion_gallito[, .(avisos_bd_s_dup, avisos_papel)]/12
(comparacion_gallito[, .(avisos_bd_c_dup, avisos_papel)]/12)[, .(avisos_papel - avisos_bd_c_dup)]
(comparacion_gallito[, .(avisos_bd_s_dup, avisos_papel)]/12)[, .(avisos_papel - avisos_bd_s_dup)]
# En torno a 200 avisos de diferencia entre el papel y la base de datos.
# La base de avisos del país es la variable no observable, avisos laborales.
# Los avisos son la variable observada con ruido y error de medición. 
# Esta diferencia nos puede estar dando una pauta de que las empresas pagan por publicar sus avisos y los mismos permanecen
# durantes varias semanas, lo cual genera que el número de avisos laborales observados este sobreestimado de los avisos
# que realmente fueron solicitados en el período.
# Dado que tengo una pauta de este error de medición, puedo reponderar la serie hacia atrás intentando que muestre los 
# avisos que realmente solicitaron las empresas mensualmente.

# Esta parte la hago solamente para los avisos con duplicados
pond_q1 = comparacion_gallito[q == 1, ratio_c_dup]
pond_q2 = comparacion_gallito[q == 2, ratio_c_dup]
pond_q3 = comparacion_gallito[q == 3, ratio_c_dup]
pond_q4 = comparacion_gallito[ano == 2014 & q == 4, ratio_c_dup]

# Repondero y vuelvo a gráficar
dt[q == 1, av_ga_pond_mes := av_ga_c_dup * pond_q1
   ][q == 2, av_ga_pond_mes := av_ga_c_dup * pond_q2
     ][q == 3, av_ga_pond_mes := av_ga_c_dup * pond_q3
       ][q == 4, av_ga_pond_mes := av_ga_c_dup * pond_q4]

dt[, {
  plot_ly(.SD) %>% 
    add_trace(x = fecha, y = av_ga_c_dup, 
              mode = "lines+markers", color = I("blue"), name = "gallito") %>% 
    add_trace(x = fecha, y = av_ga_s_dup, 
              mode = "lines+markers", color = I("blue"), name = "gallito") %>% 
    add_trace(x = fecha, y = av_ga_pond_mes, 
              mode = "lines+markers", color = I("gray"), name = "gallito_pond") #%>% 
  # add_trace(data = Q, x = ~fecha, y = ~avisos, xend = fecha, yend = ~avisos,
  #           mode = "markers", type = "scatter", showlegend = TRUE, color = I("orange"),
  #           name = "muestreos") %>% 
  # add_trace(x = fecha, y = avisos_ceres, showlegend = TRUE, color = I("skyblue"),
  #           mode = "lines+markers", type = "scatter", opacity = 0.5, name = "ceres")
}]
# Lo importante de este gráfico es observar que reponderando de esta forma se pierde cierta curvatura de la serie
# original entre 2013-2018, es una forma incorrecta de reponderar ya que, altera la serie, meses que caen
# ahora suben, en especial el q3 que en vez de caer aumenta. 
#Notar como si originalmente el mes_t > mes_t+1, con la repond mes_t < _mes_t+1

# Reponderar entonces con un solo valor, moviendo todo el nivel de la serie, usar el promedio 2013q12014 avisos papel
ponderador_c_dup = comparacion_gallito[, mean(avisos_papel)/mean(avisos_bd_c_dup)]
ponderador_s_dup = comparacion_gallito[, mean(avisos_papel)/mean(avisos_bd_s_dup)]

dt[, av_ga_pond_c_dup := av_ga_c_dup*ponderador_c_dup]
dt[, av_ga_pond_s_dup := av_ga_s_dup*ponderador_s_dup]

dt[, {
  plot_ly(.SD) %>% 
    # add_trace(x = fecha, y = avisos, showlegend = TRUE, opacity = 1,
    #           mode = "lines+markers", name = "serie") %>% 
    add_trace(x = fecha, y = av_ga_pond_c_dup, 
              mode = "lines+markers", color = I("blue"), name = "ga_pond_c_dup") %>% 
    add_trace(x = fecha, y = av_ga_pond_s_dup, 
              mode = "lines+markers", color = I("blue"), name = "ga_pond_c_dup") %>% 
    # add_trace(x = fecha, y = av_ga_pond_mes, 
    #           mode = "lines+markers", color = I("pink"), name = "ga_pond_m") %>% 
    add_trace(x = fecha, y = av_ga_c_dup, 
              mode = "lines+markers", color = I("gray"), name = "ga_c_dup") %>% 
    add_trace(data = Q, x = ~fecha, y = ~avisos, xend = fecha, yend = ~avisos,
              mode = "markers", type = "scatter", showlegend = TRUE, color = I("orange"),
              name = "muestreos") %>%
    add_trace(x = fecha, y = av_ga_c_dup_dest, showlegend = TRUE, color = I("skyblue"),
              mode = "lines+markers", type = "scatter", opacity = 0.5, name = "ga_c_dup_dest") %>% 
    add_trace(x = fecha, y = av_ga_s_dup_dest, showlegend = TRUE, color = I("skyblue"),
              mode = "lines+markers", type = "scatter", opacity = 0.5, name = "ga_s_dup_dest") %>% 
    add_trace(x = fecha, y = av_ceres, showlegend = TRUE, color = I("skyblue"),
              mode = "lines+markers", type = "scatter", opacity = 0.5, name = "ceres")
}]

# La variable de avisos_gall_pond_mes no tiene utilidad alguna por lo cual se borra.
dt[, av_ga_pond_mes := NULL]

# Los avisos de scraping son limpiados de los repetidos por link.
# Por lo tanto, se va a seguir trabajando a futuro con avisos sin duplicados por link. Eso quiere decir que las series a utilizar,
# deben ser las que están limpiadas de los repetidos por link.
# Y más importante que los ajuste que se realicen deben ser considerando las diferencias con dichas series.
# Tomar en cuenta que existe una diferencia entre el gallito en papel y el gallito de la base de datos, a la vez que los avisos
# de scraping son muy similar al original de la base de datos, por lo tanto se mantiene la diferencia con el gallito en papel.
# La misma crece levemente al usar los avisos filtrados de duplicaciones por link.
# Los avisos de buscojobs y computrabajo, también vienen limpiados de repeticiones por link.

# Es decir, las series de 2013 hacia atrás tienen que ser reponderadas para tomar en cuenta esta diferencia.

# Guardo mientras
saveRDS(object = dt, file = "./Datos/Finales/series_todas_final.rds", compress = FALSE)

# bj y ct -----------------------------------------------------------------
# # ESTO LO HAGO EN EL SCRIPT DE generacion_serie_vacantes.R
# # cargo datos de buscojobs y computrabajo.
# bj_ct <- readRDS("./Datos/Finales/serie_trim_bj_ct.rds")
# dt <- readRDS("./Datos/Finales/series_todas_final.rds")
# setkey(bj_ct, "fecha")
# setkey(dt, "fecha")
# 
# # Columnas para agregar
# str(bj_ct)
# (cols <- names(bj_ct)[grepl(names(bj_ct), pattern = "av_")])
# 
# # Join y renombrar
# dt[bj_ct, (cols) := mget(cols)]
# 
# # Genero una serie que una las 3 restantes. (gallito, bj y ct  SIN duplicados por link y CON filtro
# # de avisos repetidos entre páginas.
# # Gallito tiene ~ 4% de repetidos en cada página.
# # dt[, av_ga_ct_bj_s_dup := av_ct_bj_s_dup_c_fil + av_ga_s_dup - 0.04*av_ga_s_dup*2]
# # ESTO LO HAGO EN EL SCRIPT DE generacion_serie_vacantes.R
# # Porque lo hago con la serie reponderada.
# 
# 
# # La caída de 2019 es por computrabajo. Eso es relevante, porque entre 2015-2019 tiene un crecimiento muy relevante, que
# # empuja toda la serie, a la vez que la caída tan drámatica a partir de 2019 genera una caída muy abrupta.
# # Por seguridad, podría cortarse en 2018, hasta no tener seguridad de que paso con dicha página web y si esos avisos, no 
# # terminaron volcandose a otra página.
# 
# saveRDS(object = dt, file = "./Datos/Finales/series_todas_final.rds", compress = FALSE)

