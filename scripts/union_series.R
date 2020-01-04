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
dtm <- readRDS(paste0(repo_i,"/s_mensual95-98.rds"))
dtc <- readxl::read_excel(paste0(repo_o,"/ICDL-1998-2014.xls"), range = readxl::cell_cols("A:B"), sheet = "serie") %>% 
    as.data.table(.)
ceres <- readRDS(paste0(repo_f, "/ceres_corregido.rds"))
dti <- haven::read_dta(paste0(repo_o,"/Gallito-2000-2009.dta")) %>% as.data.table()
dte <- readRDS(paste0(repo_i,"/s_trimestral13-18.rds"))
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

estimaciones80 <- matrix(c(est1[1:4], est2[1:4], est4[1:4]), ncol = 4, nrow = 3, byrow = TRUE) %>% 
    apply(., MARGIN = 2, mean, na.rm = TRUE)
estimaciones81 <- matrix(c(est1[5:6], est2[5:6], est4[5:6]), ncol = 2, nrow = 3, byrow = TRUE) %>% 
    apply(., MARGIN = 2, mean, na.rm = TRUE)

series[, vacantes := c(estimaciones80, estimaciones81, dtu[, vacantes])]

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

dt[between(fecha, as.Date("1996/01/01"), as.Date("1998/04/01")), 
   avisos := dtm[, .(avisos = sum(avisos)), by = .(ano_c, q_c)
                ][, .(avisos, fecha = seq.Date(as.Date("1995/07/01"), as.Date("1998/07/01"), "quarter"))
                ][between(fecha, as.Date("1996/01/01"), as.Date("1998/04/01")), avisos]
  ]
dt[, plot_ly(.SD) %>% 
     add_trace(x = fecha, y = avisos, mode = "lines+markers")]

# Unión serie 80-98 y Ceres (98-2014) -------------------------------------

# Coinciden en 1998-q2
dt[fecha == as.Date("1998-04-01"), avisos]
ceres_q <- ceres[between(fecha, as.Date("1998-04-01"), as.Date("2014/09/01")), 
                .(avisos = sum(avisos_a_c)), by = .(year(fecha), quarter(fecha))]
ceres_q[, fecha := seq.Date(as.Date("1998/04/01"), as.Date("2014/07/01"), "quarter")]
ceres_q

# Genero un ponderador para unirlas. Recalculo la serie de ceres, ya que, conozco mejor la serie de urrestarazu.
ponderador = dt[fecha == as.Date("1998-04-01"), avisos]/ceres_q[fecha == as.Date("1998-04-01"), avisos]
dt[between(fecha, as.Date("1998/04/01"), as.Date("2014/07/01")), avisos := ceres_q[, avisos*ponderador]]
dt[!is.na(avisos), plot_ly(data = .SD) %>% 
     add_lines(x = fecha, y = avisos)]
# La caída comienza en el año 94, no es un problema de unión de series y los datos desde 89 al 95 son confiables dado que fueron
# suministrados directamente por el país.
# Recordar que la serie de Ceres puede estar sobrecalculada si el valor usado en el año base es muy grande.
# Serie buena practica probar y calcular con más de 1 valor. Tener 3 series de ceres y realizar el análisis para las 3
# Agregar eso aquí:

#  P E N D I E N T E

# Guardo la serie de avisos desde 80-14 para analizar diferencias con los avisos del gallito.
saveRDS(dt, here::here("Datos", "Intermedias", "avisos_80-2014.rds"))


# Unión serie 80-2014 y datos Gallito (2013-2018) -------------------------

# No tomar en cuenta en 2013 el mes 5. Por tanto, arrancar en 2013-q3
# 2018-q3 esta completo. ---> 2013-q3 al 2018-q3
# Voy a reponderar para que quede acorde a los avisos de 2013-2018

# Ceres: vt = a_t/a0*100
# Quiero ver cuánto debería ser a0 para que coincidan las series en 2013-q3? Pero ceres es mensual
temp <- readRDS(here::here("Datos", "Intermedias", "s_mensual13-18.rds"))
temp[between(fecha, "2013-06-01", "2014-10-01"), avisos]
# "2013-05-01" hasta 2014-10-01
v_t = ceres[between(fecha, "2013-06-01", "2014-10-01"), vacantes] 
a_t = temp[between(fecha, "2013-06-01", "2014-10-01"), avisos]
a_0 = a_t/v_t*100
a_0     
# Recodar que CERES dice estar desestionalizado, cuando en realidad es más bien una tendencia.
# El resultado de la supuesta base varía.
# Tiene sentido, porque la serie de vacantes de CERES es desestacionalizada (o es la tendencia)
# Por lo que no se observa el verdadero a_t

# Ahora pasa a la unión y análisis conjunto.
# dte es la serie trimestral de gallito 2013-2018
# dt  es la serie de 80-2014 compatibilizada.
dte[, fecha := seq.Date(as.Date("2013/04/01"), as.Date("2018/07/01"), "quarter")]
dte[between(fecha, "2013-07-01", "2018/07/01"), {
  y = c(min(avisos, dt[, avisos], na.rm = TRUE), max(avisos, dt[, avisos], na.rm = TRUE))
  plot(x = fecha, y = avisos, type = "l", ylim = y)
  par(new = TRUE)
  dt[between(fecha, "2013-07-01", "2018/07/01"), plot(x = fecha, y= avisos, type = "l", col = "red", ylim = y)]
  # plot_ly(data = .SD, ) %>%
  #   add_lines(x = fecha, y = avisos, ymin = y[1], ymax = y[2]) %>% 
  #   add_lines()
}]
dte[between(fecha, as.Date("2013/07/01"), as.Date("2014/07/01")), {
  par(mfrow = c(2,1), mai)
  plot(x = fecha, y = avisos, type = "l")
  dt[between(fecha, as.Date("2013/07/01"), as.Date("2014/07/01")), plot(x = fecha, avisos, type = "l")]
}]
dt[fecha == "2013-07-01", avisos]
dte[fecha == "2013-07-01", avisos]

# PONDERADOR para unir las series
ponderador = dte[fecha == "2013-07-01", avisos]/dt[fecha == "2013-07-01", avisos]

# Análisis de las series
dte[between(fecha, "2013-07-01", "2018/07/01"), {
  y = c(min(avisos, dt[, avisos], na.rm = TRUE), max(avisos, dt[, avisos], na.rm = TRUE))
  plot(x = fecha, y = avisos, type = "l", ylim = y)
  par(new = TRUE)
  dt[between(fecha, "2013-07-01", "2018/07/01"), 
     plot(x = fecha, y= avisos*ponderador, type = "l", col = "red", ylim = y, ylab = "")]
}]
dt[!is.na(avisos), {
  # y = c(min(avisos, avisos*ponderador), max(avisos, avisos*ponderador))
  # plot(x = fecha, y = avisos, type = "l", ylim = y)
  # par(new = TRUE)
  # plot(x = fecha, y= avisos*ponderador, type = "l", col = "red", ylim = y, ylab = "")
  plot_ly(data = .SD) %>% 
    add_lines(x = fecha, y = avisos) %>% 
    add_lines(x = fecha, y = avisos*ponderador, showlegend = FALSE)
}]

# UNIÓN de las series
# 1. Agrego los avisos 2013-2018
dt[between(fecha, "2013-07-01", "2018/07/01"), 
   avisos := dte[between(fecha, "2013-07-01", "2018/07/01"), avisos]]
# 2. repondero la serie hacia atrás
dt[, plot_ly(.SD) %>% 
     add_lines(x = fecha, y = avisos)]
dt[fecha < "2013/07/01", avisos := avisos*ponderador]
dt[, plot_ly(.SD) %>% 
     add_lines(x = fecha, y = avisos)]
# Importante analizar lo que se aquí. La forma de la curva es la misma, pero la escala cambia notoriamente
# Se pasa de 17 mil a 4 mil ó se pasa de 11 mil a 4 mil.

# Otra forma para unir la serie sería solamente reponderar un intervalo de ambas series de forma suave
# Es decir, que en el punto o puntos en el cual coinciden sean iguales y luego reponderar de forma suave y
# decreciente hasta que el ponderador desaparezca.
# Tengo que probar y averiguar cual sería la mejor forma.

# Guardo la serie momentaneamente:
saveRDS(dt, here::here("Datos", "Finales", "avisos.rds"))


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
dt   <- readRDS(here::here("Datos", "Intermedias", "avisos_80-2014.rds"))

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

ceres <- readRDS("./Datos/Finales/ceres_corregido.rds")
dte <- readRDS("./Datos/Intermedias/s_trimestral13-18.rds")

# Voy a trabajar solamente con la serie de avisos y las fechas de inicio y fin.
# DT <- rbindlist(list(dt99, dt00, dt01), use.names = TRUE)
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
temp <- iecon[, {
  avisos = .N*93/7
  fecha  = as.Date(paste(aniog, mesg, 01, sep = "-"))
  q      = quarter(fecha)
  # fechaQ = seq.Date(as.Date("2000/01/01"), as.Date("2009/07/01"), "quarter")
  .(avisos = avisos, fecha = fecha, q = q)
}, keyby = .(aniog, mesg)][q == 1, fechaQ := as.Date(paste(aniog, q, 01, sep = "-"))
                           ][q == 2, fechaQ := as.Date(paste(aniog, 04, 01, sep = "-"))
                             ][q == 3, fechaQ := as.Date(paste(aniog, 07, 01, sep = "-"))]

ceres_q2 <- ceres[between(fecha, as.Date("1998-04-01"), as.Date("2014/09/01")), 
                 .(avisos = sum(avisos_a)), by = .(year(fecha), quarter(fecha))]
ceres_q2[quarter == 1, fecha := as.Date(paste(year, "01", "01", sep = "-"))
         ][quarter == 2, fecha := as.Date(paste(year, "04", "01", sep = "-"))
           ][quarter == 3, fecha := as.Date(paste(year, "07", "01", sep = "-"))
             ][quarter == 4, fecha := as.Date(paste(year, "10", "01", sep = "-"))]

gallito <- dte[q == 1, fecha := as.Date(paste(ano, q, 01, sep = "-"))
               ][q == 2, fecha := as.Date(paste(ano, 04, 01, sep = "-"))
                 ][q == 3, fecha := as.Date(paste(ano, 07, 01, sep = "-"))
                   ][q == 4, fecha := as.Date(paste(ano, 10, 01, sep = "-"))]

Q <- rbindlist(list(q3_99, q1_00, q1_01, q1_10, q1_11, q1_12, q1_13, q4_13, q1_14, q2_q4_14))

# Gráfico todas las series de forma conjunta.
dt[, {
  plot_ly(.SD) %>% 
    # layout(p = ., scene = list(xaxis = list(title = "A", range = c(min(fecha), max(fecha))), 
    #                            yaxis = list(title = "B", range = c(-4,4)), zaxis = list(title = "C"))) %>% 
    add_trace(x = fecha, y = avisos, showlegend = TRUE, opacity = 1,
              mode = "lines+markers", name = "serie") %>% 
    add_trace(data = gallito[fecha != "2013-04-01"], x = ~ fecha, y = ~ avisos, 
              mode = "lines+markers", color = I("blue"), name = "gallito") %>% 
    add_trace(data = Q, x = ~fecha, y = ~avisos, xend = fecha, yend = ~avisos,
              mode = "markers", type = "scatter", showlegend = TRUE, color = I("orange"),
              name = "muestreos") %>% 
    # add_trace(data = temp, x = ~fechaQ, y = ~avisos, showlegend = TRUE, color = I("gray"),
    #           mode = "lines+markers", type = "scatter", name = "iecon") %>% 
    add_trace(data = ceres_q2, x = ~fecha, y = ~ avisos, showlegend = TRUE, color = I("skyblue"),
              mode = "lines+markers", type = "scatter", opacity = 0.5, name = "ceres")
}]
# Los valores naranja son los trimestre que estoy muestreando de gallito y van en linea con serie de ceres.
# O sea, el problema esta arrancando de ahí en adelante.
# Urgente:
# Muestrear q1-2014 para verificar que coincida con la base de datos que me mandaron.
# Verificar los calculos de gallito que realice con dicha base ---> 
#     Calculos son correctos lo único que se filtro fueron avisos repetidos, aprox 6000 en 5 años, es decir,
#     un promedio de 100 avisos por mes, nada drámatico.
#
# La caída de 2014 en adelante es demasiado grave, recordar la caída en la representación del gallito.
# Podría tomarse a partir de este año.
# Fuentes externa a utilizar:
# Weyback machine
# Encuesta de radar.

# Existen diferencias entre el gallito en papel y el gallito original.
# Cargo dichos gallitos y cambio el nivel de la serie

df3 <- readxl::read_excel("./Datos/Originales/Gallito-2014.xlsx")
setDT(df3)

DT[q == 1, fecha_trim := as.Date(paste(ano_c, 01, 01, sep = "-"))
   ][q == 2, fecha_trim := as.Date(paste(ano_c, 04, 01, sep = "-"))
     ][q == 3, fecha_trim := as.Date(paste(ano_c, 07, 01, sep = "-"))
       ][q == 4, fecha_trim := as.Date(paste(ano_c, 10, 01, sep = "-"))]

comparacion <- data.table(gallito[ano %in% c(2013, 2014) & fecha >= "2013-10-01"], 
                          avisos_papel = c(q4_13[, avisos], q1_14[, avisos], 
                                           DT[ano_c %in% c(2013, 2014) & fecha_trim >= "2013-10-01", 
                                              .(avisos = sum(avisos)), 
                                              by = .(q_c, ano_c)
                                              ][4:6, avisos]))
comparacion[, `:=`(ratio = avisos_papel/avisos,
                   diferencia = as.integer(avisos_papel - avisos))]
pond_q1 = comparacion[q == 1, ratio]
pond_q2 = comparacion[q == 2, ratio]
pond_q3 = comparacion[q == 3, ratio]
pond_q4 = comparacion[q == 4, mean(ratio)]

# Repondero y vuelvo a gráficar
gallito[q == 1, avisos_pond := avisos * pond_q1
          ][q == 2, avisos_pond := avisos * pond_q2
            ][q == 3, avisos_pond := avisos * pond_q3
              ][q == 4, avisos_pond := avisos * pond_q4]

dt[, {
  plot_ly(.SD) %>% 
    # layout(p = ., scene = list(xaxis = list(title = "A", range = c(min(fecha), max(fecha))), 
    #                            yaxis = list(title = "B", range = c(-4,4)), zaxis = list(title = "C"))) %>% 
    add_trace(x = fecha, y = avisos, showlegend = TRUE, opacity = 1,
              mode = "lines+markers", name = "serie") %>% 
    add_trace(data = gallito[fecha != "2013-04-01"], x = ~ fecha, y = ~ avisos_pond, 
              mode = "lines+markers", color = I("blue"), name = "gallito") %>% 
    add_trace(data = gallito[fecha != "2013-04-01"], x = ~ fecha, y = ~ avisos, 
              mode = "lines+markers", color = I("gray"), name = "gallito_orig") %>% 
    add_trace(data = Q, x = ~fecha, y = ~avisos, xend = fecha, yend = ~avisos,
              mode = "markers", type = "scatter", showlegend = TRUE, color = I("orange"),
              name = "muestreos") %>% 
    # add_trace(data = temp, x = ~fechaQ, y = ~avisos, showlegend = TRUE, color = I("gray"),
    #           mode = "lines+markers", type = "scatter", name = "iecon") %>% 
    add_trace(data = ceres_q2, x = ~fecha, y = ~ avisos, showlegend = TRUE, color = I("skyblue"),
              mode = "lines+markers", type = "scatter", opacity = 0.5, name = "ceres")
}]
# Lo importante de este gráfico es observar que reponderando de esta forma se pierde la curvatura de la serie
# original entre 2013-2018, es una forma incorrecta de reponderar ya que, altera gravemente la serie. Notar como
# si originalmente el mes_t > mes_t+1, con la repond mes_t < _mes_t+1

# Reponderar entonces con un solo valor, moviendo todo el nivel de la serie, usar el promedio 2014 avisos papel
ponderador = comparacion[ano == 2014, mean(avisos_papel)]/comparacion[ano == 2014, mean(avisos)]
gallito[, avisos_pond := avisos*ponderador]

dt[, {
  plot_ly(.SD) %>% 
    # layout(p = ., scene = list(xaxis = list(title = "A", range = c(min(fecha), max(fecha))), 
    #                            yaxis = list(title = "B", range = c(-4,4)), zaxis = list(title = "C"))) %>% 
    add_trace(x = fecha, y = avisos, showlegend = TRUE, opacity = 1,
              mode = "lines+markers", name = "serie") %>% 
    add_trace(data = gallito[fecha != "2013-04-01"], x = ~ fecha, y = ~ avisos_pond, 
              mode = "lines+markers", color = I("blue"), name = "gallito") %>% 
    add_trace(data = gallito[fecha != "2013-04-01"], x = ~ fecha, y = ~ avisos, 
              mode = "lines+markers", color = I("gray"), name = "gallito_orig") %>% 
    add_trace(data = Q, x = ~fecha, y = ~avisos, xend = fecha, yend = ~avisos,
              mode = "markers", type = "scatter", showlegend = TRUE, color = I("orange"),
              name = "muestreos") %>% 
    # add_trace(data = temp, x = ~fechaQ, y = ~avisos, showlegend = TRUE, color = I("gray"),
    #           mode = "lines+markers", type = "scatter", name = "iecon") %>% 
    add_trace(data = ceres_q2, x = ~fecha, y = ~ avisos, showlegend = TRUE, color = I("skyblue"),
              mode = "lines+markers", type = "scatter", opacity = 0.5, name = "ceres")
}]

# Agregar avisos de BJ y CT de waybackmachine
bj <- readxl::read_excel("./Datos/Originales/waybackMachine/buscoJobs.xlsx", range = readxl::cell_cols("A:B"))
setDT(bj)
bj[, `:=`(mes = data.table::month(fecha),
          dia = data.table::mday(fecha),
          ano = data.table::year(fecha))]
setkey(bj, fecha)
bj <- bj[!duplicated(bj, by = c("ano", "mes"), fromLast = FALSE),] 
# me quedo con el último día que aparezca del mes? Probando parece tener más sentido usar fromLast = FALSE, no es
# muy sensato que venga con una tendencia a la baja en pleno auge de la economía y ganando terreno como portal
# Suponer que la cantidad de avisos observados en cualquier punto del mes es representativo
# Generar serie mensual bajo dicho supuesto
bj[, fecha_c := as.Date(paste(ano, mes, 01, sep ="-"))]
bj[, plot_ly(.SD) %>% 
     add_trace(x = fecha_c, y = avisos, mode = "lines+markers")]
# Imputar los valores faltantes
bj_ts <- ts(bj[, avisos], start = c(2008, 4),
            end = c(2019, 1), frequency = 12)
imputeTS::plotNA.distribution(bj_ts)
bj_imp_ts <- imputeTS::na_kalman(bj_ts, smooth = TRUE, model = "StructTS")
# asigno avisos imputados
bj[, avisos_imp_bj := as.vector(bj_imp_ts)]
bj[, plot_ly(.SD) %>% 
     add_trace(x = fecha_c, y = avisos_imp_bj, mode = "lines+markers")]

# COMPUTRABAJO
ct <- readxl::read_excel("./Datos/Originales/waybackMachine/computrabajo.xlsx", range = readxl::cell_cols("A:B"))
setDT(ct)
ct[, `:=`(mes = data.table::month(fecha),
          dia = data.table::mday(fecha),
          ano = data.table::year(fecha))]
setkey(ct, fecha)
ct <- ct[!duplicated(ct, by = c("ano", "mes"), fromLast = TRUE),] 
ct[, avisos := avisos*0.5] # Repondero a la mitad
# me quedo con el último día que aparezca del mes? Probando parece tener más sentido usar fromLast = FALSE, no es
# muy sensato que venga con una tendencia a la baja en pleno auge de la economía y ganando terreno como portal
# Suponer que la cantidad de avisos observados en cualquier punto del mes es representativo
# Generar serie mensual bajo dicho supuesto
ct[, fecha_c := as.Date(paste(ano, mes, 01, sep ="-"))]
ct[, plot_ly(.SD) %>% 
     add_trace(x = fecha_c, y = avisos, mode = "lines+markers")]
# Imputar los valores faltantes
ct_ts <- ts(ct[, avisos], start = c(2008, 2),
            end = c(2018, 12), frequency = 12)
imputeTS::plotNA.distribution(ct_ts)
ct_imp_ts <- imputeTS::na_kalman(ct_ts, smooth = TRUE, model = "StructTS")
# asigno avisos imputados
ct[, avisos_imp_ct := as.vector(ct_imp_ts)]
ct[, plot_ly(.SD) %>% 
     add_trace(x = fecha_c, y = avisos_imp_ct, mode = "lines+markers")]

# Junto BJ y CT
setkey(bj, fecha_c)
setkey(ct, fecha_c)
bj[ct]
ct[bj, avisos_imp_bj := avisos_imp_bj]
ct <- ct[3:NROW(ct), ]
ct[, avisos_ct_bj := avisos_imp_bj + avisos_imp_ct]
ct[, plot_ly(.SD) %>% 
     add_trace(x = fecha_c, y = avisos_ct_bj, mode = "lines+markers")]

ct[, q := data.table::quarter(fecha_c)]
ct_q <- ct[, .(avisos_ct_bj = sum(avisos_ct_bj),
               avisos_bj = sum(avisos_imp_bj),
               avisos_ct = sum(avisos_imp_ct)), by = .(ano, q)]
ct_q[, q := fifelse(q == 1, 1, 
               fifelse(q == 2, 4,
                       fifelse(q == 3, 7, 10)))]
ct_q[, fecha := as.Date(paste(ano, q, 01, sep = "-"))]
setkey(ct_q, fecha)
# Como quedaron?
ct_q[, plot_ly(.SD) %>% 
       add_trace(x = fecha, y = avisos_ct_bj, mode = "lines+markers", name = "bj & ct") %>% 
       add_trace(x = fecha, y = avisos_bj, mode = "lines+markers", name = "bj") %>% 
       add_trace(x = fecha, y = avisos_ct, mode = "lines+markers", name = "ct")]

# Se los sumo al gallito (trimestral) NOTAR QUE NO SE FILTRO NADA, SE SUMO TODO!
setkey(gallito, fecha)
cols <- c("avisos_bj", "avisos_ct", "avisos_ct_bj")
gallito[ct_q, (cols) := mget(cols)]
gallito[, avisos_sum := avisos_pond + avisos_ct_bj]
# Análisis de gallito, ct y bj
gallito[, plot_ly(.SD) %>% 
            add_trace(x = fecha, y = avisos_ct_bj, mode = "lines+markers", name = "bj & ct") %>% 
            add_trace(x = fecha, y = avisos_bj, mode = "lines+markers", name = "bj") %>% 
            add_trace(x = fecha, y = avisos_ct, mode = "lines+markers", name = "ct") %>%
            add_trace(x = fecha, y = avisos_pond, mode = "lines+markers", name = "gall_pond") %>%   
            add_trace(x = fecha, y = avisos_sum, mode = "lines+markers", name = "todos")
        ]

# Gráfico
dt[, {
  plot_ly(.SD) %>% 
    # layout(p = ., scene = list(xaxis = list(title = "A", range = c(min(fecha), max(fecha))), 
    #                            yaxis = list(title = "B", range = c(-4,4)), zaxis = list(title = "C"))) %>% 
    add_trace(x = fecha, y = avisos, showlegend = TRUE, opacity = 1,
              mode = "lines+markers", name = "serie") %>% 
    add_trace(data = gallito[fecha != "2013-04-01"], x = ~ fecha, y = ~ avisos_sum, 
              mode = "lines+markers", color = I("red"), name = "gallito_sum") %>% 
    add_trace(data = gallito[fecha != "2013-04-01"], x = ~ fecha, y = ~ avisos_pond, 
              mode = "lines+markers", color = I("blue"), name = "gallito_pon") %>% 
    add_trace(data = gallito[fecha != "2013-04-01"], x = ~ fecha, y = ~ avisos, 
              mode = "lines+markers", color = I("gray"), name = "gallito_orig") %>% 
    add_trace(data = Q, x = ~fecha, y = ~avisos, xend = fecha, yend = ~avisos,
              mode = "markers", type = "scatter", showlegend = TRUE, color = I("orange"),
              name = "muestreos") %>% 
    # add_trace(data = temp, x = ~fechaQ, y = ~avisos, showlegend = TRUE, color = I("gray"),
    #           mode = "lines+markers", type = "scatter", name = "iecon") %>% 
    add_trace(data = ceres_q2, x = ~fecha, y = ~ avisos, showlegend = TRUE, color = I("skyblue"),
              mode = "lines+markers", type = "scatter", opacity = 0.5, name = "ceres")
}]
# Llamativo como mantiene la misma forma!



# Agrego al gráfico anterior los avisos de gallito, computrabajo y buscojobs de scraping
bj_ct <- readRDS("./Datos/Intermedias/unionAvisos.rds")
bj_ct[, q := data.table::quarter(fecha_pub)]
bj_ct[ano == 2019, .N, keyby = .(ano, q)]
gallito[ano == 2018,]


# Comparación gallito original v/s  gallito ------------------------------------------------

paquetes <- c("data.table", "magrittr")
sapply(paquetes, require, character.only = TRUE)
df <- readxl::read_excel("./Datos/Originales/Gallito-2013-2018.xlsx") %>% 
        data.table::as.data.table(.)
df2 <- readxl::read_excel("./Datos/Originales/Gallito-2013-12.xlsx") %>% 
        data.table::as.data.table(.)
df3 <- readxl::read_excel("./Datos/Originales/Gallito-2014.xlsx") %>% 
        data.table::as.data.table(.)
df2[, setdiff(colnames(df2), c("f_ini", "f_fin", "avisos")) := NULL]
octubre = df2[(month(f_ini) == 10 | month(f_ini) == 9) & month(f_fin) != 11, sum(avisos)]
diciembre = df2[month(f_ini) == 12, sum(avisos)]
# Supongamos que Noviembre es un promedio ponderado de diciembre y noviembre
noviembre = 0.4*diciembre + 0.6*octubre
# Entonces el q4 del 2013 de gallitos recabados sería:
q4_13 <- sum(octubre, noviembre, diciembre)

# Y cuánto es la suma de q4 según la base de datos de gallito dada por el país?
setnames(df, old = colnames(df), new = tolower(colnames(df)))
q4_13_bd <- df[month(fecha) %in% c(10,11,12) & year(fecha) == 2013, .N]
diciembre_bd <- df[month(fecha) == 12 & year(fecha) == 2013, .N]
octubre_bd <- df[month(fecha) == 10 & year(fecha) == 2013, .N]

c(q4_13, q4_13_bd)
# diferencia de:
q4_13 - q4_13_bd

# Diferencias en diciembre y octubre?
diciembre - diciembre_bd
octubre - octubre_bd

# Comparación mes a mes
DT[q == 1, fecha_trim := as.Date(paste(ano_c, 01, 01, sep = "-"))
  ][q == 2, fecha_trim := as.Date(paste(ano_c, 04, 01, sep = "-"))
    ][q == 3, fecha_trim := as.Date(paste(ano_c, 07, 01, sep = "-"))
      ][q == 4, fecha_trim := as.Date(paste(ano_c, 10, 01, sep = "-"))]
gallito[ano %in% c(2013, 2014) & fecha >= "2013-10-01"]
DT[ano_c %in% c(2013, 2014) & fecha_trim >= "2013-10-01", .(avisos = sum(avisos)), by = .(q_c, ano_c)
   ][3:5, avisos]
q1_14[, avisos]
q4_13

comparacion <- data.table(gallito[ano %in% c(2013, 2014) & fecha >= "2013-10-01"], 
                          avisos_papel = c(q4_13, q1_14[, avisos], DT[ano_c %in% c(2013, 2014) & fecha_trim >= "2013-10-01", .(avisos = sum(avisos)), by = .(q_c, ano_c)
                                                                      ][3:5, avisos]))
comparacion[, `:=`(ratio = round(avisos_papel/avisos, 2),
                   diferencia = as.integer(avisos_papel - avisos))]
pond_q1 = comparacion[q == 1, ratio]
pond_q2 = comparacion[q == 2, ratio]
pond_q3 = comparacion[q == 3, ratio]
pond_q4 = comparacion[q == 4, mean(ratio)]

# Repondero y vuelvo a gráficar
gallito_c <- copy(gallito)
gallito_c[q == 1, avisos := avisos * pond_q1
          ][q == 2, avisos := avisos * pond_q2
            ][q == 3, avisos := avisos * pond_q3
              ][q == 4, avisos := avisos * pond_q4]

# Agrego 2013-q4
q4_13 <- data.table(fecha = as.Date("2013-10-01"), avisos = q4_13)
Q <- rbindlist(list(q3_99, q1_00, q1_01, q1_10, q1_11, q1_12, q1_13, q4_13, q1_14, q2_q4_14))


# Gráfico todas las series de forma conjunta.
dt[, {
  plot_ly(.SD) %>% 
    # layout(p = ., scene = list(xaxis = list(title = "A", range = c(min(fecha), max(fecha))), 
    #                            yaxis = list(title = "B", range = c(-4,4)), zaxis = list(title = "C"))) %>% 
    add_trace(x = fecha, y = avisos, showlegend = TRUE, opacity = 1,
              mode = "lines+markers", name = "serie") %>% 
    add_trace(data = gallito_c[fecha != "2013-04-01"], x = ~ fecha, y = ~ avisos, 
              mode = "lines+markers", color = I("blue"), name = "gallito") %>% 
    add_trace(data = Q, x = ~fecha, y = ~avisos, xend = fecha, yend = ~avisos,
              mode = "markers", type = "scatter", showlegend = TRUE, color = I("orange"),
              name = "muestreos") %>% 
    # add_trace(data = temp, x = ~fechaQ, y = ~avisos, showlegend = TRUE, color = I("gray"),
    #           mode = "lines+markers", type = "scatter", name = "iecon") %>% 
    add_trace(data = ceres_q2, x = ~fecha, y = ~ avisos, showlegend = TRUE, color = I("skyblue"),
              mode = "line", type = "scatter", opacity = 0.5, name = "ceres")
}]

# BJ Y COMPUTRAB ----------------------------------------------------------


# BUSCOJOBS Y COMPUTRABAJO
# Cargo los avisos de buscojobs y computrabajo recolectados de weybackmachine (NO incluyo aún el scraping)
# Los mismos están llenos de datos faltantes
# computrabajo, tiene que reponderado por ~ 0.5 dado que los avisos de los últimos 30 días son la mitad o menos

# BUSCOJOBS
bj <- readxl::read_excel("./Datos/Originales/waybackMachine/buscoJobs.xlsx", range = readxl::cell_cols("A:C"))
setDT(bj)
bj[, `:=`(mes = data.table::month(fecha),
          dia = data.table::mday(fecha),
          ano = data.table::year(fecha))]
setkey(bj, fecha)
bj <- bj[!duplicated(bj, by = c("ano", "mes"), fromLast = TRUE),] # me quedo con el último día que aparezca del mes
# Suponer que la cantidad de avisos observados en cualquier punto del mes es representativo
# Generar serie mensual bajo dicho supuesto
bj[, fecha_c := as.Date(paste(ano, mes, 01, sep ="-"))]
bj[, plot_ly(.SD) %>% 
     add_trace(x = fecha_c, y = avisos, mode = "lines+markers")]
# Imputar los valores faltantes
bj_ts <- ts(bj[, avisos], start = c(2008, 4),
            end = c(2019, 1), frequency = 12)
imputeTS::plotNA.distribution(bj_ts)
bj_imp_ts <- imputeTS::na_kalman(bj_ts, smooth = TRUE, model = "StructTS")
plot.ts(bj_imp_ts)

# COMPUTRABAJO
