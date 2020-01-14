# Inicio ------------------------------------------------------------------


libs=c("readxl", "ggplot2", "ggfortify", "magrittr", "plotrix", "imputeTS", "lubridate", "data.table")
lib_nuevas <- libs[!(libs %in% installed.packages()[,"Package"])] 
if(length(lib_nuevas)) install.packages(lib_nuevas)
load_libs <- function(libraries = libs) for (lib in libraries)
    require(lib, character.only=T)
load_libs(libs)
theme_set(theme_bw())

# En este script se generen series de frecuencia mensual y trimestral para los años 95-98 del gallito, y,
# se calcula la cantidad de avisos en agosto de 1998, año base del índice ceres.
# Para ello, primero se unen 3 archivos que contienen datos de 95, 96-97 y 98.
# Luego se imputen los datos faltantes (gallitos no encontrados en biblioteca) mediante filtro de kalman.
# Se toma la fecha como fecha_inicio, es decir, la fecha inicial del intervalo de tiempo que cubre un gallito.
# Ello genera que hay semanas que pertenecen a dos meses o trimestres diferentes, es decir inicia el 
# 28 de agosto y termina el 3 de septiembre, por ejemplo.
# Se opto por imputar los datos al mes/trimestre en el cual la semana tuviese más días, es decir > 3.
# Urrestarazu en su construcción de la serie de 1981-1995 realiza lo mismo.

# Datos de entrada y carpeta de salida. La tabla de este script es intermedia.
rep_entrada <- here::here("Datos", "Originales")
rep_salida  <- here::here("Datos", "Intermedias")

# Pego 3 tablas: 95-96a98-98 sin incluir agosto 98 (la base de ceres)
dt1 <- readxl::read_excel(paste0(rep_entrada,"/Gallito-1995-09-12.xlsx"), range = readxl::cell_cols("A:Q")) %>% as.data.table()
dt2 <- readxl::read_excel(paste0(rep_entrada,"/Gallito-1996-1998.xlsx"), range = readxl::cell_cols("A:Q"), sheet = "avisos_puestos") %>%
    as.data.table()
dt3 <- readxl::read_excel(paste0(rep_entrada,"/Gallito-1998-01-06.xlsx"), range = readxl::cell_cols("A:Q")) %>% as.data.table()
lapply(list(dt1,dt2,dt3), colnames)
invisible(lapply(list(dt1,dt2,dt3), str))

# Voy a trabajar solamente con la serie de avisos y las fechas de inicio y fin.
dt <- rbindlist(list(dt1, dt2, dt3), use.names = TRUE)
dt[, setdiff(colnames(dt), c("f_ini", "f_fin", "avisos", "subseccion")) := NULL]
dt[, subseccion := tolower(subseccion)]
table(dt$subseccion) # Ok
dt[, sum(is.na(avisos)), by = subseccion] # tengo varios datos faltantes. Necesito imputarlos antes de mensualizar o trimestralizar la serie
dt[is.na(avisos),]
# Genero mes y ano.
dt[, `:=`(mes = month(f_ini),
          ano = year(f_ini),
          sem = week(f_ini),
          q = quarter(f_ini))]
dt[, .(avisos =sum(avisos)) , by = .(ano, mes, sem)] 
dt[, .(avisos =sum(avisos)) , by = .(ano, mes, sem)][is.na(avisos),] # Todos los casos son correctos. Son datos faltantes.

# Serie de tiempo para imputar NA
tsdt <- dt[, .(avisos =sum(avisos)) , by = .(ano, mes, sem)] %$% ts(avisos, frequency = 365.25/7, start = c(1995,35))
plotNA.distribution(tsdt, main = "Distribución de datos faltantes", ylab = "avisos", xlab = "fecha")

# Imputación
impKal <- na.kalman(tsdt)

# plot
plotNA.imputations(x.withNA = tsdt, x.withImputations = impKal, main = "Valores imputados", xlab = "fecha")

# Lo vuelvo a convertir a data.frame y data.table agrupado a nivel semanal.
impKal %>% as.data.frame()
dt_s <- dt[, .(avisos =sum(avisos)) , by = .(ano, mes, sem)] 
dt_s[, avisos := floor(impKal %>% as.vector)][,  `:=`(f_ini = unique(dt[, f_ini]),
                                 f_fin = unique(dt[, f_fin]))][]

# genero los trimestres y días en el mes
dt_s[, `:=`(q_ini = quarter(f_ini),
            q_fin = quarter(f_fin))]
dt_s[, diames := days_in_month(f_ini)]
dt_s[, f_ini-f_fin] # Como siempre son 7 días la diferencia es de 6 dias, ok.
dt_s[month(f_ini)!=month(f_fin), .(day(f_ini)-diames)] 
        # Casos en los cuales no se corresponden los meses entre f_ini y f_fin
        # Si es mayor a 3 entonces hay más días en f_fin.
dt_s[, mes_c := mes]
dt_s[month(f_ini) != month(f_fin) & abs(day(f_ini)-diames)<3, mes_c := month(f_fin)]

# Series agrupadas
dt_s[, sum(avisos), by = .(ano, mes_c)]
dt_s[, sum(avisos), by = .(ano, q_ini)]
dt_s[, sum(avisos), by = .(year(f_fin), q_fin)]

# Urrestarazu incluyo los avisos en el trimestre en el cual estuvieron más días.
dt_s[q_ini != q_fin,] # semanas problemáticos al trimestralizar
# Corrección de trimestres
dt_s[, q_c := q_ini][q_ini != q_fin & mes != mes_c, q_c := q_fin]
# Corrección de año
dt_s[, ano_c := ano]
dt_s[q_ini - q_fin>1 & q_c == 1, ano_c := year(f_fin)]

# Genero la serie mensual y trimestral.
mes <- dt_s[, .(avisos = sum(avisos)), by = .(ano_c, q_c, mes_c)][, fecha := as.Date(paste(ano_c,mes_c,1, sep = "-"))]
tri <- dt_s[, .(avisos = sum(avisos)), by = .(ano_c, q_c)]
saveRDS(object = mes, file = paste0(rep_salida, "/s_mensual95-98.rds"))
saveRDS(object = tri, file = paste0(rep_salida, "/s_trimestral95-98.rds"))

# Por último trabajo con agosto del 98.
d <- readxl::read_excel(paste0(rep_entrada,"/Gallito-1998-08.xlsx"), sheet = "avisos_puestos") %>% as.data.table()
d[, setdiff(colnames(d), c("f_ini", "f_fin", "subseccion", "avisos")) := NULL]
d[month(f_ini) != month(f_fin),] 
# Notar donde difieren los meses (julio y sept) practicamente todas las observaciones no caen en agosto.
# 1 dia de avisos asignados a julio se podrían distribuir en agosto (1ro agosto)
# 2 dias de avisos asignados a septiembre se podrían distribuir en agosto (30 y 31)
d[month(f_ini) == 7, sum(avisos)/6*2] # avisos que podrían sumarse a agosto desde julio
d[month(f_fin) == 9, sum(avisos)/6*1] # avisos que podrían sumarse a agosto desde septiembre
# Osea aproximado unos 530 avisos.
d2 <- d[month(f_ini) == month(f_fin),]
d2[, sum(avisos)] # avisos totales en agosto 4300
                  # avisos totales + corrección 4830
agosto98 <- data.table(avisos   = 4300,
                       avisos_2 = 4830,
                       fecha = as.Date("1998-08-01"),
                       ano = 1998,
                       mes = 8,
                       q = 3)
saveRDS(object = agosto98, file = paste0(rep_salida, "/agosto98.rds"))


# Extra -------------------------------------------------------------------


# Extra. Creación subserie 1995 hasta 2001 (Esto lo armé bastante después)
paquetes <- c("ggplot2", "plotly", "magrittr", "data.table")
sapply(paquetes, require, character.only = TRUE)
clean_names <- function(DT) {
    setnames(DT, old = names(DT), new = c("ano", "num", "f_ini", "f_fin", "seccion", "subseccion", "avisos"))
    # setnames(DT, old = names(DT), new = c("f_ini", "f_fin", "seccion", "subseccion", "avisos"))
}
archivos = list.files("./Datos/Originales", pattern = "Gallito-.*(.xlsx)", full.names = FALSE)
archivos <- archivos[!archivos %in% "Gallito-2013-2018.xlsx"]
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

DT[, setdiff(colnames(DT), c("f_ini", "f_fin", "avisos", "subseccion")) := NULL]
DT[, subseccion := tolower(subseccion)]
DT[, sum(is.na(avisos)), by = subseccion] # Datos faltantes. Necesito imputarlos antes de mensualizar o trimestralizar la serie
DT[is.na(avisos),]
DT[is.na(f_ini), ]

# No puede haber missing en fecha eso es un error. Limpiar
DT <- DT[!is.na(f_ini),]
# Genero mes y ano.
DT[, `:=`(ano = year(f_ini),
          mes = month(f_ini),
          sem = week(f_ini),
          q = quarter(f_ini))]
DT[, .(avisos =sum(avisos)) , by = .(ano, mes, sem)] 
DT[, .(avisos =sum(avisos)) , by = .(ano, mes, sem)][is.na(avisos),] # Todos los casos son correctos. Son datos faltantes.

# genero los trimestres y días en el mes
DT[, `:=`(q_ini = quarter(f_ini),
          q_fin = quarter(f_fin))]
DT[, diames := lubridate::days_in_month(f_ini)]
DT[, f_ini-f_fin] # Como siempre son 7 días la diferencia es de 6 dias, ok.
DT[abs(f_ini-f_fin) >= 6, .N]

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

imputar <- function(DT, int_ano, Q, int_frequency = 365.25/7, vec_start, modelo = "StructTS", string_date) {
    # DT[ano_c == int_ano,]
    tsdt <- DT[ano_c == int_ano & q_c == Q, .(avisos = sum(avisos)) , keyby = .(ano_c, mes_c, sem_c)] %$% 
        ts(avisos, frequency = int_frequency, start = vec_start)
    # imputeTS::plotNA.distribution(tsdt, main = "Distribución de datos faltantes", ylab = "avisos", xlab = "fecha")
    # Imputación
    impKal <- imputeTS::na_kalman(tsdt, model = modelo, smooth = TRUE, type = "BSM")
    # plot
    imputeTS::plotNA.imputations(x.withNA = tsdt, x.withImputations = impKal, main = "Valores imputados", xlab = "fecha")
    # Cantidad de avisos en 2001-Q1
    return(data.table::data.table(fecha = as.Date(string_date), avisos = sum(impKal)))
}

dt <- data.table(fecha = seq.Date(from = as.Date("1995-08-27", format = "%Y-%m-%d"), to = as.Date("2014-12-28", format = "%Y-%m-%d"), "week"),
                 key = "fecha")

setkey(DT, "f_ini")
cols <- names(DT)#[names(DT) != "f_ini"]
temp <- DT[, .(avisos = sum(avisos),
       fecha = as.Date(unique(f_ini)),
       f_fin = as.Date(unique(f_fin))), 
       keyby = .(ano, q_c, mes_c, sem_c)]
setkey(temp, "fecha")
cols = names(temp)[names(temp) != "fecha"]
dt[temp, (cols) := mget(cols), on = "fecha"]
rm(temp)

# Genero mes y ano.
dt[, `:=`(ano = year(fecha),
          mes = month(fecha),
          sem = week(fecha),
          q = quarter(fecha))]
dt[, .(avisos =sum(avisos)) , by = .(ano, mes, sem)] 
dt[, .(avisos =sum(avisos)) , by = .(ano, mes, sem)][is.na(avisos),] # Todos los casos son correctos. Son datos faltantes.

# genero los trimestres y días en el mes
dt[, `:=`(q_ini = quarter(fecha),
          q_fin = quarter(f_fin))]
dt[, diames := lubridate::days_in_month(fecha)]
dt[, fecha-f_fin] # Como siempre son 7 días la diferencia es de 6 dias, ok.
dt[abs(fecha-f_fin) >= 6, .N]

dt[month(fecha) != month(f_fin), .(lubridate::day(fecha)-diames)] 

# Casos en los cuales no se corresponden los meses entre fecha y f_fin
# Si es mayor a 3 entonces hay más días en f_fin.
dt[, mes_c := mes]
dt[month(fecha) != month(f_fin) & abs(lubridate::day(fecha)-diames) < 3, mes_c := month(f_fin)]

# Urrestarazu incluyo los avisos en el trimestre en el cual estuvieron más días.
dt[q_ini != q_fin,] # semanas problemáticos al trimestralizar
# Corrección de trimestres
dt[, q_c := q_ini][q_ini != q_fin & mes != mes_c, q_c := q_fin]
# Corrección de año
dt[, ano_c := ano]
dt[q_ini - q_fin > 1 & q_c == 1, ano_c := year(f_fin)]
# corección de semana (1ra semana del año)
dt[, sem_c := sem][q_ini - q_fin > 1 & q_c == 1, sem_c := 1]

tsdt <- dt[fecha <= "2001-03-25", .(avisos = sum(avisos)) , keyby = .(ano_c, mes_c, sem_c, sem)] %$% 
    ts(avisos, frequency = 365.25/7, start = c(1995, 35))
# Imputación
# impKal <- imputeTS::na_kalman(tsdt, model = "StructTS", smooth = TRUE, type = "BSM")
# impKal_false <- imputeTS::na_kalman(tsdt, model = "StructTS", smooth = FALSE, type = "BSM")
# impKal_level <- imputeTS::na_kalman(tsdt, model = "StructTS", smooth = FALSE, type = "trend")
# impKal_level <- imputeTS::na_kalman(tsdt, model = "StructTS", smooth = FALSE, type = "level")
# impKal2 <- imputeTS::na_kalman(tsdt, model = "StructTS", smooth = FALSE, type = "BSM")
impArima <- imputeTS::na_kalman(tsdt, model = "auto.arima", lambda = "auto")
# plot
imputeTS::plotNA.imputations(x.withNA = tsdt, x.withImputations = impKal, main = "Valores imputados", xlab = "fecha")
imputeTS::plotNA.imputations(x.withNA = tsdt, x.withImputations = impKal_false, main = "Valores imputados", xlab = "fecha")
imputeTS::plotNA.imputations(x.withNA = tsdt, x.withImputations = impKal_level, main = "Valores imputados", xlab = "fecha")
imputeTS::plotNA.imputations(x.withNA = tsdt, x.withImputations = impKal2, main = "Valores imputados", xlab = "fecha")
imputeTS::plotNA.imputations(x.withNA = tsdt, x.withImputations = impArima, main = "Valores imputados", xlab = "fecha")
imputeTS::na_seadec(tsdt) %>% imputeTS::plotNA.imputations(x.withNA = tsdt, x.withImputations = .)
# No tiene la caída de turismo en el 99 ni de fin de año
imputeTS::na_seasplit(tsdt, algorithm = "interpolation", option = "linear") %>% imputeTS::plotNA.imputations(x.withNA = tsdt, x.withImputations = .)
# Tiene caída de fin de año , y turismo?
imputeTS::na_seasplit(tsdt, algorithm = "interpolation", option = "stine", method = "parabola") %>% imputeTS::plotNA.imputations(x.withNA = tsdt, x.withImputations = .)
imputeTS::na_seasplit(tsdt, algorithm = "interpolation", option = "stine", method = "stine") %>% imputeTS::plotNA.imputations(x.withNA = tsdt, x.withImputations = .)
imputeTS::na_seasplit(tsdt, algorithm = "interpolation", option = "stine", method = "scaledstineman") %>% imputeTS::plotNA.imputations(x.withNA = tsdt, x.withImputations = .)
# Tiene caída de fin de año y una caída que se puede atribuir a turismo. Me quedo con este.
# Me quedo con stine, porque en los datos previos turismo siempre es menor a fin de año.
# imputeTS::na_seasplit(tsdt, algorithm = "interpolation", option = "spline", "periodic") %>% imputeTS::plotNA.imputations(x.withNA = tsdt, x.withImputations = .)
imputeTS::na_seasplit(tsdt, algorithm = "ma", k = 8) %>% imputeTS::plotNA.imputations(x.withNA = tsdt, x.withImputations = .)

# Guardar objeto con avisos estimados, el último NA lo agarro de la estimación del modelo arima.
avisos_imputados <- imputeTS::na_seasplit(tsdt, algorithm = "interpolation", option = "stine", method = "stine")
avisos_imputados[is.na(avisos_imputados)] <- impArima[NROW(impArima)]

dt[fecha <= "2001-03-25", avisos := as.integer(avisos_imputados)]

dt[, {plot_ly(.SD) %>% 
       add_trace(x = fecha, y = avisos, mode = "lines+markers", type = "scatter", name = "UM")
   }]
dt[, .(avisos = sum(avisos),
       fecha = as.Date(paste(min(ano_c), min(mes_c), 01, sep = "-"))), keyby = .(ano_c, q_c)
   ][, {plot_ly(.SD) %>% 
           add_trace(x = fecha, y = avisos, mode = "lines+markers", type = "scatter", name = "UM")
       }]


# Desestacionalizo --------------------------------------------------------

# Paréntesis: cargo tasas útiles para el xreg.
librerias <- c("data.table", "magrittr")
sapply(librerias, require, character.only = TRUE)

directorio <- here::here("Datos", "Originales")
des80 <- paste(directorio, "Desempleo1980-2005.xls", sep = "/")
des80_original <- readxl::read_excel(des80, skip = 12, col_names = c("fecha1", "borrar1", "fecha2", "ta", "borrar2", "borrar3", 
                                                                     "te", "borrar4", "borrar5", "td", "borrar6", "borrar7"))
des80 <- as.data.table(des80_original)

# View(des80)
des80 <- des80[complete.cases(des80),]
des80[, grep("^borrar", colnames(des80)) := NULL]
des80[, fecha2 := NULL]
des80[, fecha := gsub(fecha1, pattern = "(.*)\\s?/\\s?(\\d*)$", replacement = "\\1-\\2") %>% 
          gsub(., pattern = "-0(\\d)+", replacement = "-200\\1") %>% 
          gsub(., pattern = "-([98])(\\d)+", replacement = "-19\\1\\2") %>% 
          paste(., "01", sep = "-") %>% 
          as.Date(., format = "%B-%Y-%d")
      ][, fecha1 := NULL]
des80 <- des80[fecha > "1985-01-01",]
names_character <- c("ta", "te", "td")
for(col in names_character) {
    set(des80, j = col, value = as.numeric(des80[[col]]))
}
des80

# Desestacionalizo la serie
dt_m <- dt[fecha <= "2001-03-25" & fecha >= "1995-09-03", .(avisos = sum(avisos),
                                                            fecha  = as.Date(paste(ano_c, mes_c, 01, sep = "-")),
                                                            q_c = unique(q_c)),
           keyby = .(ano_c, mes_c)]
temp <- ts(dt_m$avisos, start = c(1995, 9), frequency = 12)
# Agrego xreg
td <- des80[between(fecha, "1995-09-01", "2004-03-01"), .(te, td)] %>% 
    ts(., start = c(1995, 9), frequency = 12)
temp <- seasonal::seas(temp)#, xreg = td)
plot(temp)
temp[["data"]][, "seasonal"] %>% min(.)
temp[["data"]][, "seasonal"] %>% max(.)

temp$data %>% plot
temp$data[, "seasonal"]
monthplot(temp)
temp[["data"]][, "seasonaladj"] %>% plot
temp[["data"]][, "adjustfac"]
summary(temp)

autoplot(temp)

dt_m[, avisos_dest := as.integer(temp[["data"]][, "final"])]
# setnames(dt_m, old = names(dt_m), new = c("ano", "mes", "avisos", "fecha", "q", "avisos_dest"))

dt_m[, {plot_ly(.SD) %>% 
        add_trace(x = fecha, y = avisos, mode = "lines+markers", type = "scatter", name = "UM") %>% 
        add_trace(x = fecha, y = avisos_dest, mode = "lines+markers", type = "scatter", name = "UM")
    }]

# Junto trimestralmente y guardo.
dt_trim <- dt_m[fecha > "1995-09-01", .(avisos = sum(avisos),
                    avisos_dest = sum(avisos_dest),
                    fecha = min(fecha),
                    mes_c = min(mes_c)),
                keyby = .(ano_c, q_c)]
setcolorder(dt_m, c("fecha", "ano_c", "q_c", "mes_c"))
setcolorder(dt_trim, c("fecha", "ano_c", "q_c", "mes_c"))
saveRDS(object = dt_m, file = "./Datos/Finales/serie_mensual_95-01.rds")
saveRDS(object = dt_trim, file = "./Datos/Finales/serie_trim_95-01.rds")
