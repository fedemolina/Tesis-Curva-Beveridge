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
