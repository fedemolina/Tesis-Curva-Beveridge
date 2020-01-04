# Compatibilización buscojobs
paquetes <- c("data.table", "magrittr")
sapply(paquetes, require, character.only = TRUE)

source(here::here("scripts", "read_files_rds.R"))
source(here::here("scripts", "clean_column.R"))
source(here::here("scripts", "clean_column_puesto.R"))
source(here::here("scripts", "clean_column_empresa.R"))
source(here::here("scripts", "clean_column_dpto.R"))
source(here::here("scripts", "delete.R"))
# Carga de datos sin detalle
# archivos <- list.files(here::here("buscojobs", "csv"), pattern = ".csv", full.names = TRUE)
# lista = list()
# for(i in archivos) {
#   lista[[i]] <- data.table::fread(file = i, encoding = "Latin-1")
# }
# lapply(lista, names)
# dt <- data.table::rbindlist(l = lista, use.names = TRUE, fill = TRUE, idcol = "file")
# names(dt)

#
dt <- read_files(here::here("buscojobs"), .pattern = ".rds")
names(dt)
#

# Nombres que se usan en el último código (el 1ero)
c('empresa', 'lugar', 'area', 'n_puestos', 'jornada', 'detalles', 'subareas', 'requisitos', 'ID', "fecha_scraping", # Esta parte es la general
  "barrio_ciudad", "fecha_pub", "dia_pub", "dpto", "requisitos", "subareas", "detalles", "ID", "puesto") # Parte individual

# barrio_ciudad  == barrioCiudad
# fecha_pub      == fechaPub ==fpub
# fecha_scraping == fechaScraping
# n_puestos      == puestos
# dia_pub
# dpto
# requisitos
# subareas
# detalles
# ID
# puesto

# Dejo todos las variables que se están usando
dt[!is.na(barrioCiudad), barrio_ciudad := barrioCiudad][, barrioCiudad := NULL]

# Cambiar formato de fecha_pub sino genera NA
dt[, fecha_pub := as.character(fecha_pub)]
dt[, fpub := as.character(fpub)]

dt[!is.na(fechaPub), fecha_pub := fechaPub
   ][, fechaPub :=NULL
     ][!is.na(fpub), fecha_pub := fpub][, fpub := NULL]
dt[!is.na(fechaScraping), fecha_scraping := fechaScraping][, fechaScraping := NULL]
dt[!is.na(puestos), n_puestos := puestos][, puestos := NULL]
dt[, year(fecha_scraping) %>% table] # Solo 2019
dt[, (fecha_pub) %>% table]

# Limpieza ----------------------------------------------------------------

# Filtrar avisos repetidos por link
# setkey(dt, "ID")
setkey(dt, "fecha_scraping")
dt <- dt[!duplicated(dt, by = "ID", fromLast = FALSE), ]
dt[, (fecha_pub) %>% table]
setkey(dt, "ID")

# nombres de columnas
setnames(dt, old = names(dt), new = tolower(names(dt)))

# cols <- c("puesto", "empresa", "dpto")
cols <- c("dpto")
dt[, (cols) := lapply(.SD, tolower), .SDcols = cols]
dt[, dpto := gsub(dpto, pattern = "\\s", replacement = "-") %>% tolower()]

change_dpto <- function(old, new) {
   dt[dpto == old, dpto := new]
}
change_dpto(new = "cerrolargo", "cerro-largo")
change_dpto(new = "paysandú", "paysandu")
change_dpto(new = "sanjosé", "san-jose")
change_dpto(new = "tacuarembó", "tacuarembo")
change_dpto(new = "treintaytres", "treinta-y-tres")
dt[, table(dpto)]

# Limpiar nombres

# detalles
# requisitos
# subareas
clean_column(dt, "detalles")
clean_column(dt, "requisitos")
clean_column(dt, "subareas")

# puesto y empresa
clean_column_puesto(dt, "puesto")
clean_column_empresa(dt, "empresa")

# Dejar solo el nombre del archivo
dt[, file := stringi::stri_split_fixed(str = file, pattern = "/csv/", n = 2, tokens_only = TRUE) %>% 
      transpose() %>% `[`(2)]

dt[, fecha_pub := gsub("Publicado hace ","", fecha_pub) %>% 
   gsub("días", "dia", .) %>% 
   gsub("minutos", "minuto", .) %>% 
   gsub("día", "dia", .) %>% 
   gsub("horas", "hora", .) %>% 
   gsub("semanas", "semana", .) %>% 
   gsub("meses", "mes", .) %>% 
   gsub("un", "1", .)]
dt[nchar(fecha_pub) < 10, c("cantidad", "unidad") := data.table::tstrsplit(fecha_pub, " ", type.convert = TRUE)]
# Al trabajar con los .rds este problema desaparece.

# Convierto a segundos
minutos = 60
horas = 60*minutos
dias = 24*horas
semanas = 7*dias
meses = 30*dias

dt[nchar(fecha_pub) < 10, sum(is.na(unidad))]
dt[, unidad_seg := meses
   ][unidad == "minuto", unidad_seg := minutos
     ][unidad == "hora",    unidad_seg := horas
       ][unidad == "dia",     unidad_seg := dias
         ][unidad == "semana",  unidad_seg := semanas]

# Voy a tener que dividir el DT y luego combinarlos con el formato correcto.
dt[nchar(fecha_pub) < 10, as.POSIXct(fecha_scraping) - (as.integer(cantidad) * unidad_seg)]
dt[nchar(fecha_pub) < 10, table(year(as.POSIXct(fecha_scraping) - (as.integer(cantidad) * unidad_seg)))]
temp1 <- dt[nchar(fecha_pub) < 10, ][, fecha_pub := as.POSIXct(fecha_scraping) - as.integer(cantidad) * unidad_seg]
temp2 <- dt[nchar(fecha_pub) >= 10, ][, fecha_pub := as.POSIXct(fecha_pub)] 
# Revisar temp2 porque tiene años 2017, además tiene errores en 2018, ej scraping 2019/07 y fecha pub 2018/04
# Se ve que hubo un error en el código.
dt[nchar(fecha_pub) >= 10, table(year(fecha_pub))]
# Lo mejor que se puede hacer para arreglarlo es imputar el mes de la fecha de scraping
temp2[, temp_scraping := as.POSIXct(fecha_scraping)]
# La diferencia siguiente es en horas, fijemos un umbral
t_mes = 31*24
temp2[abs(fecha_pub - temp_scraping) > t_mes, table(.(year(fecha_scraping), month(fecha_scraping)))]
# Ok, la mayoría de problemas se dan entre 05/19 y 07/19. Tuve un problema con el código en esa fecha
# debido a un mal calculo con minutos/minuto que se ve altero los calculos de la fecha de publicación
# como no se guardo la variable original "Hace xxx publicado" no se puede recuperar la fecha.
# Verificar aumentando el umbral
temp2[abs(temp_scraping - fecha_pub) > t_mes*2, table(.(year(fecha_scraping), month(fecha_scraping)))]
# Perfecto, como era de esperar el problema es entre 05 y 07 de 2019. En el resto de los casos, ningún aviso
# tiene diferencia mayor a 60 días.
# Opciones que se pueden aplicar:
# 1. Imputar el mes de la fecha de scraping
# 2. Calcular la diferencia promedio entre fecha_pub y fecha_scraping en avisos con fecha correcta e imputar 
temp1[, summary(as.integer(as.POSIXct(fecha_scraping) - fecha_pub))]
# Mediana de 384 y media de 391 horas ~ 12 días.
temp1[, boxplot(as.integer(as.POSIXct(fecha_scraping) - fecha_pub))]

# Para ser más exigente, tomo un umbral de 3 meses.
temp2[abs(temp_scraping - fecha_pub) > t_mes*3, ]
temp2[abs(temp_scraping - fecha_pub) > t_mes*3, summary(as.integer(temp_scraping - fecha_pub))]
# Mediana de 330 días y media de 369 ~ 10-12 días.

# Entonces la fecha de publicación va a ser la fecha de scraping - 12 días.
correc <- 12*24*60*60 # 12 dias * 24 hrs * 60 min * 60 sec
temp2[abs(temp_scraping - fecha_pub) > t_mes*3, fecha_pub := temp_scraping - correc]
# check
temp2[abs(temp_scraping - fecha_pub) > t_mes*3, ]
# bien.
temp2[, temp_scraping := NULL]

dt <- data.table::rbindlist(list(temp1, temp2), use.names = TRUE)

# check
dt[, table(year(fecha_pub))]
# Revisar 2018, no son muy pocos avisos? Esta bien, se arranco a scrapear buscojobs en enero 2019.
# Previamente solo gallito y computrabajo

dt[, data.table::tstrsplit(fecha_pub, " ", type.convert = TRUE)]
dt[, c("fecha_pub", "h_pub") := data.table::tstrsplit(fecha_pub, " ", type.convert = TRUE)]
dt[, fecha_pub := as.Date(fecha_pub, format = "%Y-%m-%d")]
dt[, `:=`(cantidad   = NULL,
          unidad     = NULL,
          unidad_seg = NULL,
          h_pub = NULL)]

dt[, `:=`(ano_scr = year(fecha_scraping),
          mes_scr = month(fecha_scraping),
          dia_scr = mday(fecha_scraping))]
dt[, `:=`(ano = year(fecha_pub),
          mes = month(fecha_pub),
          dia = mday(fecha_pub))]

# unwanted_array = list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
#                           'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
#                           'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
#                           'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
#                           'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

# Sys.setlocale("LC_TIME", "Spanish") # Esto lo quiero setear en inicio .Rprofile
# dt[, dia_pub := factor(weekdays(fecha_pub) %>% 
#                          chartr(paste(names(unwanted_array), collapse = ''),
#                                 paste(unwanted_array, collapse=''),
#                                 .), 
#                        levels = c("lunes","martes","miercoles","jueves","viernes",
#                                   "sabado","domingo"), ordered = TRUE)]

# Se ve que cambie el código de scraping en algún momento por lo tanto hay columnas con nombres diferentes pero que contienen la
# misma información
head(dt) 
tail(dt)
# REVISAR LAS FECHAS DE PÚBLICACIÓN PUEDEN TENER ERROR.

# Subareas no queda correcto, no se limpio "\r", por qué?
dt[, subareas := gsub(subareas, pattern = "\r", replacement = " ", fixed = TRUE)]
dt[, dia_pub := NULL]
dt[, fecha_scraping := as.POSIXct(fecha_scraping)] # Por qué no lo modifique antes? abombao

# Me quedo solo con los avisos de diciembre en adelante, aunque incluso podría filtrarse diciembre porque no esta completo.
# Hasta dic-2019

dt <- dt[fecha_pub >= "2018-12-01" & fecha_pub < "2020-01-01", ]

# Guardo
saveRDS(dt, "./Datos/Intermedias/buscojobs-compatibilizado.rds")
