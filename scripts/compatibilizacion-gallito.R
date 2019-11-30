# Compatabilización gallito
paquetes <- c("data.table", "magrittr")
sapply(paquetes, require, character.only = TRUE)

delete <- function(DT, del.idxs) { 
  varname = deparse(substitute(DT))
  
  keep.idxs <- setdiff(DT[, .I], del.idxs)
  cols = names(DT);
  DT.subset <- data.table(DT[[1]][keep.idxs])
  setnames(DT.subset, cols[1])
  
  for (col in cols[2:length(cols)]) 
  {
    DT.subset[, (col) := DT[[col]][keep.idxs]]
    DT[, (col) := NULL];  # delete
  }
  
  assign(varname, DT.subset, envir = globalenv())
  return(invisible())
}

# Lista de archivos con scraping NO detallado
archivos <- list.files(here::here("gallito", "csv"), full.names = TRUE)

# Lista de archivos con scraping detallado
archivos_detalle <- list.files(here::here("gallito", "detallado", "csv"), full.names = TRUE)

# Lectura NO detallados
lista = list()
for (i in archivos) {
  lista[[i]] <- data.table::fread(file = i)
}
lapply(lista, names)

dt <- data.table::rbindlist(lista, use.names = TRUE, fill = TRUE, idcol = "file")
dt[is.na(departamento), departamento := dpto]
dt[, dpto := NULL]
rm(lista)

# Lectura detallados
lista = list()
for(i in archivos_detalle) {
  lista[[i]] <- data.table::fread(file = i)
}
lapply(lista, names)

dt_detalle <- data.table::rbindlist(lista, use.names = TRUE, fill = TRUE, idcol = "file")
dt_detalle[is.na(departamento), departamento := dpto]
dt_detalle[, dpto := NULL]


# Limpieza Limpieza de avisos repetidos (se solapan las fechas de los scraping) ----------------------------------------------------------------

# Creación año y mes
dt[, `:=`(ano = year(fecha_scraping),
          mes = month(fecha_scraping),
          dia = mday(fecha_scraping))]
dt_detalle[, `:=`(ano = year(fecha_scraping),
          mes = month(fecha_scraping),
          dia = mday(fecha_scraping))]

# Tuve la mala idea de modificar el formato de links algunos comienzan con:
# https://trabajo.gallito.com.uy/anuncios/...
# anuncios/...
# Por lo tanto se corrige, dejando el formato actual.
dt[!stringr::str_detect(link, pattern = "https://trabajo.gallito.com.uy"), 
   link := paste0("https://trabajo.gallito.com.uy", link)]

dt_detalle[!stringr::str_detect(link, pattern = "https://trabajo.gallito.com.uy"), 
   link := paste0("https://trabajo.gallito.com.uy", link)]

# Filtrar repetidos SIN link
dt[is.na(link), .N]
dt[is.na(link) & duplicated(dt, by = c("puesto", "empresa", "nivel", "area", "departamento")),]

dup = dt[, .I[is.na(link) & duplicated(dt, by = c("puesto", "empresa", "nivel", "area", "departamento"))]]
delete(dt, dup) # Borrados
  # Fin repetidos sin link.

dt_detalle[is.na(link), .N] # Ninguno.

# Repetidos por link
dt[!is.na(link), uniqueN(link)/.N, keyby = .(ano, mes)]
dt[!is.na(link), sum(duplicated(link))/.N, keyby = .(ano, mes)]
dt[!is.na(link), sum(duplicated(link))/uniqueN(link), keyby = .(ano, mes)]

dup = dt[, .I[!is.na(link) & duplicated(dt, by = c("link"))]]
delete(dt, dup) # Borrados

dup = dt_detalle[, .I[duplicated(dt_detalle, by = c("link"))]]
delete(dt_detalle, dup) # Borrados
  # Fin repetidos por link

# Repetidos por c("puesto", "empresa", "nivel", "area", "departamento") una vez limpiados los link y is.na(link)
dt[duplicated(dt, by = c("puesto", "empresa", "nivel", "area", "departamento")), .N]
dt[duplicated(dt, by = c("puesto", "empresa", "nivel", "area", "departamento")), .N, by = .(ano, mes)]
dt[duplicated(dt, by = c("puesto", "empresa", "nivel", "area", "departamento", "detalle")), .N] 
dt[duplicated(dt, by = c("puesto", "empresa", "nivel", "area", "departamento", "link")), .N] 
  # Obs: Llamativo pasar de 3950 a 0 por agregar el link. Si se agrega detalle (no link) pasa 1171
  # Se repiten los llamados pero en meses diferentes, por eso la diferencia...y por eso es más seguro trabajar con link como ID.
  # Por eso, no se borran. Se trabaja bajo que distintos link (id) distinto aviso.

dt_detalle[duplicated(dt_detalle, by = c("puesto", "empresa", "nivel", "area", "departamento", 
                                         "Responsabilidades", "Funciones", "Requisitos", "detalle")), .N] 
dt_detalle[duplicated(dt_detalle, by = c("puesto", "empresa", "nivel", "area", "departamento", "detalle")), .N] 
dt_detalle[duplicated(dt_detalle, by = c("puesto", "empresa", "nivel", "area", "departamento", "link")), .N] 
  # Pasa lo mismo que en el caso anterior.
  # Fin de esta parte.


# Join --------------------------------------------------------------------

# Los avisos detallados contienen TODO la información de los NO detallados. Por lo tanto basta trabajar con ellos.
# Sin embargo existen avisos en dt que NO existen en dt_detalle
# Ello debe ser porque inicialmente el código no incluyó el detalle.
dt[, min(fecha_scraping)]
dt_detalle[, min(fecha_scraping)]
# El formato con detalle recién comienza el 27 de diciembre 2018 mientras el genérico comenzó el 11 de noviembre 2018.
dt[, .N, by = .(ano, mes)]
dt_detalle[, .N, by = .(ano, mes)]

names(dt_detalle)[!names(dt_detalle) %in% names(dt)]
  # La diferencia son las columnas Responsabilidades, Funciones y Requisitos.

# Join (left join, mantengo dt que contiene las observaciones con link NA)
setkey(dt, "link")
setkey(dt_detalle, "link")
dt[dt_detalle, on = "link"]
dt_detalle[dt, on = "link"] %>% names(.)
dt_detalle[dt, on = "link", .(paste0("i.", names(dt)[names(dt) != "link"])) := mget()]
dt_detalle[dt, on = "link", names(dt):= mget(paste0("i.", names(dt)))]
dt_detalle %>% names()

# Limpieza Gallito --------------------------------------------------------
dt[, sort(names(dt))]
dt[, names(dt) := lapply(.SD, tolower)]
dt[, names(dt) := lapply(.SD, trimws)]

dt[, lapply(.SD, class)]
dt[, lapply(.SD, uniqueN)]

# Generación ~ fecha publicación
dt[, table(fecha)]

dt[, fecha := trimws(fecha) %>% 
     gsub("hace","", x = .) %>% 
     gsub("segundos", "segundo", .) %>% 
     gsub("minutos", "minuto", .) %>% 
     gsub("días", "dia", .) %>% 
     gsub("día", "dia", .) %>% 
     gsub("horas", "hora", .) %>% 
     gsub("semanas", "semana", .) %>% 
     gsub("meses", "mes", .) %>% 
     trimws(.)]
dt[, c("cantidad", "unidad") := data.table::tstrsplit(fecha, " ", type.convert = TRUE)]
dt[, table(unidad)]

# Convierto a segundos
minutos = 60
horas = 60*minutos
dias = 24*horas
semanas = 7*dias
meses = 30*dias

dt[, unidad_seg := meses
   ][unidad == "minutos", unidad_seg := minutos
     ][unidad == "hora",    unidad_seg := horas
       ][unidad == "dia",     unidad_seg := dias
         ][unidad == "semana",  unidad_seg := semanas
           ][unidad == "segundo", unidad_seg := cantidad]

dt[, unidad_seg]
dt[, fecha_pub := as.POSIXct(fecha_scraping) - (as.integer(cantidad) * unidad_seg)]
dt[, data.table::tstrsplit(fecha_pub, " ", type.convert = TRUE)]
dt[, c("fecha_pub", "h_pub") := data.table::tstrsplit(fecha_pub, " ", type.convert = TRUE)]
dt[, fecha_pub := as.Date(fecha_pub, format = "%Y-%m-%d")]
dt[, `:=`(cantidad   = NULL,
          unidad     = NULL,
          unidad_seg = NULL)]
names(dt)
dt[2, detalle]

# Limpieza de texto


# Guardo el archivo
saveRDS(dt, "./union-avisos/gallito-compatabilizado.rds")
