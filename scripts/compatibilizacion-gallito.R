# Compatabilización gallito
paquetes <- c("data.table", "magrittr", "stringi")
sapply(paquetes, require, character.only = TRUE)

source(here::here("scripts", "read_files_rds.R"))
source(here::here("scripts", "clean_column.R"))
source(here::here("scripts", "clean_column_puesto.R"))
source(here::here("scripts", "clean_column_empresa.R"))
source(here::here("scripts", "clean_column_dpto.R"))
source(here::here("scripts", "delete.R"))

# Lista de archivos con scraping NO detallado
archivos <- list.files(here::here("gallito"), full.names = TRUE, pattern = ".rds")

# Lista de archivos con scraping detallado
archivos_detalle <- list.files(here::here("gallito", "detallado"), full.names = TRUE, pattern = ".rds")

# Lectura NO detallados
lista = list()
for (i in archivos) {
  lista[[i]] <- readRDS(file = i)
}
lapply(lista, names)

dt <- data.table::rbindlist(lista, use.names = TRUE, fill = TRUE, idcol = "file")
dt[is.na(departamento), departamento := dpto]
dt[, dpto := NULL]
rm(lista)

# Lectura detallados
lista = list()
for(i in archivos_detalle) {
  lista[[i]] <- readRDS(file = i)
}
lapply(lista, names)

dt_detalle <- data.table::rbindlist(lista, use.names = TRUE, fill = TRUE, idcol = "file")
dt_detalle[is.na(departamento), departamento := dpto]
dt_detalle[, dpto := NULL]


# Limpieza ----------------------------------------------------------------

# Tuve la mala idea de modificar el formato de links algunos comienzan con:
# https://trabajo.gallito.com.uy/anuncios/...
# anuncios/...
# Por lo tanto se corrige, dejando el formato actual.
dt[!stringr::str_detect(link, pattern = "https://trabajo.gallito.com.uy"), 
   link := paste0("https://trabajo.gallito.com.uy", link)]

dt_detalle[!stringr::str_detect(link, pattern = "https://trabajo.gallito.com.uy"), 
   link := paste0("https://trabajo.gallito.com.uy", link)]

# minúsculas puesto, empresa, nivel, area
cols <- c("puesto", "empresa", "nivel", "area")
dt[, (cols) := lapply(.SD, tolower), .SDcols = cols]
dt_detalle[, (cols) := lapply(.SD, tolower), .SDcols = cols]

# nombres de columnas
setnames(dt_detalle, old = names(dt_detalle), new = tolower(names(dt_detalle)))
setnames(dt, old = names(dt), new = tolower(names(dt)))

# Creación año y mes del scraping
generacion_fecha <- function(dt) {
  dt[, fecha_scraping := as.POSIXct(fecha_scraping)]
  dt[, fecha := tolower(fecha) %>% 
       gsub("hace","", x = ., fixed = TRUE) %>% 
       gsub("segundos", "segundo", .) %>% 
       gsub("minutos", "minuto", .) %>% 
       gsub("días", "dia", .) %>% 
       gsub("día", "dia", .) %>% 
       gsub("horas", "hora", .) %>% 
       gsub("semanas", "semana", .) %>% 
       gsub("meses", "mes", .) %>% 
       trimws(.)]
  dt[, c("cantidad", "unidad") := data.table::tstrsplit(fecha, " ", type.convert = TRUE)]
  
  # Convierto a segundos
  minutos = 60
  horas = 60*minutos
  dias = 24*horas
  semanas = 7*dias
  meses = 30*dias
  
  dt[, unidad_seg := meses
     ][unidad == "minuto", unidad_seg := minutos
       ][unidad == "hora",    unidad_seg := horas
         ][unidad == "dia",     unidad_seg := dias
           ][unidad == "semana",  unidad_seg := semanas
             ][unidad == "segundo", unidad_seg := cantidad]
  
  dt[, fecha_pub := fecha_scraping - (as.integer(cantidad) * unidad_seg)]
  dt[, c("fecha_pub", "h_pub") := data.table::tstrsplit(fecha_pub, " ", type.convert = TRUE)]
  dt[, fecha_pub := as.Date(fecha_pub, format = "%Y-%m-%d")]
  dt[, `:=`(cantidad   = NULL,
            unidad     = NULL,
            unidad_seg = NULL,
            h_pub = NULL)]
  dt[, `:=`(ano = year(fecha_pub),
            mes = month(fecha_pub),
            dia = mday(fecha_pub))]
  
  dt[, `:=`(ano_scr = year(fecha_scraping),
            mes_scr = month(fecha_scraping),
            dia_scr = mday(fecha_scraping))]
}
generacion_fecha(dt = dt)
generacion_fecha(dt = dt_detalle)

# Los primeros gallitos (sin detalle) no tienen fecha de scraping, eso fue un error, todos deben tener. La misma se puede obtener del nombre del archivo.
dt[!grepl(file, pattern = ".*_\\s?\\d*-\\d*-\\d{2}.rds") & is.na(fecha_scraping), 
   fecha_scraping := as.POSIXct(gsub(x = file, pattern = ".*_\\s?(\\d*-\\d*-\\d{2})\\s?(\\d*)-(\\d*)-(\\d*)?.rds", replacement = "\\1 \\2:\\3:\\4"))]


# Filtrar los avisos por link, dado que las fechas de scraping se colapsan. Ordenr por fecha scraping y quedare con los obtenidos en el scraping más viejo.
# cols = c("link", "fecha_pub")
cols = "fecha_scraping"
setkeyv(dt, cols)
setkeyv(dt_detalle, cols)
# Ordenar en base a link y fecha_pub de forma de quedarse con el aviso repetido más antiguo.

# avisos sin link, filtrarlos por la mayor cantidad de variables
# Para ello, notar que hay variables con ""
# empresa = 'importante empresa' es lo mismo que un missing.
# dt[empresa == "" | empresa == "importante empresa", empresa := NA_character_]
# dt[puesto == "",  puesto := NA_character_]
# dt[nivel == "", nivel := NA_character_]
# dt[area == "", area := NA_character_]
# dt[detalle == "", detalle := NA_character_]

dt[ano == 2018, table(fecha_scraping)]
dt[ano == 2018, table(fecha_pub)]
duplicados <- dt[, .I[is.na(link) & duplicated(dt, by = c("empresa", "puesto", "nivel", "area", "detalle"), fromLast = FALSE)]]
# dt <- dt[!(is.na(link) & duplicated(dt, by = c("empresa", "puesto", "nivel", "area", "detalle"), fromLast = FALSE)), ]
delete(dt, duplicados)
# test
dt[is.na(link) & duplicated(dt, by = c("empresa", "puesto", "nivel", "area", "detalle"))]
# perfecto.

# Filtrar link repetidos (que no son missing en el caso dt)
duplicados <- dt[, .I[!is.na(link) & duplicated(dt, by = "link", fromLast = FALSE)]]
delete(dt, duplicados)

dt_detalle <- dt_detalle[!duplicated(dt_detalle, by = "link", fromLast = FALSE),]

# Limpiar responsabilidades, funciones y detalle.
clean_column(dt_detalle, "responsabilidades")
clean_column(dt_detalle, "funciones")
clean_column(dt_detalle, "requisitos")
clean_column(dt_detalle, "detalle")
clean_column(dt, "detalle")

# Dejar solo el nombre del archivo
dt[, file := stri_split_fixed(str = file, pattern = "/csv/", n = 2, tokens_only = TRUE) %>% 
     transpose() %>% `[`(2)]
dt_detalle[, file := stri_split_fixed(str = file, pattern = "/csv/", n = 2, tokens_only = TRUE) %>% 
     transpose() %>% `[`(2)]

dt_detalle[base::duplicated(dt_detalle[, link])]


# Avisos repetidos (se solapan las fechas de los scrap --------

# Primero hay que filtrar por link, ello porque las fechas de scraping se solapan, entonces hay avisos que quedan más de 
# una vez. Tomar la fecha más antigua de publicación.
# A la vez, como están duplicados es necesario limpiarlos en ambas tablas antes de hacer el join.

# Ejemplo
dt[link == "https://trabajo.gallito.com.uy/anuncio/virtual-assistant-jtbqb", ]
dt[link == "https://trabajo.gallito.com.uy/anuncio/virtual-assistant-jtbqb", 
   ][, duplicated(.SD, by = "link", fromLast = TRUE)]
dt[!duplicated(dt, by = "link", fromLast = FALSE),][link == "https://trabajo.gallito.com.uy/anuncio/virtual-assistant-jtbqb",]

# dt tiene avisos sin link, porque, al comienzo no se agrego. Filtrar por otras variables.
# empresa, puesto, departamento, area, nivel, detalle son candidatos
# departamento descartar porque mismos avisos aveces aparecen con o sin departamento
dt[is.na(link),]
dt[is.na(link) & duplicated(dt, by = c("empresa", "puesto", "nivel", "area"))]
dt[is.na(link) & duplicated(dt, by = c("empresa", "puesto", "nivel", "area", "detalle"))]
# cerca de 300 avisos de diferencia.
dt[is.na(link) & 
     duplicated(dt, by = c("empresa", "puesto", "nivel", "area")) &
     !duplicated(dt, by = "detalle"),]

# Lo más correcto es usar también detalle. (2 formas de hacerlo)
# IMPORTANTE discutir: Borrar duplicados diferentes del link si o no ??
# Los avisos recolectados manualmente NO discriminan duplicados. Discutir con Rodrigo

# Obs: Puede pasar que aún esten repetidos (en dt) y que tengan detalle distinto porque como se puede ver en dt_detalle
# algunos avisos tienen responsabilidades/funciones intercambiadas, pero visto en conjunto son el mismo aviso pese a que el 
# link sea diferente, ejemplo: 
# https://trabajo.gallito.com.uy/anuncio/abogado-junior-br2qj
# https://trabajo.gallito.com.uy/anuncio/abogado-junior-6jq43


####---

# Repetidos por c("puesto", "empresa", "nivel", "area") una vez limpiados los link y is.na(link)
dt[duplicated(dt, by = c("puesto", "empresa", "nivel", "area")), .N]
dt[duplicated(dt, by = c("puesto", "empresa", "nivel", "area", "detalle")), .N]
dt[duplicated(dt, by = c("detalle")), .N]
dt[duplicated(dt, by = c("puesto", "empresa", "nivel", "area")), ]

dt[duplicated(dt, by = c("puesto", "empresa", "nivel", "area")), .N, keyby = .(ano, mes)]
dt[duplicated(dt, by = c("puesto", "empresa", "nivel", "area", "detalle")), .N, keyby = .(ano, mes)]
dt[duplicated(dt, by = c("puesto", "empresa", "nivel", "area", "detalle")), .N, keyby = .(ano_scr, mes_scr)]
dt[duplicated(dt, by = c("puesto", "empresa", "nivel", "area", "link")), .N] 

dt_detalle[duplicated(dt_detalle, by = c("puesto", "empresa", "nivel", "area", "departamento", 
                                         "responsabilidades", "funciones", "requisitos", "detalle"))] 
dt_detalle[duplicated(dt_detalle, by = c("puesto", "empresa", "nivel", "area", "departamento", 
                                         "responsabilidades", "funciones", "requisitos", "detalle")), .N] 
dt_detalle[duplicated(dt_detalle, by = c("puesto", "empresa", "nivel", "area", 
                                         "responsabilidades", "funciones", "requisitos", "detalle")), .N, keyby = .(ano, mes)] 
dt_detalle[duplicated(dt_detalle, by = c("puesto", "empresa", "nivel", "area", "departamento", "detalle")), .N] 
dt_detalle[duplicated(dt_detalle, by = c("puesto", "empresa", "nivel", "area", "departamento", "link")), .N] 


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
setkeyv(dt, c("link", "fecha_pub"))
setkeyv(dt_detalle, c("link", "fecha_pub"))
dt[dt_detalle, on = "link"] %>% names()
dt_detalle[dt, on = "link"] %>% names()
dt[dt_detalle, on = "link"]
dt_detalle[dt, on = "link"]

# código previo
# dt_detalle[dt, on = "link", names(dt) := mget(paste0("i.", names(dt)))]

#Probando
dt[dt_detalle, on = "link", names(dt_detalle) := mget(paste0("i.", names(dt_detalle)))]

# cols <- names(DF2)[3:4]
# DF1[DF2, on = .(date, id), (cols) := mget(paste0("i.", cols))]
# https://stackoverflow.com/questions/37993924/replace-specific-values-based-on-another-dataframe/37994369#37994369

# Limpieza Gallito --------------------------------------------------------

# Limpieza de texto de las siguientes columnas:
# detalle, aunque esta es menos relevante porque es la información resumida que se ve antes de entrar al aviso
# responsabilidades
# funciones
# requisitos
# puesto, vale la pena?
# empresa
# departamento, tiene que quedar igual para todas las páginas.

# dt[, detalle := NULL] No borrar detalle porque sirve para identificar avisos en 2018 que no tienen responsabilidades, funciones, requisitos...
# Importante porque claramente hay avisos repetidos en noviembre y diciembre.
# Sin embargo, puede pasar que sean el mismo aviso y poner información diferente en el detalle. En caso que sea igual el detalle es una variable importante para identificar.
# Necesario ser más exigente con 2018 porque hay 2176 avisos sin link en donde pueden haber muchos repetidos.

# pnud uru / 16 / 006 es pnud, arreglar
dt[, empresa := trimws(empresa)]
dt[empresa == "" | empresa == "importante empresa" | empresa == "confidencial" | empresa == "empresa" | is.null(empresa), empresa := NA_character_]
dt[puesto == "",  puesto := NA_character_]
dt[nivel == "", nivel := NA_character_]
dt[area == "", area := NA_character_]
dt[detalle == "", detalle := NA_character_]

# De nuevo fijar key en fecha scraping para quedarse con los avisos más antiguos
setkeyv(dt, c("fecha_scraping", "fecha_pub"))
dt[, .N, keyby = .(ano, mes)]
dt[, .N, keyby = .(ano_scr, mes_scr)]
dt[ano == 2018 & ano_scr == 2018, .(fecha_scraping, fecha_pub, mes, puesto, empresa, nivel, area, detalle)] #%>% View()
dt[duplicated(dt, by = c("puesto", "empresa", "nivel", "area","detalle"), fromLast = FALSE), .N, by = .(ano, mes)]
dt[ano == 2018 & duplicated(dt, by = c("puesto", "empresa", "nivel", "area","detalle")), 
   .(fecha_scraping, fecha_pub, puesto, empresa, nivel, area, detalle, responsabilidades, funciones), keyby = puesto] #%>% View()
dt[duplicated(dt, by = c("puesto", "empresa", "nivel", "area","detalle")), .N, by = .(ano, mes)]


duplicados <- dt[, .I[ano == 2018 & duplicated(dt, by = c("puesto", "empresa", "nivel", "area","detalle"), fromLast = FALSE)]]
delete(dt, duplicados)
dt[, .N, keyby = .(ano, mes)]
dt[ano == 2018 & duplicated(dt, by = c("puesto", "empresa", "nivel", "area", "detalle")), ]
dt[ano == 2018 & duplicated(dt, by = c("puesto", "empresa", "nivel", "area"), fromLast = FALSE), .N, by = .(ano, mes)]
dt[ano == 2018 & duplicated(dt, by = c("puesto", "empresa", "nivel", "area")), ] %>% View()
# Notar como aveces cambia levemente el nombre del puesto pero es lo mismo
# Ejemplo:
# guardias armados para cerro	
# guardias armados. cerro	

dt[duplicated(dt, by = c("puesto", "empresa", "nivel", "area", "departamento")), , keyby = fecha_scraping] # departamento al comienzo tiene missing
dt[duplicated(dt, by = c("puesto", "empresa", "nivel", "area", "departamento", "responsabilidades")), .N, by = .(ano, mes)]
# Siguen existiendo avisos duplicados.

dt[duplicated(dt, by = c("puesto", "empresa", "nivel", "area", "departamento"))]

# Guardo el archivo
saveRDS(dt, "./Datos/Intermedias/gallito-compatibilizado.rds")
