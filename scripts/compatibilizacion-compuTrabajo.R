# Compatibilización compuTrabajo
paquetes <- c("data.table", "magrittr", "stringi")
sapply(paquetes, require, character.only = TRUE)
# des_req = desc = descripción requerida
# Departamento == dpto
# cate == categorias
# Empresa == empresa
# Ciudad == ciudad

# últimos nombres que se están usando en el código de scraping
 # "ID"             "puesto"         "empresa"        "dpto"           "ciudad"         "fecha"          "fecha_scraping"
 # "categorias"     "desc"           "resumen"  

source(here::here("scripts", "read_files_rds.R"))
source(here::here("scripts", "clean_column.R"))
source(here::here("scripts", "clean_column_puesto.R"))
source(here::here("scripts", "clean_column_empresa.R"))
source(here::here("scripts", "clean_column_dpto.R"))
source(here::here("scripts", "delete.R"))
drop_col <- function(DT, drop_var, get_var) {
  DT[!is.na(get(drop_var)), (get_var) := get(drop_var)][, (drop_var) := NULL]
}

dt <- read_files(here::here("computrabajo"), .pattern = ".rds")
setkey(dt, "fecha_scraping")
# Al filtrar avisos quedarse con el más antiguo.

# Limpieza ----------------------------------------------------------------

# Normalizo columnas
drop_col(dt, drop_var = "Ciudad", get_var = "ciudad")
drop_col(dt, drop_var = "Departamento", get_var = "dpto")
drop_col(dt, drop_var = "Empresa", get_var = "empresa")
drop_col(dt, drop_var = "cate", get_var = "categorias")
drop_col(dt, drop_var = "des_req", get_var = "desc")
dt[, hora := NULL]
dt[!is.na(links), links := paste0("https://www.computrabajo.com.uy", links)]
drop_col(dt, drop_var = "links", get_var = "ID")
dt[is.na(ID), .N] # De qué fecha_scraping son?
dt[is.na(ID), table(fecha_scraping)] # Solamente las de noviembre 2018, diciembre esta OK
dt[year(fecha_scraping) == 2018 & month(fecha_scraping) == 12, .N]
dt[year(fecha_scraping) == 2018 & month(fecha_scraping) == 12, length(unique(ID))]
dt[year(fecha_scraping) == 2018 & month(fecha_scraping) == 12 & !duplicated(dt, by = "ID"), .N]

# Distribución por año-mes previo al filtrado
dt[, .N, by = .(year(fecha_scraping), month(fecha_scraping))]

# Se remueven link repetidos, que son causados principalmente porque se solapan avisos entre != fechas scraping (se obtenían más de 30 días)
# Primero voy a filtrar avisos que no tienen ID (fecha_scraping de noviembre), acá da igual fromLast porque son todos de la misma fecha_scrap
dt[is.na(ID), table(fecha_scraping)]
dt[is.na(ID), ][grepl(pattern = "^\\d+", x = fecha), gsub(x = fecha, pattern = "\\d{1,2}\\s(.+)", replacement = "\\1") %>% table]
# Ok, entonces:
# junio = 3
# julio = 28
# agosto = 243
# setiembre = 977
# octubre = 1152
# noviembre = 1279
# El único que puede aumentar su cantidad es noviembre, si el resto aumenta es porque hay error (quedan duplicados)
dt[month(fecha_scraping) == 12 & year(fecha_scraping) == 2018, 
   ][grepl(pattern = "^\\d+", x = fecha), gsub(x = fecha, pattern = "\\d{1,2}\\s(.+)", replacement = "\\1") %>% table]
# Notar como se solapan avisos y aumenta noviembre y octubre (y aumentan más considerando el scraping de enero)
# Lo que puedo hacer es, desde diciembre en adelante identificar todos los avisos de 24 de noviembre hacia atrás y borrarlos, ya que,
# están contenidos en el scraping del 24 de noviembre aunque no tienen link.
# Ó borrar por completo el scraping de noviembre sin link y tomar los scraping de diciembre en adelante.
# Esta última parece la mejor opción, ya que, quedan contenidos los avisos de noviembre-octubre e incluso parte de setiembre en el scraping de dic.
# Además los avisos de setiembre no se van a tomar en cuenta.
# Conclusión:
# SE BORRAN LOS AVISOS SIN ID correspondientes al scraping de noviembre.
dt <- dt[fecha_scraping != "2018-11-24 03:00:55", ]
dt[is.na(ID), .N]
# AHORA TODOS LOS AVISOS TIENEN ID.

# Filtrado de avisos
dup = dt[, .I[(duplicated(dt, by = c("ID"), fromLast = FALSE))]] # CORREGIR DEBERÍA SER FALSE
delete(dt, dup) # Borrados
dt[, table(fecha_scraping)] 
dt[, class(fecha_scraping)]
# Demasiados avisos el 22/02/2019, revisar posible error (repetidos?).
dt[fecha_scraping == "2019-02-22 17:42:29 -03", ID]
dt[grepl(pattern = "^\\d+", x = fecha), gsub(x = fecha, pattern = "\\d{1,2}\\s(.+)", replacement = "\\1") %>% table] 
dt[, .N, keyby = .(year(fecha_scraping), month(fecha_scraping))]
# REVISAR!
# Otra opción es ir borrarno las fechas en las que se solapan los scraping.

# LIMPIAR TEXTO:
# resumen, categorias, desc
# empresa, dpto, puesto

clean_column(dt, "resumen", pagina = "computrabajo")
clean_column(dt, "categorias", pagina = "computrabajo")
clean_column(dt, "desc", pagina = "computrabajo")
# Los warnings son por las filas vacías. Pero esta correcto. Para chequear cargar de nuevo los datos, filtrar y observar que los nros coinciden.

clean_column_empresa(dt, "empresa", pagina = "computrabajo")
clean_column_dpto(dt, "dpto", pagina = "computrabajo")
clean_column_puesto(dt, "puesto", pagina = "computrabajo")

# Porqué fue que no se pasaron a NA? Creo que era porque generaba problema al limpiar el texto
dt[resumen == "NA", resumen := NA_character_]
dt[categorias == "NA", categorias := NA_character_]
dt[desc == "NA", desc := NA_character_]


# Generación de fecha -----------------------------------------------------
dt[, fecha_scraping := as.POSIXct(fecha_scraping)]
dt[, fecha := gsub(fecha, pattern = "[\\.\\,]", replacement = "") %>% 
     stringi::stri_replace_all_fixed(str = ., pattern = " m", replacement = "m") %>% 
     stringi::stri_replace_all_fixed(str = ., pattern = "setiembre", replacement = "septiembre")]
dt[is.na(fecha_scraping), .N]
dt[is.na(fecha), .N]
# Limpiar fecha requiere 2 patrones diferentes:
# 1. Si ayer -> fechascraping - 1
#    Si hoy  -> fechascraping 
# 2. else fecha (reformateada)

dt[, temp_fecha := {
  # Fijar Sys.setlocale()
  # correr en la terminal: sudo locale-gen es_ES.UTF-8
  lct <- Sys.getlocale("LC_TIME") # obtener el local actual
  # Fijar el local de España por defecto
  Sys.setlocale("LC_TIME", "es_ES.UTF-8")
  # es_UY.UTF-8
  # Filtro en 2 DT porque tienen != formato de fecha
  filtro <- .SD[, which(grepl(pattern = "ayer|hoy", ignore.case = TRUE, x = fecha))]
  # Formato 1
  dia_segundos = 60*60*24
  temp1 = .SD[filtro,]
  temp1[grepl(fecha, pattern = "ayer", ignore.case = TRUE), temp_fecha := (fecha_scraping - dia_segundos)]
  temp1[grepl(fecha, pattern = "hoy", ignore.case = TRUE), temp_fecha := fecha_scraping] # me olvido dif horaria en el dia
  temp1[, temp_fecha := as.Date(temp_fecha)]
  
  # Formato 2
  temp2 <- .SD[!filtro]
  temp2[, ano := year(fecha_scraping)]
  temp2[ 
      grepl(x = fecha, pattern = "octubre|noviembre|diciembre", ignore.case = TRUE) & 
       year(fecha_scraping) == 2019 & 
       month(fecha_scraping) %in% c(1, 2, 3), ano := 2018
  ]
  temp2[
    grepl(x = fecha, pattern = "octubre|noviembre|diciembre", ignore.case = TRUE) & 
    year(fecha_scraping) == 2019 &
    month(fecha_scraping) %in% c(10, 11, 12), ano := 2019
  ]
  temp2[, .(ano, fecha, fecha_scraping)]
  temp2[, temp_fecha := as.Date(paste(fecha, ano, sep = " "), format = "%d %B %Y")]
  # Volver al local anterior
  Sys.setlocale("LC_TIME", lct)
  temp2[, ano := NULL]
  
  mat <- matrix(nrow = NROW(.SD), ncol = 1, dimnames = list(NULL, "temp_fecha")) %>% 
          as.data.table()
  mat[, temp_fecha := as.Date(temp_fecha)]
  mat[filtro,  temp_fecha := temp1$temp_fecha]
  mat[!filtro, temp_fecha := temp2$temp_fecha]
  mat$temp_fecha
}]

# Revisar
dt[, .(temp_fecha, fecha, fecha_scraping)] #Perfeito!
dt[is.na(temp_fecha), .N]
dt[is.na(fecha), .N]
dt[is.na(fecha_scraping), .N]

dt[, fecha_pub := temp_fecha][, `:=`(fecha = NULL,
                                     temp_fecha = NULL)]

# # Genera la fecha de publicación del aviso a partir de fecha y fecha_scraping
# dt[, c("fecha_scraping", "hora_scraping") := data.table::tstrsplit(fecha_scraping, " ", type.convert = TRUE)]

# Creación año y mes
dt[, `:=`(ano_scr = year(fecha_scraping),
          mes_scr = month(fecha_scraping),
          dia_scr = mday(fecha_scraping))]
dt[, `:=`(ano = year(fecha_pub),
          mes = month(fecha_pub),
          dia = mday(fecha_pub))]
dt[, .N, keyby  = .(ano, mes)]

# Pasos siguientes --------------------------------------------------------

# Check final
dt[, .N, keyby  = .(ano, mes)]
dt[!duplicated(dt, by = c("empresa", "puesto")), .N, keyby = .(ano, mes)]
dt[duplicated(dt, by = c("empresa", "puesto")), .N, keyby = .(ano, mes)]    # Potenciales avisos repetidos por empresa-puesto
dt[duplicated(dt, by = c("empresa", "puesto")),] %>% View()
# Claramente hay avisos repetidos empresa-puesto con muy pocos días de diferencia, pese a ID diferente.
# El problema con los primeros scraping es que no tienen texto.
# Ello va a generar que no se puedan identificar como repetidos en el análisis de texto.
# Cuántos no tienen texto en la descripción?
dt[is.na(desc), .N]
dt[is.na(desc), .N, keyby = .(ano, mes)]
# Solamente los de 2018.

# Filtrar fechas desde octubre 2018 en adelante.
dt <- dt[fecha_pub >= "2018-10-01", ]

# Voy a ser más exigente con estos avisos y filtrar por empresa-puesto, ya mismo??? No, es preferible hacerlo más adelante cuando se filtran las 3 
# páginas. ¿Por qué? porque suma información tener esos avisos repetidos por empresa-puesto lo cual puede ser una aproximación para los avisos pasados.

   
# Guardo
saveRDS(dt, "./Datos/Intermedias/computrabajo-compatibilizado.rds")
