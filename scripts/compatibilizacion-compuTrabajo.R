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

source(here::here("scripts", "read_files.R"))
source(here::here("scripts", "clean_column.R"))
source(here::here("scripts", "delete.R"))
drop_col <- function(DT, drop_var, get_var) {
  DT[!is.na(get(drop_var)), (get_var) := get(drop_var)][, (drop_var) := NULL]
}

dt <- read_files(here::here("computrabajo", "csv"), .pattern = ".csv")


# Limpieza ----------------------------------------------------------------


# Normalizo columnas
drop_col(dt, drop_var = "Ciudad", get_var = "ciudad")
drop_col(dt, drop_var = "Departamento", get_var = "dpto")
drop_col(dt, drop_var = "Empresa", get_var = "empresa")
drop_col(dt, drop_var = "cate", get_var = "categorias")
drop_col(dt, drop_var = "des_req", get_var = "desc")
dt[, links := paste0("https://www.computrabajo.com.uy", links)]
drop_col(dt, drop_var = "links", get_var = "ID")
dt[, hora := NULL]

# Se remueven link repetidos, que son causados principalmente porque se solapan las fechas de scraping.
dup = dt[, .I[!is.na(ID) & duplicated(dt, by = c("ID"))]]
delete(dt, dup) # Borrados

# Limpiar texto

# resumen
# categorias
# desc

clean_column(dt, "resumen", pagina = "computrabajo")
clean_column(dt, "categorias", pagina = "computrabajo")
clean_column(dt, "desc", pagina = "computrabajo")


# Generación de fecha -----------------------------------------------------
dt[, fecha_scraping := as.POSIXct(fecha_scraping)]
dt[, fecha := gsub(fecha, pattern = "[\\.\\,]", replacement = "") %>% 
     stringi::stri_replace_all_fixed(str = ., pattern = " m", replacement = "m") %>% 
     stringi::stri_replace_all_fixed(str = ., pattern = "setiembre", replacement = "septiembre")]
# Limpiar fecha requiere 2 patrones diferentes:
# 1. Si ayer -> fechascraping - 1
#    Si hoy  -> fechascraping 
# 2. else fecha (reformateada)

dt[, temp_fecha := {
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
  temp2[, ano := NULL]
  
  mat <- matrix(nrow = NROW(.SD), ncol = 1, dimnames = list(NULL, "temp_fecha")) %>% 
          as.data.table()
  mat[, temp_fecha := as.Date(temp_fecha)]
  mat[filtro,  temp_fecha := temp1$temp_fecha]
  mat[!filtro, temp_fecha := temp2$temp_fecha]
  mat$temp_fecha
}]

dt[, .(temp_fecha, fecha, fecha_scraping)] #Perfeito!
dt[, fecha_pub := temp_fecha][, `:=`(fecha = NULL,
                                     temp_fecha = NULL)]

# Genera la fecha de publicación del aviso a partir de fecha y fecha_scraping
dt[, c("fecha_scraping", "hora_scraping") := data.table::tstrsplit(fecha_scraping, " ", type.convert = TRUE)]

# Creación año y mes
dt[, `:=`(ano_scr = year(fecha_scraping),
          mes_scr = month(fecha_scraping),
          dia_scr = mday(fecha_scraping))]
dt[, `:=`(ano = year(fecha_pub),
          mes = month(fecha_pub),
          dia = mday(fecha_pub))]


# Pasos siguientes --------------------------------------------------------


# Guardo
saveRDS(dt, "./Datos/Intermedias/computrabajo-compatibilizado.rds")
