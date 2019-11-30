# Compatibilización compuTrabajo
paquetes <- c("data.table", "magrittr")
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
source(here::here("scripts", "clean_text.R"))
drop_col <- function(DT, drop_var, get_var) {
  DT[!is.na(get(drop_var)), (get_var) := get(drop_var)][, (drop_var) := NULL]
}

dt <- read_files(here::here("computrabajo", "csv"), .pattern = ".csv")

drop_col(dt, drop_var = "Ciudad", get_var = "ciudad")
drop_col(dt, drop_var = "Departamento", get_var = "dpto")
drop_col(dt, drop_var = "Empresa", get_var = "empresa")
drop_col(dt, drop_var = "cate", get_var = "categorias")
drop_col(dt, drop_var = "des_req", get_var = "desc")
dt[, links := paste0("https://www.computrabajo.com.uy", links)]
drop_col(dt, drop_var = "links", get_var = "ID")
dt[, hora := NULL]

# Genera la fecha de publicación del aviso a partir de fecha y fecha_scraping
dt[, c("fecha_scraping", "hora_scraping") := data.table::tstrsplit(fecha_scraping, " ", type.convert = TRUE)]

# Creación año y mes
dt[, `:=`(ano = year(fecha_scraping),
          mes = month(fecha_scraping),
          dia = mday(fecha_scraping))]

# Repetidos por link
dt[is.na(ID), .N] # Ninguno.
dt[!is.na(ID), uniqueN(ID)/.N, keyby = .(ano, mes)]
dt[!is.na(ID), sum(duplicated(ID))/.N, keyby = .(ano, mes)]
dt[!is.na(ID), sum(duplicated(ID))/uniqueN(ID), keyby = .(ano, mes)]

dup = dt[, .I[!is.na(ID) & duplicated(dt, by = c("ID"))]]
delete(dt, dup) # Borrados

# Filtrar repetidos SIN link. Es correcto esto?
dt[is.na(ID), .N]
dt[duplicated(dt, by = c("puesto", "empresa", "dpto", "ciudad", "categorias", "resumen")),]

dup = dt[,duplicated(dt, by = c("puesto", "empresa", "dpto", "ciudad", "categorias", "resumen"))]
delete(dt, dup) # Borrados
# Fin repetidos sin link.





# Limpir texto
# resumen, categorias, desc, 
dt[1:10, clean_text(resumen)]
dt[1:10, clean_text(categorias)]
dt[1:10, clean_text(desc)]

# patrones: 
# Dos palabras juntas minúsculaMayúscula
# númeroletra ó númeroLetra

dt[9, desc %>% 
     gsub(x = ., "-|\\.", "") %>% 
     gsub(x = ., "([a-z])([A-Z])", "\\1 \\2") %>% 
     gsub(x = ., "(\\d)([A-Z]|[a-z])", "\\1 \\2") %>% 
     tolower(.)
     ]
dt[1:10, clean_text(desc)]
dt[1:9, cat(desc)]
dt[9:10, desc]
dt[9:10, cat(desc)]
dt[9, stringr::str_split(desc, pattern = "Requerimientos")]
dt[9, strsplit(x = desc, split = "Requerimientos", fixed = TRUE)]

clean_descripcion <- function(x) {
    gsub(x = x, "([a-z])(\\.)([A-Z])", "\\1 \\3") %>% 
    gsub(x = ., "([A-Z])(\\.)([A-Z])", "\\1 \\3") %>% 
    gsub(x = ., "([a-z])(\\.)([a-z])", "\\1 \\3") %>% 
    gsub(x = ., "([a-z])(\\,)([a-z])", "\\1 \\3") %>% 
    gsub(x = ., "-|\\.|(Descripción)", "") %>%
    gsub(x = ., "([a-z])([A-Z])", "\\1 \\2") %>% 
    gsub(x = ., "(\\d)([A-Z]|[a-z])", "\\1 \\2") %>% 
    gsub(x = ., pattern = "\\(|\\)|\\$|,|\\.00", replacement = "") %>% 
    gsub(x = ., pattern = "\r|\n|\t|\\s{2,}", replacement = " ") %>% 
    gsub(x = ., pattern = "•", replacement = "") %>% 
    tolower(.) %>% 
    trimws(.) %>% 
    gsub(x = ., "rquisitos", replacement = "requisitos", fixed = TRUE)
}
dt[9, desc %>% 
     tstrsplit(x = ., split = "Requerimientos", fixed = TRUE)]

# Dividir desc (descripción) en dos variables: 1) Descripción
#                                              2) Requerimientos
dt[, c("descripcion", "requerimientos") := tstrsplit(desc, split = "Requerimientos\r\r\n", fixed = TRUE, type.convert = TRUE)]
dt[, descripcion := clean_descripcion(descripcion)] 
dt[9, descripcion]
dt[3, descripcion]
dt[4, descripcion]
# Crear:
# descripcion (Descripción), es el primer strsplit porque "Descripción" lo borre
# buscamos
# requisitos | buscamos:requsitos excluyentes:
# formación|formacion
# valora (se valora)
# horario (horario | horario de trabajo)
# fecha_contrato (fecha de contratacion)
# cant_vacantes (cantidad de vacantes)
# responsabilidades principales
# tareas
# se ofrece

# descripcion es el primer elemento al hacer strplit

# Limpieza de texto


# Guardo
saveRDS(dt, "./union-avisos/computrabajo-compatibilizado.rds")
