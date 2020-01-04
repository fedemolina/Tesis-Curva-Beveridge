# Explicación del scrip
#
#

paquetes <- c("dplyr", "lubridate", "ggplot2", "data.table")
sapply(paquetes, require, character.only = TRUE)

dt <- readxl::read_xls(here::here("Datos", "CIUO88", "listado-ocupaciones-ciuo08.xls")) %>% 
        data.table::as.data.table(.)
# Limpiar nombres
clean_names = function(dt) {
    names_dirty = names(dt)
    names_clean = names(dt) %>%  
                    gsub(pattern = " ", replacement = "_") %>% 
                    gsub(x = ., pattern = ",", replacement = "") %>% 
                    
        
                    tolower()
    largo = nchar(names_clean) %>% which.max
    names_clean[largo] = "grupos"
    names_clean[1] = "denominaciones"
    return(names_clean)
}
# Renombrar
data.table::setnames(dt, old = names(dt), new = clean_names(dt))
# Limpiar nombre variables
cols = c("denominaciones", "grupos")
dt[, `:=`(denominaciones = tolower(denominaciones),
          grupos = tolower(grupos))]

dig_1 <- dt[nchar(codigo_ciuo_08) == 1, .(codigo_ciuo_08, grupos)]
dig_2 <- dt[nchar(codigo_ciuo_08) == 2, .(codigo_ciuo_08, grupos)]
dig_3 <- dt[nchar(codigo_ciuo_08) == 3, .(codigo_ciuo_08, grupos)]
dig_4 <- dt[nchar(codigo_ciuo_08) == 4, .(codigo_ciuo_08, grupos)] %>% unique()

denom <- dt[!is.na(denominaciones), .(denominaciones, codigo_ciuo_08)]

dig_4$grupos[grepl(x = dig_4$grupos, pattern = "cocin", fixed = TRUE)]


# El país
ga <- readxl::read_xlsx(here::here("Datos", "Originales", "Gallito-2013-2018.xlsx"), 
                        na = c("", " ", "-", ".", ".."), 
                        col_names = c("fecha", "area", "puesto", "puesto_anun", 
                                      "resp", "url", "estudio", "jera",
                                      "cat", "desc", "anun",
                                      "fun", "prog", "carrera",
                                      "ubic", "exp", "idioma"),
                        skip = 1, col_types = c("date", rep("text", 16))) %>% as.data.table()

clean_caracters <- function(x) {
    x %>% 
        gsub(x = ., "\\n", "") %>% 
        gsub(x = ., "\\r", "") %>% 
        gsub(x = ., "-", "") %>% 
        gsub(x = ., ":", "") %>% 
        gsub(x = ., ",", "") %>% 
        gsub(x = ., "!", "") %>% 
        gsub(x = ., "¡", "") %>% 
        trimws(x = .) %>% 
        tolower(.) %>% 
        iconv(x = ., from = "UTF-8", to = "ASCII//TRANSLIT")
}

clean_caracters(ga$puesto)
