# Limpiar nombres
clean_text <- function(x, pagina) {
  x[is.na(x)] = ""
  
  # Remplazo unicode
  
  # Remplazo carácters especiales (unicode que no se como tal)
  
  # Remplazo puntuación
  
  # Remplazo números
  
  # Separo palabras juntas
  
  # Remuevo palabras que aparecen repetidas consecutivamente
  
  # Separar dígitos de palabras
  
  # Minúsculas
  
  # Remuevo acentos
  
  # Remuevo más de 2 espacios
  
  # Limpiar espacios en izq-der 
  
  
  # remplazas puntuación
  stringi::stri_replace_all(str = x, replacement = " ", regex = "[:punct:]") %>%
    # Separar palabras juntas
    gsub(., pattern = "([a-z])([A-Z])", replacement = "\\1 \\2") %>% 
    # Separar dígitos de palabras
    gsub(., pattern = "(\\d)([A-Z])", replacement = "\\1 \\2") %>% 
    gsub(., pattern = "([A-Z])(\\d)", replacement = "\\1 \\2") %>% 
    # Limpiar el siguiente unicode
    # gsub(., pattern = "\u0095\t|\u0095", replacement = " ") %>% 
    # Limpiar más de 2 espacios o tabs..
    stringi::stri_replace_all(str = ., replacement = " ", regex = "[\\s]{2,}") %>%
    # Limpiar espacios en izq-der 
    stringi::stri_trim_both(.) %>%
    # Minúsculas
    tolower(.) %>% 
    # Limpiar palabras que aparecen repetidas consecutivamente, quedarse solo con 1
    gsub(., pattern = "\\b(\\w+)(\\s+\\1\\b)+", replacement = "\\1") %>% 
    # /\b(\S+)(?:\s+\1\b)+/i
    # (\\b\\S+\\b)(($|\\s+)\\1)+
    # Si es computrabajo limpiar la primera vez que aparezca "Descripción"
    {if(pagina == "computrabajo") {
      stringi::stri_replace_first(str = ., fixed = "descripción ", replacement = "")
    } else .}
  # Si es buscojobs limpiar la primera vez que aparezca "Buscamos:"
  
  # Si gallito
}
# Remover stopwords
remove_stopwords <- function(x) {
  temp <- unlist(stringi::stri_split(x, regex = " "))
  temp[!temp %in% tm::stopwords(kind = "es")] %>% 
    paste(., collapse = " ")
}
# Limpiar la columna seleccionada de dt
clean_column <- function(dt, column, pagina = "no") {
  dt[, (column) := {
    lapply(dt[, get(column)], function(x){
      clean_text(x, pagina) %>% 
        remove_stopwords(.)
    }) %>% unlist(.)
  }]  
}




# probando
library(magrittr)
temp = "\tasociados s.a sa .s.a. sa. s.a.  12 puestos - arriba U+1F600" 
stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")

# Remuevo tab, newline, vertical tab, form feed, carriage return, space y más
stringi::stri_replace_all(temp, replacement = " ", regex = "\\s") %>% 
  # Remuevo posibles signos de unicode, todo lo que no sea alfa númerico
  # Remuevo puntuación
  # Remuevo dígitos
  stringi::stri_replace_all(temp, replacement = " ", regex = "[^a-zA-Z]") %>% 
  # # Remuevo todos los signos de puntuación
  #   stringi::stri_replace_all(., replacement = " ", regex = "[:punct:]")
  stringr::str_replace_all(., "[\\s]+", " ")


# Limpiar nombres   sessionInfo()
clean_text <- function(x, pagina) {
  x[is.na(x)] = ""
  
  # Remplazo unicode
  # Remplazo carácters especiales (unicode que no se como tal)
  # Remplazo puntuación
  
  # Remplazo números
  
  # Separo palabras juntas
  
  # Remuevo palabras que aparecen repetidas consecutivamente
  
  # Separar dígitos de palabras
  
  # Minúsculas
  
  # Remuevo acentos
  
  # Remuevo más de 2 espacios
  
  # Limpiar espacios en izq-der 
  
  
  # remplazas puntuación
  stringi::stri_replace_all(str = x, replacement = " ", regex = "[:punct:]") %>%
    # Separar palabras juntas
    gsub(., pattern = "([a-z])([A-Z])", replacement = "\\1 \\2") %>% 
    # Separar dígitos de palabras
    gsub(., pattern = "(\\d)([A-Z])", replacement = "\\1 \\2") %>% 
    gsub(., pattern = "([A-Z])(\\d)", replacement = "\\1 \\2") %>% 
    # Limpiar el siguiente unicode
    # gsub(., pattern = "\u0095\t|\u0095", replacement = " ") %>% 
    # Limpiar más de 2 espacios o tabs..
    stringi::stri_replace_all(str = ., replacement = " ", regex = "[\\s]{2,}") %>%
    # Limpiar espacios en izq-der 
    stringi::stri_trim_both(.) %>%
    # Minúsculas
    tolower(.) %>% 
    # Limpiar palabras que aparecen repetidas consecutivamente, quedarse solo con 1
    gsub(., pattern = "\\b(\\w+)(\\s+\\1\\b)+", replacement = "\\1") %>% 
    # /\b(\S+)(?:\s+\1\b)+/i
    # (\\b\\S+\\b)(($|\\s+)\\1)+
    # Si es computrabajo limpiar la primera vez que aparezca "Descripción"
    {if(pagina == "computrabajo") {
      stringi::stri_replace_first(str = ., fixed = "descripción ", replacement = "")
    } else .}
  # Si es buscojobs limpiar la primera vez que aparezca "Buscamos:"
  
  # Si gallito
}
# Remover stopwords
remove_stopwords <- function(x) {
  temp <- unlist(stringi::stri_split(x, regex = " "))
  temp[!temp %in% tm::stopwords(kind = "es")] %>% 
    paste(., collapse = " ")
}
# Limpiar la columna seleccionada de dt
clean_column <- function(dt, column, pagina = "no") {
  dt[, (column) := {
    lapply(dt[, get(column)], function(x){
      clean_text(x, pagina) %>% 
        remove_stopwords(.)
    }) %>% unlist(.)
  }]  
}
