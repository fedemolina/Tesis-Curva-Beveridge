# Limpiar nombres
clean_text_puesto <- function(x, pagina) {
  x[is.na(x)] = ""
  # remplazar puntuación
  stringi::stri_replace_all(str = x, replacement = " ", regex = "[[:punct:]]|[[:space:]]") %>%
    # Separar palabras juntas
    gsub(., pattern = "([a-z])([A-Z])", replacement = "\\1 \\2") %>% 
    # Separar dígitos de palabras
    gsub(., pattern = "(\\d)([A-Z])", replacement = "\\1 \\2") %>% 
    gsub(., pattern = "([A-Z])(\\d)", replacement = "\\1 \\2") %>% 
    # Limpiar más de 2 espacios o tabs..
    stringi::stri_replace_all(str = ., replacement = " ", regex = "[\\s]{2,}") %>%
    # Limpiar espacios en izq-der 
    stringi::stri_trim_both(.) %>%
    # Minúsculas
    tolower(.) %>% 
    # Limpiar palabras que aparecen repetidas consecutivamente, quedarse solo con 1
    gsub(., pattern = "\\b(\\w+)(\\s+\\1\\b)+", replacement = "\\1") %>% 
    # limpiar los ejemplos/as
    gsub(., pattern = "(.+)\\sas$", replacement = "\\1")
}
# Remover stopwords
remove_stopwords_puesto <- function(x) {
  temp <- unlist(stringi::stri_split(x, regex = " "))
  temp[!temp %in% c(tm::stopwords(kind = "es"), "as")] %>% 
    paste(., collapse = " ")
}
# Limpiar la columna seleccionada de dt
clean_column_puesto <- function(dt, column, pagina = "no") {
  dt[, (column) := {
    lapply(dt[, get(column)], function(x){
      clean_text_puesto(x, pagina) %>% 
        remove_stopwords_puesto(.)
    }) %>% unlist(.)
  }]  
}
