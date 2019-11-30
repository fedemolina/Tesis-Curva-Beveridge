clean_text <- function(x) {
  x %>% 
    gsub(x = ., "-|\\.", "") %>% 
    gsub(x = ., "([a-z])([A-Z])", "\\1 \\2") %>% 
    gsub(x = ., "(\\d)([A-Z]|[a-z])", "\\1 \\2") %>% 
    gsub(x = ., pattern = "\\(|\\)|\\$|,|\\.00", replacement = "") %>% 
    gsub(x = ., pattern = "\r|\n|\\s{2,}", replacement = " ") %>% 
    tolower(.) %>% 
    trimws(.)
}