
url_tester <- function(url_list){
  url_list %>%
    # Map a version of read_lines() that otherwise returns 404
    purrr::map( purrr::possibly(readr::read_lines, otherwise = 404) ) %>%
    # Set the names of the result
    purrr::set_names( url_list ) %>% 
    # paste() and collapse each element
    purrr:::map(., paste, collapse =" ") %>%
    # Remove the 404 
    purrr::discard(~.x == 404) %>%
    names() # Will return the names of the good ones
}

# Try this function on the urls object
url_tester(urls)


url_tester <- function(url_list, type = c("result", "error")){
  res <- url_list %>%
    # Create a safely() version of read_lines() 
    purrr::map( purrr::safely(readr::read_lines)) %>%
    purrr::set_names( url_list ) %>%
    # Transpose into a list of $result and $error
    purrr::transpose() 
  # Complete this if statement
  if (type == "result") return( res$result ) 
  if (type == "error") return( res$error ) 
}

# Try this function on the urls object
url_tester(urls, type = "error") 



url_tester <- function(url_list){
  url_list %>%
    # Map a version of GET() that would otherwise return NULL 
    map( possibly(GET, NULL) ) %>%
    # Set the names of the result
    set_names( urls ) %>%
    # Remove the NULL
    compact() %>%
    # Extract all the "status_code" elements
    map(~.x[["status_code"]])
}

# Try this function on the urls object
url_tester(urls)