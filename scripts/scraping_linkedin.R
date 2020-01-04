
rm(list = ls())
library(rvest)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(robotstxt) # Con esta libreria corrobo si los datos "se pueden descargar".
library(httr)

# 1. Find OAuth settings for linkedin:
#    https://developer.linkedin.com/documents/linkedins-oauth-details
endpoints <- oauth_endpoints("linkedin")

# 2. Register an application at https://www.linkedin.com/secure/developer
#    Make sure to register http://localhost:1410/ as an "OAuth 2.0 Redirect URL".
#    (the trailing slash is important!)
#
#    Replace key and secret below.
myapp <- oauth_app("linkedin",
                   key = "86cnuna4revqln",
                   secret = "TgAajEVNAvvjGVO7"
)

# 3. Get OAuth credentials
token <- oauth2.0_token(endpoints, myapp)

# 4. Use API
req <- GET("https://api.linkedin.com/v1/people/~", config(token = token))
stop_for_status(req)
content(req)

############################

user_url <- "https://www.linkedin.com/in/federico-molina-magne/"

username <- 'federicoandresmolina@gmail.com'
password <- 'FedeMolina1899*'

library(rvest)   
library(tidyverse)

linkedin_url <- "https://linkedin.com/"
pgsession <- html_session(linkedin_url) 
pgform <- html_form(pgsession)[[1]]
filled_form <- set_values(pgform,
                          session_key = username, 
                          session_password = password)

submit_form(pgsession, filled_form)

pgsession <- jump_to(pgsession, user_url)
page_html <- read_html(pgsession)

pgsession <- submit_form(pgsession, filled_form) 
## CHECK THIS CODE 
pgsession <- jump_to(pgsession, user_url) 
page_html <- read_html(pgsession)
