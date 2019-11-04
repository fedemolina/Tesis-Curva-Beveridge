paquetes <- c("data.table","magrittr")
sapply(paquetes, require, character.only = TRUE)

dt <- readRDS(here::here("Datos","Intermedias","ech13-18.rds"))

