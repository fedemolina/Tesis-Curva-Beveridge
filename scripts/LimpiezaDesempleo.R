librerias <- c("data.table", "magrittr")
sapply(librerias, require, character.only = TRUE)

directorio <- here::here("Datos", "Originales")
des80 <- paste(directorio, "Desempleo1980-2005.xls", sep = "/")
des04 <- paste(directorio, "Desempleo2004-2019.xls", sep = "/")
des80_original <- readxl::read_excel(des80, skip = 12, col_names = c("fecha1", "borrar1", "fecha2", "ta", "borrar2", "borrar3", 
                                                           "te", "borrar4", "borrar5", "td", "borrar6", "borrar7"))
des04_original <- readxl::read_excel(des04, skip = 12, col_names = c("fecha", "ta", "borrar1", "borrar2", 
                                                           "te", "borrar3", "borrar4", "td", "borrar5", "borrar6"))

des80 <- as.data.table(des80_original)
des04 <- as.data.table(des04_original)
str(des80)
str(des04)

# Limpieza ----------------------------------------------------------------

des80[, grep("^borrar", colnames(des80)) := NULL]
des04[, grep("^borrar", colnames(des04)) := NULL]

# Limpieza 1980
des80[, fecha2:=NULL]
des80 <- des80[complete.cases(des80)] # delete row by reference don't implemented yet
des80[te == "...", te := NA_character_]
des80 <- des80[grep(pattern = ("Enero|Abril|Julio|Octubre"), x = fecha1)]
names_character <- c("ta", "te", "td")
for(col in names_character) {
    set(des80, j = col, value = as.numeric(des80[[col]]))
} # This is the fastest way to do it.

# for(col in names_character) {
#     des80[, (col) :=as.numeric(des80[[col]])]
# } This is slower than the previous one when you have thousand of columns.

des80[,gsub("/8", x = fecha1, replacement = "198")]
des80[, ano:= as.numeric(sub(".*?[/](\\d+).*", "\\1", fecha1))][
    ano < 80, ano := as.numeric(paste0(200, ano))][
    ano < 2000, ano := as.numeric(paste0(19, ano))]

des80[, mes:=gsub("[/](\\d+)", x = fecha1, replacement = "")]

meses <- c("Enero", "Abril", "Julio", "Octubre")
for(m in meses) {
    if (m == "Enero"){
        des80[mes==m, mes:=01]
    } else if (m == "Abril") {
        des80[mes==m, mes:=04]
    } else if (m == "Julio") {
        des80[mes==m, mes:=07]
    } else {
        des80[mes==m, mes:=10]}
}
des80[, dia:=01]
des80[, fecha:= lubridate::ymd(paste(ano,mes,dia, sep = "-"))]
des80[, fecha1:= NULL]
# use (\\d+)[\] to match digits followed by \;
# specify .* before and after the pattern so that strings other than the pattern can be removed;
# use ? on the first .* to make the match unready so that all the three digits will be kept;
# use \\1 to refer the capture group (\\d+);


# Limpieza 2004
des04 <- des04[complete.cases(des04)] # delete row by reference don't implemented yet

des04[, ano:= as.numeric(sub(".*?[/](\\d+).*", "\\1", fecha))][
    ano <10, ano := as.numeric(paste0(200,ano))][
    ano <20, ano := as.numeric(paste0(20,ano))]
des04 <- des04[complete.cases(des04)]

des04[, fecha:= sub(".*?^(Enero|Abril|Julio|Octubre).*", "\\1", fecha)]
des04 <- des04[-grep(pattern = "-", x = fecha)]

meses <- c("Enero", "Abril", "Julio", "Octubre")
for(m in meses) {
    if (m == "Enero"){
        des04[fecha==m, mes:=01]
    } else if (m == "Abril") {
        des04[fecha==m, mes:=04]
    } else if (m == "Julio") {
        des04[fecha==m, mes:=07]
    } else {
        des04[fecha==m, mes:=10]}
}
des04[, dia:=01]
des04[, fecha:= lubridate::ymd(paste(ano,mes,dia, sep = "-"))]

# Join ambas tablas

str(des80)
str(des04)
des80[, mes := as.numeric(mes)]
df <- data.table::rbindlist(list(des04, des80), use.names = TRUE)
setkeyv(df, "fecha")
df <- df[!duplicated(fecha)]

# Imputación tasa de actividad
library(imputeTS)
ta <- ts(data = df[, ta], start = c(1981,1), frequency = 4)
imputeTS::plotNA.distribution(ta)
statsNA(ta)

impute_interpolation <- vector()
impute_interpolation$spline <- na.interpolation(ta, option = "spline")
impute_interpolation$lineal <- na.interpolation(ta, option = "linear")
impute_interpolation$stine <-  na.interpolation(ta, option = "stine")
impute_kalman_arima <- na.kalman(ta, model = "auto.arima")
impute_kalman_struc <- na.kalman(ta, model = "StructTS", smooth = TRUE)

par(mfrow = c(2,1))
plotNA.imputations(ta, impute_kalman_arima)
par(new = T)
plotNA.imputations(ta, impute_kalman_struc)
par(new = T)
plotNA.imputations(ta, impute_interpolation$lineal)
plotNA.imputations(ta, impute_interpolation$spline)
plotNA.imputations(ta, impute_interpolation$stine)

# Me tomo el promedio de los 5 tipos de imputaciones
extrac_imp <- function(x) {
    window(x, start = c(1982,1), end = c(1982,2))
}
est1 <- extrac_imp(impute_kalman_arima)
est2 <- extrac_imp(impute_kalman_struc)
est3 <- extrac_imp(impute_interpolation$lineal)
est4 <- extrac_imp(impute_interpolation$spline)
est5 <- extrac_imp(impute_interpolation$stine)

estimaciones <- matrix(c(est1, est2, est3, est4, est5), ncol = 2, nrow = 5, byrow = TRUE) %>% 
                    apply(., MARGIN = 2, mean)
df[is.na(ta), ta := estimaciones]

saveRDS(df, here::here("Datos", "Finales", "Tasas-Montevideo.rds"))




# Extra - agregado después ------------------------------------------------
# Solo quiero datos para 95-2001 para agregar como xreg en avisos.
librerias <- c("data.table", "magrittr")
sapply(librerias, require, character.only = TRUE)

directorio <- here::here("Datos", "Originales")
des80 <- paste(directorio, "Desempleo1980-2005.xls", sep = "/")
des80_original <- readxl::read_excel(des80, skip = 12, col_names = c("fecha1", "borrar1", "fecha2", "ta", "borrar2", "borrar3", 
                                                                     "te", "borrar4", "borrar5", "td", "borrar6", "borrar7"))
des80 <- as.data.table(des80_original)

# View(des80)
des80 <- des80[complete.cases(des80),]
des80[, grep("^borrar", colnames(des80)) := NULL]
des80[, fecha2 := NULL]
des80[, fecha := gsub(fecha1, pattern = "(.*)\\s?/\\s?(\\d*)$", replacement = "\\1-\\2") %>% 
          gsub(., pattern = "-0(\\d)+", replacement = "-200\\1") %>% 
          gsub(., pattern = "-([98])(\\d)+", replacement = "-19\\1\\2") %>% 
          paste(., "01", sep = "-") %>% 
          as.Date(., format = "%B-%Y-%d")
      ][, fecha1 := NULL]
des80 <- des80[fecha > "1985-01-01",]
names_character <- c("ta", "te", "td")
for(col in names_character) {
    set(des80, j = col, value = as.numeric(des80[[col]]))
}
des80
