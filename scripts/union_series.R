libs=c("readxl", "ggplot2", "ggfortify", "dplyr", "plotrix", "lubridate", "inspectdf", "magrittr", "plotly", "data.table")
lib_nuevas <- libs[!(libs %in% installed.packages()[,"Package"])] 
if(length(lib_nuevas)) install.packages(lib_nuevas)
load_libs <- function(libraries = libs) for (lib in libraries)
    require(lib, character.only=T)
load_libs(libs)
theme_set(theme_bw())


# En este script se trabaja con las series de:
    # Urrestarazu 81-95 Trimestral              dtu
    # Molina      96-98 Semanal                 dtm
    # Molina    Agos-98 Semanal                 dta
    # Ceres       98-14 Mensual                 dtc
    # Iecon       00-09 sin frecuencia fija     dti
    # ElPais      13-18 Frecuencia semanal      dte

# Se van a unir las serie de Urrestarazu, Molina, Ceres y ElPais para lograr vacantes desde 1981 hasta 2018.
# Iecon solamente se utiliza para analizar el movimiento y correlación con la serie de ceres en el periodo que coinciden
# La serie de Agosto-98 se usa porque es el año base del índice de ceres y puede usarse para comparar la cantidad de avisos.

repo_o <- here::here("Datos", "Originales")
repo_i <- here::here("Datos", "Intermedias")
repo_f <- here::here("Datos", "Finales")

# Datos

dtu <- readxl::read_excel(paste0(repo_o,"/vacantes-1981-1995.xlsx")) %>% as.data.table()
dtm <- readRDS(paste0(repo_i,"/s_mensual95-98.rds"))
dtc <- readxl::read_excel(paste0(repo_o,"/ICDL-1998-2014.xls"), range = readxl::cell_cols("A:B"), sheet = "serie") %>% 
    as.data.table
dti <- haven::read_dta(paste0(repo_o,"/Gallito-2000-2009.dta")) %>% as.data.table()
dte <- readRDS(paste0(repo_i,"/s_mensual13-18.rds"))
dta <- readRDS(paste0(repo_i, "/agosto98.rds"))


# Serie Urrestarazu y Molina ----------------------------------------------

dtu

#  Serie Molina y Ceres ------------------------------------------------------------------

str(dtm)
str(dtc)
dtc[, fecha := as.Date(fecha)]
setkey(dtm, fecha)
setkey(dtc, fecha)
temp <- dtc[dtm, nomatch = 0]
temp[, `:=`(ind1 = avisos/dta$avisos*100,
            ind2 = avisos/dta$avisos_2*100)]
# El valor de la base debería resultar de avisos/vacantes que es:
# vacantes = avisos_t/avisos_to entonces
# vacantes = avisos --> avisos_t*avisos_to/avisos_t = avisos_to


# Suponiendo se desestacionalizo mediante una diferencia estacional delta12.
temp[, dta$avisos/100*vacantes]

tt <- ts(dtm[2:35, avisos], frequency = 12, start = c(1995,9))
decompose(tt) %>% plot



# Serie Ceres y El País ---------------------------------------------------

dtc
dte
