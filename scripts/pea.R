# Construcción PEA de Montevideo --------------------------------------------------------
paquetes <- c("data.table", "magrittr", "tempdisagg")
sapply(paquetes, require, character.only = TRUE)

# Nota del INE:
# IMPORTANTE:     A partir de 1998 los datos individuales deberán ponderarse con los pesos que figuran 
# en las bases (pesotri, pesosem, pesoan) atendiendo a si los cálculos se hacen sobre un trimestre, 
# un semestre o anuales.
# Para los cálculos del total país año 1997, se ponderan los datos de Montevideo e 
# interior con los factores 0,469 y 0,531 respectivamente.

# sampling weights A sampling weight of 1000 means that the observation represents
# 1000 individuals in the population

# Carga de datos ECH, censos, proyecciones poblaciones
dt <- readRDS(here::here("Datos","Intermedias","sub_ech_80_18.rds"))

# Muestra autoponderada: PobTotal/MuestraTotal = ponderadores


# Tasas -------------------------------------------------------------------
# Tasas de actividad, empleo y desempleo.
tasas <- readRDS(here::here("Datos", "Finales", "Tasas-Montevideo.rds"))
tasas[, plot(x = fecha, y = ta, type = "l")]
tasas[, plot(x = fecha, y = te, type = "l")]
tasas[, plot(x = fecha, y = td, type = "l")]
# Tasa vacantes, PEA y PET urrestarazu.
tasas_urr <- readxl::read_excel(here::here("Datos", "Originales", "vacantes-1981-1995.xlsx")) %>% 
                as.data.table(.)
pea_urr <- readRDS(here::here("Datos", "Finales", "pea-urrest-pred.rds"))


# CENSOS ------------------------------------------------------------------
# URUGUAY: Censos desde 1852 hasta 2011
censo <- readxl::read_excel(here::here("Datos", "Censos_1852_2011.xls"), range = "A4:J11") %>% as.data.table(.)
censo <- censo[complete.cases(censo),] 
setnames(censo, old = names(censo), new = gsub(names(censo), pattern = " (1)", replacement = "", fixed = TRUE))
censo <- data.table::melt.data.table(censo, id.vars = "Departamento", variable.name = "año", value.name = "pob")
censo[Departamento == "Montevideo", ]


# Proy.Uy.1998 de URUGUAY rev 1998 ------------------------------
# URUGUAY 1950-2025: Proyecciones de población, revisión 1998.
# pob_proy_98 <- readxl::read_excel(here::here("Datos", "proyecciones_revision_1998", "proyecciones1950-2050_rev_1998.xls"),
#                                   sheet = "3. Población", range = "A8:BY110",
#                                   col_names = c("edad",seq.int(1950, 2025, 1))) %>% as.data.table(.)
# pob_proy_98 <-  pob_proy_98[!grep(edad, pattern = "-"), ]
# pob_proy_98[grep(edad, pattern = "[A-Z|a-z]"), edad := 85] # 85 = 85+
# pob_proy_98[, edad := as.integer(edad)]
# pob_proy_98 <- pob_proy_98[edad >= 14,] # Solo mayores iguales a 14, PET
# pob_proy_98 <- melt(pob_proy_98, id.vars = "edad", variable.name = "ano", value.name = "pob")
# pob_proy_98[, sum(pob), by = ano]
    
# # Trimestralizar la serie del 98, método de Denton-Cholette
# pob_proy_98_tsa <- ts() 


# Proy.Uy.2013 URUGUAY rev 2013 ---------------------------------------------
# URUGUAY 1996-2050: Población estimada y proyectada revisión 2013
pob_uy <- readxl::read_excel(here::here("Datos", "1.2_Poblacion_e_indicadores.xls"), range = "A5:B61", sheet = 2, 
                             col_names = c("ano", "pob_total")) %>%
            as.data.table(.)
pob_uy <- pob_uy[complete.cases(pob_uy),]
pob_uy[between(ano, 1997, 2020), plot(x = ano, y = pob_total, type = "l")]
# Caída de población por la crisis y posterior aumento moderado y luego explosivo.
pob_uy[, pob_total]
pob_uy[, shift(pob_total, type = "lead")]
pob_uy[, .(delta = (shift(pob_total, type = "lead") - pob_total),
           fecha = shift(ano, type = "lead"))][, plot(fecha, delta, type = "l")]
pob_uy[, .(delta = (shift(pob_total, type = "lead") - pob_total)/pob_total*100,
           fecha = shift(ano, type = "lead"))][, plot(x = fecha, y = delta, type = "l")]
# Nota: Originalmente le puse pob_uy

# MONTEVIDEO 1995-2025 Proyecciones 2013. (Edad desde 15 en adelante)
# Esto ya sería la PET de montevideo
pob_uy_mdeo_1 <- readxl::read_excel(here::here("Datos", "Departamentos_poblacion_por_sexo_y_edad_1996-2025.xls"), sheet = "Montevideo",
                                  range = "A10:AE28", col_names = c("edad",as.character(seq.int(1996, 2025)))) %>% 
                                  as.data.table(.)
mdeo_proy13 <- data.table::melt(pob_uy_mdeo_1, variable.name = "ano", value.name = "pob", 
                                measure.vars = names(pob_uy_mdeo_1)[names(pob_uy_mdeo_1) != "edad"], 
                                variable.factor = FALSE)
mdeo_proy13[, ano := as.numeric(ano)]
mdeo_proy13 <- mdeo_proy13[, .(pob = sum(pob)), by = ano]
mdeo_proy13[, plot(pob, x = ano, type = "l")]

mdeo_proy13[data.table::melt(pob_uy_mdeo_1[!edad %in% c("0-4", "5-9", "10-14"), -"edad"], 
                             variable.name = "ano", value.name = "pob", 
                             measure.vars = names(pob_uy_mdeo_1)[names(pob_uy_mdeo_1) != "edad"], variable.factor = FALSE
                            )[, 
                              .(pob = sum(pob)), 
                              by = ano
                            ][, ano := as.integer(ano)], on = "ano", pet := i.pob]
mdeo_proy13[, {
  par(mfrow = c(2,1), mai = c(0.5, 0.5, 0.1, 0.1))
  plot(x = ano, y = pob, type = "l", xlab = "")
  plot(x = ano, y = pet, type = "l", xlab = "")
  }]
# Trimestralizar la serie
temp1 <- ts(mdeo_proy13[between(ano, 1996, 2018), pet], start = c(1996, 1), frequency = 1)
temp2 <- ts(pet_ech_impute[, pet], start = c(1996, 1), frequency = 4)
plot(temp1)
plot(temp2)
tempdisagg::td(formula = temp1 ~ 0 + temp2, 
               method = "denton-cholette", conversion = "average") %>% predict() %>% plot


# Proy.Mdeo.2005 MONTEVIDEO rev 2005 ------------------------------------------
# MONTEVIDEO 1996-2025: Proyecciones de población con revisión al año 2005, Años 1996-2025
pob_proy_2005 <- readxl::read_excel(here::here("Datos", "proyecciones_revision_2005", "Totpais_deptos_edad_simple_1996-2025.xls"), sheet = "Montevideo", 
                                    range = "A10:AE105", col_names = c("edad", seq.int(1996,2025)), 
                                    col_types = rep("numeric", 31)) %>% as.data.table(.) # Se modifico excel en vez de 95+ se puso 95
# Solo los mayores o iguales a 14 años: PET o a 15 años.
# pob_proy_2005 <-  pob_proy_2005[edad >= 14,]
pob_proy_2005 <- melt(pob_proy_2005, id.vars = "edad", variable.name = "ano", value.name = "pob", 
                      variable.factor = FALSE)
pob_proy_2005[, ano := as.integer(ano)]
pob_proy_2005[, .(pob = sum(pob)), by = ano]

# Trimestralizar la serie usando el método de Denton-Cholette
pob_proy_2005_ts_a <- ts(pob_proy_2005[, 
                                       .(pob = sum(pob)), 
                                       by = ano
                                       ][, pob], start = 1996, frequency = 1)
pob_proy_2005_ts_q <- predict(tempdisagg::td(pob_proy_2005_ts_a ~ 1, method = "denton-cholette", conversion = "average"))
plot(pob_proy_2005_ts_q)
plot(pob_proy_2005_ts_a)

# Calculado POB y PET para MONTEVIDEO en base proyecciones 96-2018
# Cuando entreguen los datos revisión 2013 con apertura por edad simple se hace con ellos
# Luego hay que trimestralizar

# Población MONTEVIDEO anual 1996-2018 Montevideo en base estimaciones-proyecciones INE
pob_ano_proy2005 <- pob_proy_2005[ano <= 2018, .(pob = sum(pob)), keyby = ano]

# PET MONTEVIDEO anual 1996-2018 Montevideo en base estimaciones-proyecciones INE
pet_ano_proy2005 <- pob_proy_2005[edad >= 14, .(pet = sum(pob)), keyby = ano]
pet_ano_proy2005[, plot(x = ano, y = pet, type = "l")]
# Trimestralizada
pet_ano_proy2005_ts <- ts(pet_ano_proy2005[between(ano, 1996, 2018), pet], start = 1996, frequency = 1)
pet_ano_proy2005_ts_q <- predict(tempdisagg::td(pet_ano_proy2005_ts ~ 1, method = "denton-cholette", conversion = "average"))
plot(pet_ano_proy2005_ts_q)
predict(tempdisagg::td(pet_ano_proy2005[between(ano, 1996, 2018), pet] ~ 1, 
                       method = "denton-cholette", conversion = "average", to = 4))

# POB MONTEVIDEO en base ECH y comparación con proyección
lista = list()
temp = dt[bc_anio >= 1998 & !is.na(bc_pesoan) & bc_dpto == "montevideo", .(bc_pesoan, bc_anio), keyby = bc_anio]
for(i in 1:100) {
    lista[[i]] <- temp[, sum(sample(x = bc_pesoan, size = .N, replace = TRUE)), by = bc_anio]
}
  rm(temp)
# dt[bc_anio >= 1998, sum(sample(x = bc_pesoan, size = .N, replace = TRUE), na.rm = TRUE), keyby = bc_anio]

prueba <- rbindlist(lista)
prueba[, .(pob = mean(V1, na.rm = TRUE)), by = bc_anio]
dt[bc_dpto == "montevideo" & bc_anio >= 1998, .(pob = sum(bc_pesoan, na.rm = TRUE)), keyby = bc_anio]
comparacion <- cbind(prueba[, .(pob_ech_s = mean(V1, na.rm = TRUE)), by = bc_anio],
      dt[bc_dpto == "montevideo" & bc_anio >= 1998, .(pob_ech = sum(bc_pesoan, na.rm = TRUE)), keyby = bc_anio],
      pob_proy_2005[between(ano, 1998, 2018), .(pob_proy = sum(pob)), by = ano])
comparacion
comparacion[, {
  y = c(min(pob_ech, pob_proy), max(pob_ech, pob_proy))
  x = c(1998, 2018)
  plot(x = bc_anio, y = pob_ech, type = "l", xlim = x, ylim = y)
  par(new = TRUE)
  plot(pob_proy, x = bc_anio, col = "red", xlim = x, ylim = y, new = TRUE)
  }]
comparacion[, matplot(x = bc_anio, y = .SD, type = "l"), .SDcols = c("pob_ech", "pob_proy")]

# Comparación incluyendo las proyecciones de 2015 y 2013 para Uruguay
par(mfrow = c(2, 1))
mdeo_proy13[between(ano, 1998, 2018), plot(x = ano, y = pet, type = "l")]
comparacion[, matplot(x = bc_anio, y = .SD, type = "l"), .SDcols = c("pob_ech", "pob_proy")]
# Las proyecciones de 2005 para Montevideo son con caídas de la población, pero las proyecciones de 2013
# para Mdeo son al alza. Obviamente trabajo con las de 2013. Estas últimas son consistentes con la ECH. (tendencialmente)




# ECH: ECH restringida MONTEVIDEO ----------------------------------------------------------
# Población MDEO trimestral 1998-2018 en base a ECH y ponderadores trimestrales
pob_trim_ech <- dt[bc_dpto == "montevideo" & bc_anio > 1997, .(pob = sum(pesotri, na.rm = TRUE), fecha = min(unique(fecha))), keyby = .(bc_anio, trim)]
pob_trim_ech[, plot(x = fecha, y = pob, type = "l")]

# PET MDEO trimestral 1998-2018 en base a ECH y ponderadores trimestrales
pet_trim_ech <- dt[bc_dpto == "montevideo" & bc_anio > 1997 & bc_pe3 >= 14, 
                   .(pet = sum(pesotri, na.rm = TRUE)), 
                   keyby = .(bc_anio, trim)]
dt[is.na(pesotri) & bc_anio >= 1998, .N, by = bc_anio]

# PEA MDEO en base ECH a partir de: TA = PEA/PET*100 desde 1998.1 a 2018.4
pea_ech = (tasas[fecha > as.Date("1997-10-01") & fecha < as.Date("2019-01-01"), "ta"] * pet_trim_ech[, "pet"])/100
pea_ech[,fecha := seq.Date(from = as.Date("1998-01-01"), to = as.Date("2018-10-01"), by = "quarter")]
setnames(pea_ech, old = "ta", new = "pea")

# Union con PEA urrestarazu (que incluye predicción hasta 1998)
# Eso se tiene que mejorar generando PEA 1996-2018 cosa de predecir solamente 1 año en urrestarazu.
ponderador <- pea_ech[year(fecha) %in% 1998, pea]/(pea_urr[year(fecha) %in% 1998, pea])
pea_urr[year(fecha) %in% 1998,]
pea_urr[, pea_corr := pea*c(ponderador[3:4],rep(ponderador, 68/4))]
# Ahora las uno
pea_union <- rbind(pea_urr[!year(fecha) %in% 1998, .(pea = pea_corr, fecha = fecha)], pea_ech)
pea_union[pea == 0, ]

# Tengo que imputar los 0 como NA y calcular esos valores.
pea_union[pea == 0, pea := NA_real_]
# Imputación con imputeTS
library(imputeTS)
pea_ts <- ts(data = pea_union[, pea], start = c(1981,3), frequency = 4)
imputeTS::plotNA.distribution(pea_ts)
imputeTS::statsNA(pea_ts)

impute_interpolation <- vector()
impute_interpolation$spline <- imputeTS::na_interpolation(pea_ts, option = "spline")
impute_interpolation$lineal <- imputeTS::na_interpolation(pea_ts, option = "linear")
impute_interpolation$stine  <- imputeTS::na_interpolation(pea_ts, option = "stine")
impute_kalman_arima         <- imputeTS::na_kalman(pea_ts, model = "auto.arima")
impute_kalman_struc         <- imputeTS::na_kalman(pea_ts, model = "StructTS", smooth = TRUE)

par(mfrow = c(2,1))
plotNA.imputations(pea_ts, impute_kalman_arima)
par(new = T)
plotNA.imputations(pea_ts, impute_kalman_struc)
par(new = T)
plotNA.imputations(pea_ts, impute_interpolation$lineal)
plotNA.imputations(pea_ts, impute_interpolation$spline)
plotNA.imputations(pea_ts, impute_interpolation$stine)
par(mfrow = c(1,1))

# Me tomo el promedio de los 5 tipos de imputaciones
extrac_imp <- function(x) {
    window(x, start = c(2012,1), end = c(2012,4))
}
est1 <- extrac_imp(impute_kalman_arima)
est2 <- extrac_imp(impute_kalman_struc)
est3 <- extrac_imp(impute_interpolation$lineal)
est4 <- extrac_imp(impute_interpolation$spline)
est5 <- extrac_imp(impute_interpolation$stine)

estimaciones <- matrix(c(est1, est2, est3, est4, est5), ncol = 4, nrow = 5, byrow = TRUE) %>% 
    apply(., MARGIN = 2, mean)
pea_union[is.na(pea), pea := estimaciones]
pea_union[, plot(x = fecha, y = pea, type = "l")]
# Este salto brusco viene explicado por la tasa de actividad?
tasas[, plot(x = fecha, y = ta, type = "l")]
pet_trim_ech[trim == 1, fecha := paste(bc_anio, 1, 01, sep = "-")
             ][trim == 2, fecha := paste(bc_anio, 4, 01, sep = "-")
               ][trim == 3, fecha := paste(bc_anio, 7, 01, sep = "-")
                 ][trim == 4, fecha := paste(bc_anio, 10, 01, sep = "-")
                   ][, fecha := as.Date(fecha)]
pet_trim_ech[, plot(x = fecha, y = pet, type = "l")]
pet_trim_ech[between(bc_anio, 2000, 2011), plot(x = fecha, y = pet, type = "l")]
pet_trim_ech[between(bc_anio, 2009, 2011), plot(x = fecha, y = pet, type = "l")]
pet_trim_ech[pet != 0, plot(x = fecha, y = pet, type = "l")]
# El salto en la PEA viene explicado por un salto en la PET calculado a partir de la ech!
# Esto lo podría arreglar al trimestralizar la PET que surge de las proyecciones-estimaciones del INE
# a nivel anual desde 1996 en adelante.

# Imputo la PET también (hacer una función para eso)
pet_trim_ech[bc_anio == 2012, pet := NA]
temp <- ts(data = pet_trim_ech[, pet], start = c(1998,1), frequency = 4)
imputeTS::plotNA.distribution(temp)
imputeTS::statsNA(temp)

impute_interpolation <- vector()
impute_interpolation$spline <- imputeTS::na_interpolation(temp, option = "spline")
impute_interpolation$lineal <- imputeTS::na_interpolation(temp, option = "linear")
impute_interpolation$stine  <- imputeTS::na_interpolation(temp, option = "stine")
impute_kalman_arima         <- imputeTS::na_kalman(temp, model = "auto.arima")
impute_kalman_struc         <- imputeTS::na_kalman(temp, model = "StructTS", smooth = TRUE)

est1 <- extrac_imp(impute_kalman_arima)
est2 <- extrac_imp(impute_kalman_struc)
est3 <- extrac_imp(impute_interpolation$lineal)
est4 <- extrac_imp(impute_interpolation$spline)
est5 <- extrac_imp(impute_interpolation$stine)

estimaciones <- matrix(c(est1, est2, est3, est4, est5), ncol = 4, nrow = 5, byrow = TRUE) %>% 
  apply(., MARGIN = 2, mean)
pet_trim_ech[bc_anio == 2012, pet := estimaciones]
pet_trim_ech[, plot(x = fecha, y = pet, type = "l")]

# Genero un vector desde 1996 hasta 2018 de la pet, con NA para 96 y 97 y luego los imputo.
temp <- ts(c(rep(NA, 8), pet_trim_ech[, pet]), start = c(1996, 1), frequency = 4)
imputeTS::plotNA.distribution(temp)
imputeTS::statsNA(temp)

impute_interpolation <- vector()
impute_interpolation$spline <- imputeTS::na_interpolation(temp, option = "spline")
impute_interpolation$lineal <- imputeTS::na_interpolation(temp, option = "linear") # No sirve todos son iguales
impute_interpolation$stine  <- imputeTS::na_interpolation(temp, option = "stine")  # No sirve todos son iguales
impute_kalman_arima         <- imputeTS::na_kalman(temp, model = "auto.arima")
impute_kalman_struc         <- imputeTS::na_kalman(temp, model = "StructTS", smooth = TRUE)
impute_kalman_struc_f         <- imputeTS::na_kalman(temp, model = "StructTS", smooth = FALSE)

library(forecast)
autoplot(impute_interpolation$spline) +
  autolayer(impute_kalman_arima) +
  autolayer(impute_kalman_struc) +
  autolayer(impute_interpolation$lineal) +
  autolayer(impute_interpolation$stine) +
  autolayer(impute_kalman_struc_f)
# La estimación arima no tiene sentido.

# Me tomo el promedio de los 5 tipos de imputaciones
extrac_imp <- function(x) {
  window(x, start = c(1996,1), end = c(1997,4))
}
# est1 <- extrac_imp(impute_kalman_arima)
est2 <- extrac_imp(impute_kalman_struc)
est4 <- extrac_imp(impute_interpolation$spline)

estimaciones97 <- matrix(c(est2[5:8], est4[5:8]), ncol = 4, nrow = 2, byrow = TRUE) %>% 
  apply(., MARGIN = 2, mean)
estimaciones96 <- matrix(c(est2[1:4], est4[1:4]), ncol = 4, nrow = 2, byrow = TRUE) %>% 
  apply(., MARGIN = 2, mean)
pet_ech_impute <- data.table(pet = c(estimaciones96, estimaciones97, pet_trim_ech[, pet]), 
                             fecha = seq.Date(from = as.Date("1996/01/01"), to = as.Date("2018/10/01"), by = "quarter"))
pet_ech_impute[, plot(y = pet, x = fecha, type = "l")]

# Lo que debería terminar de hacer acá:
# Trabajar con la PET calculada a partir de las proyecciones del INE y trimestralizarlas. Ver si puedo usar otra
# serie como indicador para trimestralizar, ejemplo la TA o la PET ó PEA de la ECH.


# Proy.Uy 2013 Mdeo (sigue) -----------------------------------------------

# Trimestralizar la serie
temp1 <- ts(mdeo_proy13[between(ano, 1996, 2018), pet], start = c(1996, 1), frequency = 1)
temp2 <- ts(pet_ech_impute[, pet], start = c(1996, 1), frequency = 4)
plot(temp1)
plot(temp2)
mdeo_proy_q <- data.table(
                pet = as.integer(predict(tempdisagg::td(formula = temp1 ~ 0 + temp2, 
                      method = "denton-cholette", conversion = "average"))),
                fecha = seq.Date(as.Date("1996/01/01"), as.Date("2018/10/01"), "quarter"))
# Calculo la PEA usando TA = PEA/PET*100
mdeo_proy_q[, pea := tasas[between(year(fecha), 1996, 2018), ta]*pet/100]
mdeo_proy_q[, plot(pea, x = fecha, type = "l")]
mdeo_proy_q[, sapply(.SD, class)]

# Unión de PEA urrestarazu y la recién calculada. Bajo un ponderador
ponderador <- mdeo_proy_q[year(fecha) %in% 1996, pea]/(pea_urr[year(fecha) %in% 1996, pea])
pea_urr[year(fecha) %in% 1996, ]
pea_urr[, pea_corregida := pea*c(ponderador[3:4], rep(ponderador, 68/4))]
pea_urr[, matplot(x = fecha, y = .SD, type = "l", ), .SDcols = c("pea", "pea_corregida")]
pea_urr[, {
  y = c(min(pea, pea_corregida), max(pea, pea_corregida))
  plot(x = fecha, y = pea, type = "l", ylim = y)
  par(new = TRUE)
  plot(x = fecha, y = pea_corregida, ylim = y, type = "l")
}]
# Notar que altera la serie de urrestarazu, no solo en nivel sino en la forma, aunque no la tendencia, seria 
# interesante probar encadenando hacia atras
pea_union <- rbind(pea_urr[!year(fecha) %in% c(1996:1998), .(pea_corregida, fecha = fecha)], mdeo_proy_q[, .(pea_corregida = pea, fecha)])
pea_union[, plot(pea_corregida, x = fecha, type = "l")]

# Voy a imputar la PEA hacia atrás, para obtener los valores hasta 1980.
temp <- ts(c(rep(NA, 6), pea_union[, pea_corregida]), start = c(1980, 1), frequency = 4)
imputeTS::plotNA.distribution(temp)
imputeTS::statsNA(temp)

impute_interpolation <- vector()
impute_interpolation$spline <- imputeTS::na_interpolation(temp, option = "spline")
impute_kalman_arima         <- imputeTS::na_kalman(temp,   model = "auto.arima", stepwise = FALSE, approximation = FALSE)
impute_kalman_struc         <- imputeTS::na_kalman(temp,   model = "StructTS", smooth = TRUE)
impute_kalman_struc_f         <- imputeTS::na_kalman(temp, model = "StructTS", smooth = FALSE)

# Me tomo el promedio de los 3 tipos de imputaciones
extrac_imp <- function(x) {
  window(x, start = c(1980,1), end = c(1981,2))
}
est1 <- extrac_imp(impute_kalman_arima)
est2 <- extrac_imp(impute_kalman_struc)
est4 <- extrac_imp(impute_interpolation$spline)
est5 <- extrac_imp(impute_kalman_struc_f)

autoplot(temp) +
  autolayer(est1) +
  autolayer(est2) +
  autolayer(est4) +
  autolayer(est5)
# Los splines son un desastre.
# La estimación arima es creciente.
# La estimación por kalman es suave

estimaciones81 <- matrix(c(est1[5:6], est2[5:6]), ncol = 2, nrow = 2, byrow = TRUE) %>% 
  apply(., MARGIN = 2, mean, na.rm = TRUE)
estimaciones80 <- matrix(c(est1[1:4], est2[1:4]), ncol = 4, nrow = 2, byrow = TRUE) %>% 
  apply(., MARGIN = 2, mean, na.rm = TRUE)
pea_union_impute <- data.table(pea = c(estimaciones80, estimaciones81, pea_union[, pea_corregida]), 
                             fecha = seq.Date(from = as.Date("1980/01/01"), to = as.Date("2018/10/01"), by = "quarter"))
pea_union_impute[, plot(y = pea, x = fecha, type = "l")]

# Extra: Obtener los avisos de urrestarazu ahora que tengo la PEA de 1980 (la base)
# para eso tengo que imputar las vacantes de 1980 y 1981(1,2)
temp <- ts(c(rep(NA, 6), tasas_urr[, vacantes]), start = c(1980, 1), frequency = 4)
imputeTS::plotNA.distribution(temp)
imputeTS::statsNA(temp)

impute_interpolation <- vector()
impute_interpolation$spline <- imputeTS::na_interpolation(temp, option = "spline")
impute_kalman_arima         <- imputeTS::na_kalman(temp, model = "auto.arima")
impute_kalman_struc         <- imputeTS::na_kalman(temp, model = "StructTS", smooth = TRUE)

# Me tomo el promedio de los 3 tipos de imputaciones
extrac_imp <- function(x) {
  window(x, start = c(1980,1), end = c(1981,2))
}
est1 <- extrac_imp(impute_kalman_arima)
est2 <- extrac_imp(impute_kalman_struc)
est4 <- extrac_imp(impute_interpolation$spline)

estimaciones81 <- matrix(c(est1[5:6], est2[5:6], est4[5:6]), ncol = 2, nrow = 3, byrow = TRUE) %>% 
  apply(., MARGIN = 2, mean, na.rm = TRUE)
estimaciones80 <- matrix(c(est1[1:4], est2[1:4], est4[1:4]), ncol = 4, nrow = 3, byrow = TRUE) %>% 
  apply(., MARGIN = 2, mean, na.rm = TRUE)

largo = NROW(pea_union_impute) - length(c(estimaciones80, estimaciones81, tasas_urr[, vacantes]))
pea_union_impute[, vacantes := c(estimaciones80, estimaciones81, tasas_urr[, vacantes], rep(NA, largo))
                 ][, avisos := vacantes*100*(pea_union_impute[year(fecha) == 1980, pea]/pea)]
pea_union_impute[between(year(fecha), 1980, 1995), {
  par(mfrow = c(2,1))
  plot(x = fecha, y = vacantes, type = "l", )
  plot(x = fecha, y = avisos, type = "l")
}]
# ESTA SERIE DE AVISOS ESTA MAL CALCULADA!!!!!!!!!!

# Guardo PEA
saveRDS(pea_union_impute[, .(pea, fecha)], file = here::here("Datos", "Finales", "estimacionPEA.rds"), compress = FALSE)
