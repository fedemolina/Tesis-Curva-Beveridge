
# lectura dta, limpieza y guardado sub_dt ---------------------------------


paquetes <- c("data.table", "magrittr")
sapply(paquetes, require, character.only = TRUE)
# Lectura de bases ECH compatabilizadas y descomprimidas, en formato dta
# Luego dichos archivos son borrados para ahorrar espacio en disco
# Si se quire repetir simplemente hay que descomprimir el archivo original que contiene todas las bases.

repo  <- here::here("Datos", "ECH", "ech_dta")
files <- list.files(path = repo, pattern = ".dta")
dt <- sapply(files, function(x) {data.table::as.data.table(haven::read_dta(paste(repo,x,sep="/")))}, simplify=FALSE)

# Todas las variables numeric en realidad son integer
numeric_to_integer <- function(dt){
    future.apply::future_lapply(dt, function(x) {
        numeric_cols = names(x)[future.apply::future_sapply(x, is.numeric)]
        invisible(x[, (numeric_cols) := future.apply::future_lapply(.SD, as.integer), .SDcols = numeric_cols])
    })
}
numeric_to_integer(dt)
rds <- gsub(x = names(dt),pattern = ".dta", replacement = ".rds")
j = 0
repo  <- here::here("Datos", "ECH")
for(i in files) {
    j = j + 1
    saveRDS(object = dt[[i]], file = paste(repo, rds[j], sep = "/"), compress = TRUE)
}

# Ahora leo los datos en formato .rds y compilo las bases para calcular la PEA
# repo  <- here::here("Datos", "ECH")
# files <- list.files(path = repo, pattern = ".rds")
# dt <- sapply(files, function(x) {readRDS(paste(repo, x, sep = "/"))}, simplify = FALSE)

#  Que variables me interesa mantener para calcular la PEA y alguna otra variable de interés?
# repo  <- here::here("Datos", "ECH")
# files <- list.files(path = repo, pattern = ".rds", full.names = TRUE)
# dt <- sapply(files, readRDS, simplify=FALSE)
names(dt) <- gsub(files, pattern = ".*/|.dta", replacement = "")
vars <- "bc_correlat,bc_nper,bc_filtloc,bc_pesoan,pesotri,pesosem,bc_mes,bc_anio,bc_dpto,bc_ccz,bc_area,bc_pe2,bc_pe3,bc_pe4,bc_pe5,bc_pobp,bc_pf41,bc_cat2,bc_pf081,bc_pf082,bc_pf40,bc_rama,bc_pf39,bc_tipo_ocup,bc_pf07,bc_pf051,bc_pf052,bc_pf053,bc_pf06,bc_horas,bc_horas_1,bc_pf04,bc_pf21,bc_pf22,bc_pf26,bc_pf34,bc_reg_disse,bc_register,bc_register2,bc_subocupado,bc_subocupado1"
vars <- unlist(strsplit(vars,","))
is_haven <- function(x) {
    if(class(x) == "haven_labelled"){
        return(TRUE)
    } else {
        return(FALSE)
    }
}
cantidad_haven <- function(dt) {
    lapply(dt, function(x) {
        temp <- lapply(x, is_haven) %>% unlist(.) %>% sum
    }) %>% unlist() %>% sum()    
}
haven_to_factor <- function(dt){
    future.apply::future_lapply(dt, function(x) {
        haven_cols = names(x)[future.apply::future_sapply(x, is_haven)]
        x[, (haven_cols) := future.apply::future_lapply(.SD, haven::as_factor), .SDcols = haven_cols]
    })
}
sub_DT <- function(dt, vars) {
    # Copia dt
    DT <- copy(dt)
    # Variables que están en todas las tablas. Asumiendo que el mínimo de variables está en todas las tablas.
    # Si ese no fuera el caso, hay que modificar esta parte y se complejiza un poco.
    z <- length(vars)
    lapply(DT, function(x) {
        temp = vars[vars %in% names(x)]
        if(length(temp) < z) {
            vars <<- temp
            z = length(vars)
        }
    })
    # Del año 98 en adelante existen las variables pesotri y pesosem
    pesos = paste0("p",c(c(98,99),seq.int(0,18)))
    # Borrar todas las variables que no son de interes
    for(i in names(DT)) {
        if(i %in% pesos) {
            temp = names(DT[[i]])
            vars = c(vars, "pesotri", "pesosem")
            temp = temp[!(temp %in% vars)]
            DT[[i]][, (temp) := NULL]
        } else {
            temp = names(DT[[i]])
            temp = temp[!(temp %in% vars)]
            DT[[i]][, (temp) := NULL]
        }
    }
    # Se borran todas las variables no deseadas
    # lapply(DT, function(x){
    #     temp = names(x)
    #     temp = temp[!(temp %in% vars)]
    #     x[, (temp) := NULL]
    # })    
    # Chequear si hay alguna de clase haven y modificarla en caso necesario
    
    # if(cantidad_haven(dt = DT) > 0) {
    #     haven_to_factor(dt = DT)
    # }
    
    # retorna el objeto
    rbindlist(DT, use.names = TRUE, fill = TRUE, idcol = "id_ech")
}
sub_dt <- sub_DT(DT, vars)

# Chequeo 
sub_dt[,.(na = sum(is.na(pesotri)), n = .N), by = id_ech] # Porque hay NA en 2012? Eso es raro.
sub_dt[,.(na = sum(is.na(pesosem)), n = .N), by = id_ech] # 2012, 2016 y 2018. El semestral no me afect
# Revisar en la tabla general
names(dt$p12)[names(dt$p12)=="pesotri"] # No esta dicha variable. Habra que imputar esos valores

# Creo identificador de la persona
sub_dt[, table(bc_anio, useNA = "a")]
sub_dt[is.na(bc_anio),] # Es del año 84
sub_dt[is.na(bc_anio), bc_anio := 1984]
sub_dt[, table(bc_mes, useNA = "a")]
sub_dt[bc_mes == -15, bc_mes := NA_integer_]
sub_dt[, table(bc_pe3, useNA = "a")]
sub_dt[bc_pe3 == -15, bc_pe3 := NA_integer_]
sub_dt[, table(bc_pe4, useNA = "a")]
sub_dt[bc_pe4 == -15 | bc_pe4 == -9, bc_pe4 := NA_integer_]

sub_dt[, id_persona := seq.int(from = 1, to = NROW(sub_dt), by = 1)]

# keys
cols = c("id_ech", "bc_anio", "bc_mes", "bc_correlat", "bc_nper", "id_persona")
setkeyv(x = sub_dt, cols = cols)

# Etiquetas
dpto <- data.table(num = seq.int(1,19), departamento = c('Montevideo',
                                                         'Artigas',
                                                         'Canelones',
                                                         'Cerro Largo',
                                                         'Colonia',
                                                         'Durazno',
                                                         'Flores',
                                                         'Florida',
                                                         'Lavalleja',
                                                         'Maldonado',
                                                         'Paysandú',
                                                         'Río Negro',
                                                         'Rivera',
                                                         'Rocha',
                                                         'Salto',
                                                         'San José',
                                                         'Soriano',
                                                         'Tacuarembó',
                                                         'Treinta y Tres'))
clean_string <- function(x) {
    x %>% 
        tolower(.) %>% 
        gsub(x = ., pattern = " ", "_") %>% 
        gsub(x = ., pattern = ",", "") %>% 
        gsub(x = ., pattern = ".\\_\\-\\_\\.", replacement =  "_") %>% 
        iconv(x = ., to = "ASCII//TRANSLIT")
}
dpto[, departamento := clean_string(departamento)]

# Limpio los 91 que debería ser 19??? NO. Vinieron como 91, o sea esta mal.
sub_dt[bc_dpto == 91, bc_dpto := 19]
sub_dt[, table(bc_dpto, useNA = "always")]

# join
sub_dt <- dpto[sub_dt, on = c("num == bc_dpto")][, `:=`(bc_dpto = departamento,
                                                num = NULL, 
                                                departamento = NULL)]

ocupacion <- data.table::data.table(num = (c(seq.int(1,9),11, -15)), ocup = c("menor_de_14_años",
                                                                                          "ocupados",
                                                                                          "desocupados_busca_trabajo_por_primera_vez",
                                                                                          "desocupados",
                                                                                          "desocupados_seguro_de_desempleo",
                                                                                          "inactivo_tareas_del_hogar",
                                                                                          "inactivo_estudiante",
                                                                                          "inactivo_rentista",
                                                                                          "inactivo_pensionista_y_Jubilado",
                                                                                          "inactivo_otros", 
                                                                                          NA_character_
))

sub_dt <- sub_dt[ocupacion, on = c("bc_pobp == num")][, `:=`(bc_pobp = ocup,
                                                     ocup = NULL
)]

sub_dt[bc_mes == -15, bc_mes := NA]
sub_dt[, fecha := lubridate::ymd(paste(bc_anio, bc_mes, 01, sep = "-"))]

# Trimestre
sub_dt[, trim := lubridate::quarter(fecha, with_year = FALSE)]

# Guardar la subtabla
intermedias <- here::here("Datos", "Intermedias")
saveRDS(object = sub_dt, file = paste0(intermedias,"/sub_ech_80_18.rds"), compress = FALSE)

# Bind de las ECH del 2013 al 2018 para hacer un análisis similar al de Espino, Goinheix y Alvez de 2000 a 2009.
df <- rbindlist(DT[c("p13", "p14", "p15", "p16", "p17", "p18")], fill = TRUE, use.names = TRUE)
# guardar
saveRDS(object = df, file = paste0(here::here("Datos", "Intermedias"), "/ech13-18.rds"))




# Construcción PEA de Montevideo --------------------------------------------------------
paquetes <- c("data.table", "magrittr")
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

# Tasas de actividad, empleo y desempleo.
tasas <- readRDS(here::here("Datos", "Finales", "Tasas-Montevideo.rds"))

# Tasa vacantes, PEA y PET urrestarazu.
tasas_urr <- readxl::read_excel(here::here("Datos", "Originales", "vacantes-1981-1995.xlsx")) %>% as.data.table
pea_urr <- readRDS(here::here("Datos", "Finales", "pea-urrest-pred.rds"))

# URUGUAY: Censos desde 1852 hasta 2011
censo <- readxl::read_excel(here::here("Datos", "Censos_1852_2011.xls"), range = "A4:J11") %>% as.data.table(.)
censo <- censo[complete.cases(censo),] 
setnames(censo, old = names(censo), new = gsub(names(censo), pattern = " (1)", replacement = "", fixed = TRUE))
censo <- data.table::melt.data.table(censo, id.vars = "Departamento", variable.name = "año", value.name = "pob")
censo[Departamento == "Montevideo", ]

# URUGUAY 1950-2025: Proyecciones de población, revisión 1998.
pob_proy_98 <- readxl::read_excel(here::here("Datos", "proyecciones_revision_1998", "proyecciones1950-2050_rev_1998.xls"),
                                  sheet = "3. Población", range = "A8:BY110",
                                  col_names = c("edad",seq.int(1950, 2025, 1))) %>% as.data.table(.)
pob_proy_98 <-  pob_proy_98[!grep(edad, pattern = "-"), ]
pob_proy_98[grep(edad, pattern = "[A-Z|a-z]"), edad := 85] # 85 = 85+
pob_proy_98[, edad := as.integer(edad)]
pob_proy_98 <- pob_proy_98[edad >= 14,] # Solo mayores iguales a 14, PET
pob_proy_98 <- melt(pob_proy_98, id.vars = "edad", variable.name = "ano", value.name = "pob", )
pob_proy_98[, sum(pob), by = ano]


# URUGUAY 1996-2050: Población estimada y proyectada revisión 2013
pob_uy <- readxl::read_excel(here::here("Datos", "1.2_Poblacion_e_indicadores.xls"), range = "A4:B61", sheet = 2) %>% as.data.table(.)
pob_uy <- pob_uy[complete.cases(pob_uy),]

# MONTEVIDEO 1996-2025: Proyecciones de población con revisión al año 2015, Años 1996-2025
pob_proy_2005 <- readxl::read_excel(here::here("Datos", "proyecciones_revision_2005", "Totpais_deptos_edad_simple_1996-2025.xls"), sheet = "Montevideo", 
                                    range = "A10:AE105", col_names = c("edad", seq.int(1996,2025)), 
                                    col_types = rep("numeric", 31)) %>% as.data.table(.) # Se modifico excel en vez de 95+ se puso 95
# Solo los mayores o iguales a 14 años: PET
# pob_proy_2005 <-  pob_proy_2005[edad >= 14,]
pob_proy_2005 <- melt(pob_proy_2005, id.vars = "edad", variable.name = "ano", value.name = "pob", 
                      variable.factor = FALSE)
pob_proy_2005[, ano := as.integer(ano)]
pob_proy_2005[, .(pob = sum(pob)), by = ano]
  # Trimestralizar la serie usando el método de Denton-Cholette
library(tempdisagg)
pob_proy_2005_ts_a <- ts(pob_proy_2005[, .(pob = sum(pob)), by = ano][, pob], start = 1996, frequency = 1)
pob_proy_2005_ts_q <- predict(tempdisagg::td(pob_proy_2005_ts_a ~ 1, method = "denton-cholette", conversion = "average"))
plot(pob_proy_2005_ts_q)
plot(pob_proy_2005_ts_a)

# Calculado POB y PET para MONTEVIDEO en base proyecciones 96-2018
# Cuando entreguen los datos revisión 2013 con apertura por edad simple se hace con ellos
# Luego hay que trimestralizar

# Población MDEO anual 1996-2018 Montevideo en base estimaciones-proyecciones INE
pob_ano_proy2005 <- pob_proy_2005[ano <= 2018, .(pob = sum(pob)), keyby = ano]

# PET MDEO anual 1996-2018 Montevideo en base estimaciones-proyecciones INE
pet_ano_proy2005 <- pob_proy_2005[edad >= 14, .(pet = sum(pob)), keyby = ano]

# POB MONTEVIDEO en base ECH y comparación con proyección
lista = list()
temp = dt[bc_anio >= 1998 & !is.na(bc_pesoan) & bc_dpto == "montevideo", .(bc_pesoan, bc_anio), keyby = bc_anio]
for(i in 1:250) {
    lista[[i]] <- temp[, sum(sample(x = bc_pesoan, size = .N, replace = TRUE)), by = bc_anio]
}
rm(temp)
# dt[bc_anio >= 1998, sum(sample(x = bc_pesoan, size = .N, replace = TRUE), na.rm = TRUE), keyby = bc_anio]

prueba <- rbindlist(lista)
prueba[, mean(V1, na.rm = TRUE), by = bc_anio]
dt[bc_dpto == "montevideo" & bc_anio >= 1998, sum(bc_pesoan, na.rm = TRUE), keyby = bc_anio]
cbind(prueba[, mean(V1), by = bc_anio],
      dt[bc_dpto == "montevideo" & bc_anio >= 1998, sum(bc_pesoan, na.rm = TRUE), keyby = bc_anio],
      pob_proy_2005[between(ano, 1998, 2018), sum(pob), by = ano]) # Son prácticamente iguales las ech, difiere
                                                                   # la proyección del resultado de la ECH


# Población MDEO trimestral 1998-2018 en base a ECH y ponderadores trimestrales
pob_trim_ech <- dt[bc_dpto == "montevideo" & bc_anio > 1997, .(pob = sum(pesotri, na.rm = TRUE), fecha = min(unique(fecha))), keyby = .(bc_anio, trim)]
pob_trim_ech[, plot(x = fecha, y = pob, type = "l")]

# PET MDEO trimestral 1998-2018 en base a ECH y ponderadores trimestrales
pet_trim_ech <- dt[bc_dpto == "montevideo" & bc_anio > 1997 & bc_pe3 >= 14, .(pet = sum(pesotri, na.rm = TRUE)), keyby = .(bc_anio, trim)]
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
pet_trim_ech[pet != 0, plot(x = fecha, y = pet, type = "l")]
    # El salto en la PEA viene explicado por un salto en la PET!
    # Esto lo podría arreglar al trimestralizar la PET que surge de las proyecciones-estimaciones del INE
    # a nivel anual desde 1996 en adelante.


# (Continuación) PET MDEO anual 1996-2018 Montevideo en base estimaciones-proyecciones INE
    # Trimestralizar







# Verificar variables character y factor que estén bien escritas
clean_bc_pobp <- function(x) {
    x %>% 
        as.character(.) %>%
        trimws(.) %>% 
        gsub(x = ., pattern = "Inactivo – Pensionista y Jubilado", "inactivo_pensionista_y_jubilado", fixed = TRUE) %>% 
        gsub(x = ., pattern = "Inactivo – Estudiante", replacement = "Inactivo_Estudiante", fixed = TRUE) %>% 
        gsub(x = ., pattern = "Inactivo – Rentista", replacement = "Inactivo_Rentista", fixed = TRUE) %>% 
        gsub(x = ., pattern = "Inactivo – tareas del hogar", replacement = "Inactivo_tareas_del_hogar", fixed = TRUE) %>% 
        gsub(x = ., pattern = "Inactivo – Otros", replacement = "Inactivo_Otros", fixed = TRUE) %>% 
        gsub(x = ., pattern = " ", replacement = "_") %>%
        gsub(x = ., pattern = ",", replacement = "") %>%
        gsub(x = ., pattern = "___", replacement = "_") %>%
        gsub(x = ., pattern = "__", replacement = "_") %>%
        tolower(.) %>% 
        as.factor(.)
        # iconv(x = ., from = "UTF-8", to = "ASCII//TRANSLIT")
}
# dt[, bc_pobp := clean_bc_pobp(bc_pobp)]

dt[is.na(bc_pesoan), sum(is.na(bc_pesoan)), keyby = bc_anio] # 88 NA en 2007.
dt[is.na(bc_pesoan), ]
dt[, .(pob = sum(bc_pesoan, na.rm = TRUE)), keyby = bc_anio]

# PET
pet <- dt[bc_pe3 >= 14, ]
pet[bc_anio > 1997, .(peso_ano = sum(bc_pesoan, na.rm = TRUE)), keyby = .(bc_anio)]
# Hay un salto demasiado grande del 2005 al 2006. ¿Por qué?
pet[bc_anio > 1997, .(na = sum(is.na(bc_pesoan))), keyby = .(bc_anio)]

# Filtro para obtener la PET de Montevideo
dt_pet <- dt[bc_pe3 >= 14 & bc_dpto == "montevideo", ]

pet_trim <- dt_pet[bc_anio > 1997, .(pea_tri = sum(pesotri, na.rm = TRUE)), keyby = .(bc_anio, trim)]

# Filtro para obtener la PEA de Montevideo
# Ocupados:     - Ocupados plenos
#               - Ocupados con restricciones:   - Informales
#                                               - Sin protección social
#                                               - Subempleados
# Desocupados:  - Desocupados propiamente dichos
#               - Buscan trbajo por primera vez
#               

dt_pea <- dt_pet[(bc_pobp == "ocupados" | bc_pobp == "desocupados_busca_trabajo_por_primera_vez" | 
                 bc_pobp == "desocupados_seguro_de_desempleo" | bc_pobp == "desocupados") & bc_pobp != "menor_de_14_años", ]

pea_trim <- dt_pea[bc_anio > 1997, .(pea_tri = sum(pesotri, na.rm = TRUE)), keyby = .(bc_anio, trim)]
dt_pea[bc_anio > 1997, .(pea = sum(bc_pesoan, na.rm = TRUE)), keyby = bc_anio]
pea_trim[, ano_trim := paste(bc_anio, trim, sep = "_")]
pea_trim_ts <- ts(data = pea_trim[, pea], start = c(1998, 1), frequency = 4)

library(ggfortify)
autoplot(pea_ts)


# Análisis usando paquete survey
library(survey)
dt_s <- survey::svydesign(weights = ~pesotri, data = dt[!is.na(pesotri) & bc_pe3 >= 14 & bc_anio >= 1998, ][, index := 1], ids =~ id_persona)
summary(dt_s)

svymean(~id_persona, dt_s)

svyby(~id_persona + bc_anio, ~bc_dpto, design = dt_s, svymean)
svyby(~pesotri, ~bc_anio + trim, design = dt_s, svytotal)
