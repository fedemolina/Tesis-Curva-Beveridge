libs=c("lubridate", "data.table", "readxl", "ggplot2", "ggfortify", "inspectdf", "magrittr", "plotly", "forecast")
lib_nuevas <- libs[!(libs %in% installed.packages()[,"Package"])] 
if(length(lib_nuevas)) install.packages(lib_nuevas)
load_libs <- function(libraries = libs) for (lib in libraries)
    require(lib, character.only=T)
load_libs(libs)
theme_set(theme_bw())

# En este scrip primero se realiza un breve análisis de la tabla, y se identifican avisos duplicados en base a distintos
# criterior, dicho número es estable.
# Luego se generan series de avisos laborales de frecuencias mensual y trimestral.
# A continuación y aún truco, se analiza en profundidad la información de los gallitos.


# Entrada y salida
data <- here::here("Datos", "Originales", "Gallito-2013-2018.xlsx")
repo_s <- here::here("Datos", "Intermedias")

df <- readxl::read_xlsx(data, na = c("", " ", "-", ".", ".."), col_names = c("fecha", "area", "puesto", "puesto_anunciado", 
                                                                             "responsabilidad", "url", "estudio", "jerarquia",
                                                                             "categoria", "descripcion", "anunciante",
                                                                             "funciones", "programa", "carrera",
                                                                             "ubicacion", "experiencia", "idioma"),
                        skip = 1, col_types = c("date", rep("text", 16))) %>%
            as.data.table()

setkeyv(df, c("fecha", "url"))
# Análisis (breve) de la tabla y generación de la serie. Más adelante.
apply(df, MARGIN = 2, FUN = function(x) length(unique(x)))
sapply(df, function(x) length(unique(x))) # Es más rápido que apply.
lapply(df, FUN = function(x) length(unique(x)))
df[, lapply(.SD, function(x) length(unique(x)))]
# Hay 115675 url no repetidas de las 122090 filas.
df[duplicated(url), .(fecha, url, puesto)][,.N] # Osea 6415 avisos duplicados según la url.
df[duplicated(url, fromLast = FALSE), .(fecha, url, puesto)][,.N] # Osea 6415 avisos duplicados según la url.
df[duplicated(url), .(fecha, url, puesto)][,.N, keyby = .(mes =month(fecha))]
# Notar que el patrón de repetición se mueve entre 400-700 por mes (independiente del año)
df[duplicated(url), .(fecha, url, puesto)][,.N, keyby = .(ano = year(fecha),mes =month(fecha))][
    , .(fecha = as.Date(paste(ano,mes,01, sep = "-")), N)
] %>% ggplot(., aes(x = fecha, y = N)) + 
    geom_line()  +
    labs(title = "Avisos repetidos segun url",
         y = "cantidad de avisos")

# Prueba de avisos duplicados según otras combinaciones, además de la url.
df[duplicated(df[,.(url,puesto)]), .N]                  # 6407
df[duplicated(df[,.(url,puesto,jerarquia)]), .N]        # 6405
df[duplicated(df[,.(url,puesto,jerarquia, area)]), .N]  # 6403
df[duplicated(df[,.(url,puesto,jerarquia, area, puesto_anunciado)]), .N] # 6403

# Tomando en cuenta el detalle de las funciones
df[duplicated(df[,.(url,funciones)]), .N]               # 6395
df[duplicated(df[,.(url,puesto,jerarquia, area, puesto_anunciado, funciones)]), .N] # 6383.
# Por lo tanto se concluye que los avisos repetidos están en torno a 6400, el número es estable.
df[duplicated(df[,.(url,puesto,jerarquia, area, puesto_anunciado, funciones, fecha)]), .N] # 6129
# Si incluso agregamos la fecha el número se mantiene bastante estable. O sea realmente son avisos repetidos, y además
# sabemos que hay 6129 que se repiten incluso en la misma fecha.

# Genero las series a nivel mensual y trimestral, con duplicados y sin duplicados.
mes  <- df[!duplicated(df, by = "url", fromLast = FALSE), 
           .(avisos_s_dup = .N), 
           keyby = .(ano =year(fecha), q = quarter(fecha), mes = month(fecha))]
trim <- df[!duplicated(df, by = "url", fromLast = FALSE), 
           .(avisos_s_dup = .N), 
           keyby = .(ano =year(fecha), q = quarter(fecha))]
mes[, avisos_c_dup := df[, 
           .(avisos_c_dup = .N), 
           keyby = .(ano =year(fecha), q = quarter(fecha), mes = month(fecha))][, avisos_c_dup]
    ]
trim[, avisos_c_dup := df[, 
           .(avisos_c_dup = .N), 
           keyby = .(ano =year(fecha), q = quarter(fecha))][, avisos_c_dup]
    ]
trim[, mes := dplyr::case_when(
    q == 1 ~ 1,
    q == 2 ~ 4,
    q == 3 ~ 7,
    q == 4 ~ 10
)]

trim[, fecha := as.Date(paste(ano, mes, 1, sep = "-"))]
mes[, fecha := as.Date(paste(ano, mes, 1, sep = "-"))]

# Me quedo solamente con los trimestres completos. Se filtra 2013 q2. En el caso mensual, se agrega el mes 6 que esta completo
mes <- mes[fecha >= "2013-06-01",]
trim <- trim[fecha >= "2013-07-01"]

# Agrego los datos que faltan de 2018 extraídos de:
# https://www.gub.uy/ministerio-trabajo-seguridad-social/datos-y-estadisticas/estadisticas/informe-2018-caracterizacion-demanda-laboral-segun-avisos-clasificados
# Es la misma base de datos, solo que yo la tengo hasta septiembre (la unica diferencia es en septiembre, en donde
# el valor que aparece en dicho pdf coincide con el valor de avisos con duplicados pero el resto coincide con el
# valor de avisos sin duplicados).
# Avisos sin duplicar por link:
# Octubre:   1036
# Noviembre: 1053
# Diciembre: 744
# Hay que agregarle la diferencia entre duplicados y no duplicados.
mes[, {
    mean(avisos_c_dup - avisos_s_dup)
}, keyby = .(mes)
][, tail(V1, 3)]

q4_2018 <- data.table(ano = 2018,
                      q   = 4,
                      mes = 10:12,
                      avisos_s_dup = c(1036, 1053, 744),
                      avisos_c_dup = c(1036 + 82, 1053 + 82, 744 + 83),
                      fecha = seq.Date(as.Date("2018-10-01"), as.Date("2018-12-01"), by = "month"))

mes <- rbind(mes, q4_2018)

trim <- rbind(trim, 
            q4_2018[, .(
            ano = 2018,
            q = 4,
            mes = 10,
            avisos_s_dup = sum(avisos_s_dup), 
            avisos_c_dup = sum(avisos_c_dup),
            fecha = as.Date("2018-10-01"))])

# Agrego los avisos de scraping del año 2019.
scraping <- readRDS("./Datos/Finales/AvisosCompatibilizados.rds")

(ga_19 <- scraping[ano == 2019 & pagina == "gallito", 
                   .(avisos = .N,
                     fecha =as.Date(paste(unique(ano), unique(mes), 01, sep = "-")),
                     ano = unique(ano))
                   , keyby = .(mes)])
# Entre mes 8 y 9 casi no se scrapeo, pasar a NA
scraping[pagina == "gallito", table(fecha_scraping)]
ga_19[mes %in% c(8,9), avisos := NA]
# Acá ya están limpiados los duplicados? Revisar
scraping[duplicated(scraping, by = "id"), .N, by = .(ano, mes, pagina)]
# Solo gallito en 2018, porque el id (link) es NA, no se tenía. Así que, OK.
# Los avisos de scraping son SIN duplicados por link
setnames(ga_19, old = names(ga_19), new = gsub(names(ga_19), pattern = "avisos", replacement = "avisos_s_dup"))
ga_19[, q := data.table::quarter(fecha)]

mes <- data.table::rbindlist(list(mes, ga_19), use.names = TRUE, fill = TRUE)

# Imputar los 2 valores de 2019.
mes_ts <- ts(mes$avisos_s_dup, start = c(2013, 6), frequency = 12)

imputeTS::plotNA.distribution(mes_ts)
ga_imp_ts  <- imputeTS::na_kalman(mes_ts, smooth = TRUE, model = "StructTS", type = "BSM", optim.control = list(maxit = 1000))
ga_imp_ts1 <- imputeTS::na_kalman(mes_ts, smooth = FALSE, model = "StructTS", optim.control = list(maxit = 1000))
ga_imp_ts2 <- imputeTS::na_kalman(mes_ts, smooth = FALSE, model = "auto.arima", optim.control = list(maxit = 1000), 
                                  stepwise = F, allowdrift = FALSE, allowmean = TRUE, parallel = TRUE, stationary = F, 
                                  seasonal = TRUE) 
(autoplot(ga_imp_ts) +
        autolayer(ga_imp_ts1) +
        autolayer(ga_imp_ts2)) %>% 
    ggplotly()

mes[, avisos_s_dup := as.vector(ga_imp_ts)]

# Desestacionalización ----------------------------------------------------

ga_ts <- ts(mes[, avisos_c_dup], start = c(2013, 6), frequency = 12)
ga_ts_s_dup <- ts(mes[, avisos_s_dup], start = c(2013, 6), frequency = 12)
plot(ga_ts)
lines(ga_ts_s_dup)

ga_adj       <- seasonal::seas(ga_ts,       transform.function = "none")
ga_adj_s_dup <- seasonal::seas(ga_ts_s_dup, transform.function = "none")
# Sin transformación para ver la cantidad de avisos del estacional
par(mfrow = c(1,2))
plot(ga_adj)
plot(ga_adj_s_dup)
ga_adj$x %>% plot()
ga_adj$series
ga_adj$data
ga_adj$data %>% plot 
ga_adj_s_dup$data %>% plot 

# Comparar los seasonal.
par(mfrwo = c(1,2))
(autoplot(ga_adj[["data"]][,"seasonal"]) +
    autolayer(ga_adj_s_dup[["data"]][, "seasonal"])) %>% 
    ggplotly(.)

# Comparar trend + seasonal
par(mfrwo = c(1,2))
(autoplot(ga_adj[["data"]][,"trend"]) +
        autolayer(ga_adj_s_dup[["data"]][, "trend"]) +
        autolayer(ga_adj_s_dup[["data"]][, "seasonaladj"]) +
        autolayer(ga_adj[["data"]][, "seasonaladj"])) %>% 
    ggplotly(.)


# Esto sirve para ver que no debería agregar la estacionalidad a la serie de CERES, porque
# al modelar la estacionalidad de forma multiplicativa la misma crece/disminuye si la serie crece o disminuye
# Se observa claramente en la desestacionalización, como si bien sigue el mismo patrón la mism disminuye.
ga_adj$data[, "seasonal"]
monthplot(ga_adj)
monthplot(ga_adj_s_dup)
ga_adj[["data"]][, "seasonaladj"]
ga_adj[["data"]][, "adjustfac"]
round(mes[, avisos_s_dup] - ga_adj_s_dup[["data"]][, "seasonaladj"]) == round(ga_adj_s_dup[["data"]][, "adjustfac"])
# Ok. Entonces la serie original - adjustfac me da la serie final desestacionalizada
# Donde el adjustfac = S + I + algo más
# Entonces una aproximación a la serie de avisos de ceres podría ser restarle el promedio mensual del adjustfac.
# Sin embargo, como dije antes esto va a agregar una estacionalidad determinista (si se agrega un promedio mensual
# de las estacionalidades) y con un rango menor al que haya tenido porque el nivel de la serie es menor.
# Una opción más osada y díficil requeríria estimar hacia atrás dicha estacionalidad, en base a la observada
# por parte de los avisos reales de gallito (de la base de datos) y tomar en cuenta el nivel de dicha serie,
# de forma tal que ello se vea reflejado en la estacionalidad.

# Genero los promedios mensuales
c(rep(NA,5),ga_adj[["data"]][, "adjustfac"]) %>% length()
(prom_mensual <- c(rep(NA,5), ga_adj[["data"]][, "adjustfac"]) %>%
    matrix(., ncol = 12, byrow = TRUE) %>% 
    apply(., MARGIN = 2, mean, na.rm = TRUE) %>% 
    data.table(adj_mensual = ., mes = seq.int(1,12,1)))
(prom_mensual2 <- c(rep(NA,5), ga_adj_s_dup[["data"]][, "adjustfac"]) %>%
        matrix(., ncol = 12, byrow = TRUE) %>% 
        apply(., MARGIN = 2, mean, na.rm = TRUE) %>% 
        data.table(adj_mensual = ., mes = seq.int(1,12,1)))


mes[, `:=`(avisos_c_dup_dest = c(as.integer(ga_adj[["data"]][,"seasonaladj"]), rep(NA_real_,12)),
           avisos_s_dup_dest = as.integer(ga_adj_s_dup[["data"]][,"seasonaladj"]))]

trim <- mes[fecha >= "2013-07-01", .(
        avisos_c_dup = sum(avisos_c_dup),
        avisos_c_dup_dest = sum(avisos_c_dup_dest),
        avisos_s_dup = sum(avisos_s_dup),
        avisos_s_dup_dest = sum(avisos_s_dup_dest),
        fecha = min(fecha),
        mes = min(mes)), by = .(ano, q)]

setcolorder(mes, c("fecha", "ano", "q", "mes"))
setcolorder(trim, c("fecha", "ano", "q", "mes"))

# Guardo con fecha, ano, q, mes que si bien se pueden generar todas a partir de fecha, luego se vuelven a utilizar
# Asi no las tengo que calcular de nuevo.
saveRDS(mes,"./Datos/Finales/serie_mensual_ga_13-19.rds", compress = FALSE)
saveRDS(trim, "./Datos/Finales/serie_trimestral_ga_13-19.rds", compress = FALSE)



# EDA. RE-ARMARLO Y PROFUNDIZAR ACÁ HAY MUCHA INFORMACIÓN DE UTILIDAD. ---------------------------------------------------------------------

dplyr::glimpse(df)
totalNA <- apply(df, 2, function(x) {round(sum(is.na(x))/nrow(df)*100, 2)})
totalNA <- totalNA[totalNA<40]     # Hasta un 40% de NA

df <- df[names(totalNA)]

tofactor <- apply(df, MARGIN = 2, FUN = function(x) length(unique(x)))
tofactor <- tofactor[tofactor<30]
df[names(tofactor)] %>% apply(., MARGIN = 2, function(x) {table(x)})
df[names(tofactor)]
df[names(tofactor)] <- lapply(X = df[names(tofactor)], FUN = factor) 
# Otras formas de hacer lo mismo: 
# data %<>% mutate_at(cols, funs(factor(.))) o simplemente: data %<>% mutate_at(cols, factor)
# df %<>% mutate_at(names(tofactor), factor)
# df %>% mutate_if(is.character, as.factor)
apply(df, 2, function(x) {round(sum(is.na(x))/nrow(df)*100, 2)})

df$fecha <- lubridate::as_date(df$fecha)
df$mes <- lubridate::month(df$fecha)
df$ano <- lubridate::year(df$fecha)
df$anomes <- lubridate::make_date(year = df$ano, month = df$mes, day = 1L)
df$anotrimestre <- lubridate::quarter(df$fecha, with_year = TRUE)


# Para los gráficos voy a usar datos sin NA lo cual hace perder unos 3-4 mil datos.
(table(df$area, deparse.level = 2, dnn = "Area") %>% 
        as.data.frame(., responseName = "Avisos") %>%
        ggplot(., aes( x = reorder(Area, Avisos), y = Avisos)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        theme(axis.text.y = element_text(size = 6)) +
        labs(x = "Área", y = "Cantidad de Avisos")) %>% 
    plotly::ggplotly()
# La mayoria se centra en servicios y oficios.
# Luego ventas - comercial
# Lo mismo del gráfico anterior pero en formato tabla, retornando los n primeros a elegir en la función head.
table(df$area, deparse.level = 2, dnn = "Area") %>% 
    as.data.frame(., responseName = "Avisos") %>% 
    arrange(desc(Avisos)) %>% head(., n=5)

# Análisis del nivel jerarquico de las publicaciones
grafico <- function(group, x, filtro = TRUE, filtrar = 5) {
    group = enquo(group)
    x = enquo(x)
    (df %>% 
         group_by(!!group, !!x) %>% 
         summarise(avisos = n()) %>% 
         arrange(avisos) %>% 
            {if(isTRUE(filtro)) dplyr::filter(., avisos >= filtrar) else .} %>% 
         ggplot(., aes(x = !!group, y = avisos, color = !!x)) +
         geom_line()) %>% 
        plotly::ggplotly()    
}
grafico(anomes, jerarquia, filtrar = 100, filtro = FALSE)
grafico(anomes, area, filtrar = 50)

## Serie temporal de avisos
df$uno = 1 
df$id = seq.int(nrow(df))
serie = df %>% 
    group_by(fecha = anomes) %>%
    summarise(avisos = sum(uno))
(ggplot(serie, aes(x = fecha, y = avisos)) +
    geom_line()) %>% 
    plotly::ggplotly()
# Es correcto filtrar mayo porque arranca a partir del 28, look:
df[order(df$fecha),] %>% head
df[order(df$fecha),] %>% tail
serie <- filter(serie, fecha != "2013-05-01")
(ggplot(serie, aes(x = fecha, y = avisos)) +
        geom_line()) %>% 
    plotly::ggplotly()

sts <- ts(serie$avisos, start = c(2013,6), frequency = 12)

forecast::ggseasonplot(sts)
forecast::ggseasonplot(sts, polar = TRUE)
forecast::ggsubseriesplot(sts)
boxplot(sts~cycle(sts))
sts %>% auto.arima(., stepwise = FALSE, approximation = FALSE, lambda = "auto", biasadj = TRUE) %>% 
    forecast::forecast(., bootstrap = TRUE) %>% autoplot()

