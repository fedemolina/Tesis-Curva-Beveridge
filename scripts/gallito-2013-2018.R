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

# Análisis (breve) de la tabla y generación de la serie. Más adelante.
apply(df, MARGIN = 2, FUN = function(x) length(unique(x)))
sapply(df, function(x) length(unique(x))) # Es más rápido que apply.
lapply(df, FUN = function(x) length(unique(x)))
df[, lapply(.SD, function(x) length(unique(x)))]
# Hay 115675 url no repetidas.
df[duplicated(url), .(fecha, url, puesto)][,.N] # Osea 6415 avisos duplicados según la url.
df[duplicated(url), .(fecha, url, puesto)][,.N, by = .(mes =month(fecha))][order(mes),]
# Notar que el patrón de repetición se mueve entre 400-700 por mes (independiente del año)
df[duplicated(url), .(fecha, url, puesto)][,.N, by = .(ano = year(fecha),mes =month(fecha))][order(ano,mes),][
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

# Genero las series a nivel mensual y trimestral
mes  <- df[!duplicated(df[, url]),][, .(avisos = .N), by = .(ano =year(fecha), q = quarter(fecha), mes = month(fecha))][order(ano, mes),]
trim <- df[!duplicated(url), .(avisos = .N), by = .(ano =year(fecha), q = quarter(fecha))][order(ano, q),] 

mes[, fecha := as.Date(paste(ano, mes, 1, sep = "-"))]

saveRDS(mes, paste0(repo_s, "/s_mensual13-18.rds"))
saveRDS(trim, paste0(repo_s, "/s_trimestral13-18.rds"))



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

