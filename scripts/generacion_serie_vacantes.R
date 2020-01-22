# Serie gallito -----------------------------------------------------------
paquetes <- c("ggplot2", "forecast", "magrittr", "plotly", "data.table")
sapply(paquetes, require, character.only = TRUE)

dt <- readRDS("./Datos/Finales/series_todas_final.rds")
Q  <- readRDS("./Datos/Finales/muestreos.rds")
imp <- readRDS("./Datos/Finales/serie_trim_95-01.rds")

dt[, {plot_ly(.SD) %>% 
        add_trace(x = fecha, y = av_urr_mol, mode = "lines+markers", type = "scatter", name = "UM") %>%
        add_trace(x = fecha, y = av_ceres, mode = "lines+markers", type = "scatter", name = "ceres") %>%
        add_trace(x = fecha, y = av_ceres + season_kal, mode = "lines+markers", type = "scatter", name = "ceres_kal") %>%
        # add_trace(x = fecha, y = av_umc, mode = "lines", type = "scatter", name = "union") %>% 
        # add_trace(x = fecha, y = av_umc_c, mode = "lines", type = "scatter", name = "union_c") %>%
        add_trace(data = Q, x =~ fecha, y =~ avisos, mode = "markers", type = "scatter", name = "muestreos")
        }]

dt[, {plot_ly(.SD) %>% 
        add_trace(x = fecha, y = av_urr_mol, mode = "lines+markers", type = "scatter", name = "UM") %>%
        add_trace(x = fecha, y = av_ceres, mode = "lines+markers", type = "scatter", name = "ceres") %>% 
        add_trace(x = fecha, y = av_ceres + season_kal, mode = "lines+markers", type = "scatter", name = "ceres_kal") %>%
        add_trace(x = fecha, y = av_ceres + season_kal_adj, mode = "lines+markers", type = "scatter", name = "ceres_kal") %>%
        add_trace(data = imp, x =~ fecha, y =~ avisos_dest, mode = "lines+markers", type = "scatter", name = "molina") %>% 
        add_trace(data = imp, x =~ fecha, y =~ avisos, mode = "lines+markers", type = "scatter", name = "molina") %>% 
        add_trace(data = Q, x =~ fecha, y =~ avisos, mode = "markers", type = "scatter", name = "muestreos")
    }]

ts <- seasonal::seas(ts(dt$av_urr_mol, start = c(1980,1), frequency = 4))
ts[["data"]]
plot(ts[["data"]])
(ts[["data"]][, "seasonaladj"] %>% autoplot(.)) %>% ggplotly(.)

dt[fecha <= "2001-01-01", av_urr_mol_dest := as.vector(ts[["data"]][, "final"])]
dt[fecha <= "2001-01-01", av_urr_mol_trend := as.vector(ts[["data"]][, "trend"])]

dt[, {plot_ly(.SD) %>% 
        add_trace(x = fecha, y = av_urr_mol, mode = "lines+markers", type = "scatter", name = "UM") %>%
        add_trace(x = fecha, y = av_ceres, mode = "lines+markers", type = "scatter", name = "ceres") %>% 
        add_trace(x = fecha, y = av_ceres + season_kal, mode = "lines+markers", type = "scatter", name = "ceres_kal") %>%
        add_trace(x = fecha, y = av_ceres + season_kal_adj, mode = "lines+markers", type = "scatter", name = "ceres_kal") %>%
        add_trace(x = fecha, y = av_urr_mol_dest, mode = "lines+markers", type = "scatter", name = "UM_dest") %>%
        add_trace(x = fecha, y = av_urr_mol_trend, mode = "lines+markers", type = "scatter", name = "UM_trend")
        # add_trace(data = imp, x =~ fecha, y =~ avisos_dest, mode = "lines+markers", type = "scatter", name = "molina") %>% 
        # add_trace(data = imp, x =~ fecha, y =~ avisos, mode = "lines+markers", type = "scatter", name = "molina") %>% 
        # add_trace(data = Q, x =~ fecha, y =~ avisos, mode = "markers", type = "scatter", name = "muestreos")
}]

# Prueba, uno las series y luego desestacionalizo.
dt[fecha <= "1998-07-01", av_umc := av_urr_mol
   ][between(fecha, "1998-07-01", "2014-07-01"), av_umc := av_ceres]

ts <- seasonal::seas(ts(dt$av_umc, start = c(1980,1), frequency = 4))
ts[["data"]]
plot(ts[["data"]])
(ts[["data"]][, "seasonaladj"] %>% autoplot(.)) %>% ggplotly(.)

dt[fecha <= "2014-07-01", av_umc_dest := as.vector(ts[["data"]][, "final"])]
dt[fecha <= "2014-07-01", av_umc_trend := as.vector(ts[["data"]][, "trend"])]

dt[, {plot_ly(.SD) %>% 
        add_trace(x = fecha, y = av_urr_mol, mode = "lines+markers", type = "scatter", name = "UM") %>%
        add_trace(x = fecha, y = av_ceres, mode = "lines+markers", type = "scatter", name = "ceres") %>% 
        # add_trace(x = fecha, y = av_ceres + season_kal, mode = "lines+markers", type = "scatter", name = "ceres_kal") %>%
        # add_trace(x = fecha, y = av_ceres + season_kal_adj, mode = "lines+markers", type = "scatter", name = "ceres_kal") %>%
        add_trace(x = fecha, y = av_urr_mol_dest, mode = "lines+markers", type = "scatter", name = "UM_dest") %>%
        add_trace(x = fecha, y = av_urr_mol_trend, mode = "lines+markers", type = "scatter", name = "UM_trend") %>% 
        add_trace(x = fecha, y = av_umc, mode = "lines+markers", type = "scatter", name = "temp") %>%
        add_trace(x = fecha, y = av_umc_dest, mode = "lines+markers", type = "scatter", name = "temp_dest") %>%
        add_trace(x = fecha, y = av_umc_trend, mode = "lines+markers", type = "scatter", name = "temp_trend")
}]

# El hecho de unir las series en forma bruta y luego desestacionalizarla o calcular la tendencia+ciclo no genera problemas, las alteraciones
# son leves, no afecta la forma de la serie.
# Es necesario analizar los efectos de forma rigurosa.
# Pero dado el objetivo de buscar quiebres estructurales, sigo adelante porque no genera problemas en dicho sentido.

# Ahora repito el proceso para 2014 en adelante.
# Pero la 1ra etapa es poder unir con la serie de gallito de la base de datos (la variable inicialmente no observable).
# Hay que realizar dos reponderaciones:
# 1. Gallitos papel difieren de gallitos base de datos (y web), reponderar para coincidir (notar la diferencia con los muestreos).
# 2. Gallito ~ 2010-2012 representa el 100%
#    Gallito ~ 2019      representa el 36-40%
# Si mi universo de análisis (población) es gallito + buscojobs + computrabajo + uruguayConcursa*
# obs*: UruguayConcursa viene en buscojobs.
# Entonces puedo realizar la suma de los avisos en los períodos que coinciden.

dt[, {plot_ly(.SD) %>% 
        # add_trace(x = fecha, y = av_umc, mode = "lines+markers", type = "scatter", name = "UM") %>%
        # add_trace(x = fecha, y = av_ga_c_dup, mode = "lines+markers", type = "scatter", name = "ceres") %>% 
        add_trace(data = Q, x =~ fecha, y =~ avisos, mode = "markers", type = "scatter", name = "muestreos") %>%
        add_trace(x = fecha, y = av_umc, mode = "lines+markers", type = "scatter", name = "avisos_umc") #%>%
        # add_trace(x = fecha, y = av_umc_dest, mode = "lines+markers", type = "scatter", name = "temp_dest") %>%
        # add_trace(x = fecha, y = av_umc_trend, mode = "lines+markers", type = "scatter", name = "temp_trend") %>% 
        # add_trace(x = fecha, y = av_ga_c_dup, mode = "lines+markers", type = "scatter", name = "ga_bj_ct")
}]

dt[, {plot_ly(.SD) %>% 
        add_trace(x = fecha, y = av_ga_c_dup, mode = "lines+markers", type = "scatter", name = "ga_c_dup") %>%
        add_trace(x = fecha, y = av_ga_s_dup, mode = "lines+markers", type = "scatter", name = "ga_s_dup")
}]

# Parte 1.A
# Reponderación por avisos repetidos (por link). Se filtra en base a los avisos repetidos por mes en la base de datos de gallito, el promedio
# entre 2013-2018 es que los avisos sin repetir (POR LINK, ie, ID) representa ~ 94% de los avisos totales.

# Parte 1.B
# Reponderación porque la variable de interés no observable 1980-2014 (avisos reales, BD) no coincide con la variable observable, medida con error,
# gallito en papel.
# Periodo observaod de las 2 series 2013-q4 hasta 2014q4. Se hace para los avisos SIN duplicados (por link)

# Parte 1.
# A y B se realizan de forma conjunta. En vez de reponderar 2 veces, se realiza el calculo de la parte B respecto a la serie de avisos de 
# gallito de la BD ya filtrada, lo cual es más claro.

(comparacion_gallito <- data.table(
    avisos_bd_s_dup = dt[data.table::between(fecha, "2013-10-01", "2014-10-01"), av_ga_s_dup],
    avisos_bd_c_dup = dt[data.table::between(fecha, "2013-10-01", "2014-10-01"), av_ga_c_dup],
    avisos_papel    =  Q[data.table::between(fecha, "2013-10-01", "2014-10-01"), avisos],
    fecha = seq.Date(as.Date("2013-10-01"), as.Date("2014-10-01"), "quarter")
))
comparacion_gallito[, `:=`(ratio_s_dup = avisos_papel/avisos_bd_s_dup,
                           ratio_c_dup = avisos_papel/avisos_bd_c_dup,
                           diferencia_s_dup = as.integer(avisos_papel - avisos_bd_s_dup),
                           pct_s_dup = avisos_bd_s_dup/avisos_papel,
                           pct_c_dup = avisos_bd_c_dup/avisos_papel)]
setkey(comparacion_gallito, "fecha")
comparacion_gallito
comparacion_gallito[, sapply(.(ratio_s_dup, pct_s_dup, diferencia_s_dup), mean)]
# Los avisos en papel tiene duplicados
# Los avisos de BD a utilizar están filtrados de duplicados por link.
# Notar como, cuando se calcula el ratio/pct la diferencia disminuye 
# En el caso de los ratios ~ 5 pp
# Visto como porcentaje avisos_BD/avisos_papel ~ 3 pp.
# En el único caso que casi no se modifica es en 2013-10-01, que tiene TODO noviembre imputado, lo cual
# parece estar generando que sea un trimestre sobrecalculado en papel.

# Revisar esto que escribí:
# O sea, los avisos reales son entre un 70-80% de los avisos observados a nivel trimestral, en el año 2014.
# Lo que sería entre un 26-42% de avisos repetidos por trimestre. 
# El promedio es de 33% de repetidos, 
# Avisos reales un 75% de los observados y una diferencia prom de 2550 trimestral.
# En el análisis de texto, BORRANDO los avisos con link repetidos, y tomando un umbral de 0.8 se obtiene para gallito,
# aproximadamente un 10% de avisos repetidos promedio anual.
# Hay que volver a calcular la similaridad de avisos SIN borrar los link y ver que % da.
# Va a ser igual o mayor, puede llevar a que la diferencia disminuya.

# Cuanta diferencia promedio por semana aprox? 
comparacion_gallito[, .(avisos_bd_s_dup, avisos_papel)]/12
(comparacion_gallito[, .(avisos_bd_s_dup, avisos_papel)]/12)[, .(avisos_papel - avisos_bd_s_dup)]
# En torno a 200 avisos de diferencia entre el papel y la base de datos.
# La base de avisos del país es la variable no observable, avisos laborales.
# Los avisos son la variable observada con ruido y error de medición. 
# Esta diferencia nos puede estar dando una pauta de que las empresas pagan por publicar sus avisos y los mismos permanecen
# durantes varias semanas, lo cual genera que el número de avisos laborales observados este sobreestimado de los avisos
# que realmente fueron solicitados en el período.

# Reponderar entonces con un solo valor, moviendo todo el nivel de la serie, usar el promedio 2014q1 2014q4 avisos papel
# Obs: recordar que el promedio de los ratios es != del ratio de los promedios.
# Se puede tomar un promedio de papel y BD y calcular un ratio, o elegir algún valor de los ratio
# Pero, los valores en los meses del 4to trimestre generan diferencias mayores porque la serie de BD
# contiene una fuerte estacionalidad, al desestacionalizarla (ver gráfico) esas diferencias disminuyen.
# Además el resto de valores de la serie están por debajo de los muestreos, y al desestacionalizarla
# ello no mejora, porque la estacionalidad esta especialmente en diciembre y enero, por lo cual
# quedaría una diferencia de nivel. Entonces lo más sensato sería unirlas en base los ratios sin
# dichos trimestres
# Además, 2013-10-01 tiene TODO noviembre imputado y puede estar sobreestimado, viendose una dif mayor
# El resto de los muestreos de 2014, no tienen valores imputados, siendo más confiables y muestran 
# diferencias sustancialmente menores.
(ponderador_s_dup = comparacion_gallito[between(fecha, "2014-04-01", "2014-07-01"), 
                                       mean(avisos_papel)/mean(avisos_bd_s_dup)])
dt[, {plot_ly(.SD) %>% 
        add_trace(data = Q, x =~ fecha, y =~ avisos/ponderador_s_dup, mode = "markers", type = "scatter", name = "muestreos") %>%
        add_trace(x = fecha, y = av_umc/ponderador_s_dup, mode = "lines+markers", type = "scatter", name = "av_umc/pond") %>%
        add_trace(x = fecha, y = av_ga_s_dup, mode = "lines+markers", type = "scatter", name = "av_ga_s_dup") %>%
        add_trace(x = fecha, y = av_ga_s_dup_dest, mode = "lines+markers", type = "scatter", name = "av_ga_s_dup_dest") %>%
        add_trace(x = fecha, y = av_umc/ponderador_s_dup + season_kal/ponderador_s_dup, mode = "lines+markers", type = "scatter", name = "av_umc/pond_s_dup")
}]

# Etapa 2. (revisar luego las etapas, las fui haciendo sobre la marcha)
# 1. Tomar serie av_umc desde 1980-2012.01, período en que coincide con avisos de gallito papel.
# 2. Reponderar dicha serie en base al ponderador_s_dup
# 3. Tomar el muestreo 2013.01 y reponderar en base a ponderador_s_dup
# 4. Tomar la serie av_ga_s_dup entre 2013.3 y 2019.4
# 5. Generar una tabla temporal con secuencia de fechas desde 1980.01-2019.4 + variable avisos con NA
# 6. Insertar los valores de las variables previamente mencionadas.
# 7. Imputar los valores faltantes.
# 8. Agregar dicha serie a la table dt
# FIN.

# Pasos 1-6.
a = dt[data.table::between(fecha, "1980-01-01", "2012-01-01"), av_umc]/ponderador_s_dup
b = Q[fecha == "2013-01-01", avisos]/ponderador_s_dup
c = dt[data.table::between(fecha, "2013-07-01", "2019-10-01"), av_ga_s_dup]
temp <- data.table(fecha = seq.Date(as.Date("1980-01-01"), as.Date("2019-10-01"), "quarter"),
                   avisos = NA_real_)
temp[data.table::between(fecha, "1980-01-01", "2012-01-01"), avisos := a
     ][fecha == "2013-01-01", avisos := b
       ][between(fecha, "2013-07-01", "2019-10-01"), avisos := c]
rm(a,b,c)
temp[, plot_ly(.SD) %>% 
         add_trace(x =fecha, y = avisos, mode = "markers", type = "scatter", name = "muestreos")]

# Paso 7.
temp_ts <- ts(temp$avisos, start = c(1980,1), frequency = 4)

temp_kal    <- imputeTS::na_kalman(temp_ts, model = "StructTS", smooth = TRUE, type = "BSM")
temp_arima  <- imputeTS::na_kalman(temp_ts, model = "auto.arima", stepwise = TRUE, parallel = TRUE) # con TRUE no cambia
temp_seadec <- imputeTS::na_seadec(temp_ts, algorithm = "interpolation")
temp_seadec_kal <- imputeTS::na_seadec(temp_ts, algorithm = "kalman")
temp_seas   <-  imputeTS::na_seasplit(temp_ts, algorithm = "interpolation")
temp_seas_kal   <-  imputeTS::na_seasplit(temp_ts, algorithm = "kalman")

(autoplot(temp_kal) +
    autolayer(temp_arima) +
    autolayer(temp_seadec) +
    autolayer(temp_seadec_kal) +
    autolayer(temp_seas) +
    autolayer(temp_seas_kal)) %>% 
    ggplotly(.)
# Salvo seasplit el resto estiman igual.
# Tiene mucha sentido que la serie imputada capte la estacionalidad de la serie con la cual se va a unir
# Se opta por dicho método. Usar interpolación (cualquiera de sus tipos) o kalman no ofrece diferencias.
impute_spanish <- function (x.withNA, x.withImputations, x.withTruth = NULL, legend = TRUE, 
                            main = "Visualization Imputed Values", xlab = "Time", ylab = "Value", 
                            colWithTruth = "green3", colLines = "black", colWithImputations = "indianred2", 
                            colWithNA = "steelblue2", ylim = c(min(c(x.withImputations, 
                                                                     x.withTruth), na.rm = TRUE), max(c(x.withImputations, 
                                                                                                        x.withTruth), na.rm = TRUE)), pch = 20, cex = 0.8, ...) 
{
    data.withNA <- x.withNA
    data.withImputations <- x.withImputations
    data.withTruth <- x.withTruth
    if (!is.null(dim(data.withNA)) && dim(data.withNA)[2] != 
        1) {
        stop("Input data.withNA is not univariate")
    }
    if (!is.numeric(data.withNA)) {
        stop("Input data.withNA is not numeric")
    }
    if (!is.null(dim(data.withImputations)) && dim(data.withImputations)[2] != 
        1) {
        stop("Input data.withImputations is not univariate")
    }
    if (!is.numeric(data.withImputations)) {
        stop("Input data.withImputations is not numeric")
    }
    if (!is.ts(data.withNA)) {
        data.withNA <- as.vector(data.withNA)
    }
    if (!is.ts(data.withImputations)) {
        data.withImputations <- as.vector(data.withImputations)
    }
    if (!is.ts(data.withTruth)) {
        data.withTruth <- as.vector(data.withTruth)
    }
    id.na <- which(is.na(data.withNA))
    par.default <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(par.default))
    if (legend == TRUE) {
        graphics::par(oma = c(0.5, 0, 0, 0))
    }
    if (is.null(data.withTruth)) {
        graphics::plot(data.withImputations, type = "l", ylim = ylim, 
                       col = colWithImputations, ylab = ylab, xlab = xlab, 
                       main = main, ...)
        graphics::points(data.withImputations, col = colWithImputations, 
                         pch = pch, cex = cex)
        graphics::lines(data.withNA, col = colLines)
        graphics::points(data.withNA, col = colWithNA, pch = pch, 
                         cex = cex)
        if (legend == TRUE) {
            graphics::par(fig = c(0, 1, 0, 1), oma = c(0, 0, 
                                                       0, 0), mar = c(0, 0, 0, 0), new = TRUE)
            graphics::plot(0, 0, type = "n", bty = "n", xaxt = "n", 
                           yaxt = "n")
            graphics::legend("bottom", bty = "n", xjust = 0.5, 
                             horiz = TRUE, cex = 1, legend = c("Valores imputados", 
                                                               "Valores conocidos"), col = c("indianred2", "steelblue"), 
                             pch = c(20))
        }
    }
    else {
        if (!is.null(dim(data.withTruth)) && dim(data.withTruth)[2] != 
            1) {
            stop("Input x.withTruth is not univariate")
        }
        if (!is.numeric(data.withTruth)) {
            stop("Input x.withTruth is not numeric")
        }
        graphics::plot(data.withTruth, type = "l", ylim = ylim, 
                       col = colWithTruth, ylab = ylab, xlab = xlab, main = main, 
                       ...)
        graphics::points(data.withTruth, col = colWithTruth, 
                         pch = pch, cex = cex)
        graphics::lines(data.withNA, col = colLines)
        graphics::points(data.withImputations, col = colWithImputations, 
                         pch = pch, cex = cex)
        graphics::points(data.withNA, col = colWithNA, pch = pch, 
                         cex = cex)
        if (legend == TRUE) {
            graphics::par(fig = c(0, 1, 0, 1), oma = c(0, 0, 
                                                       0, 0), mar = c(0, 0, 0, 0), new = TRUE)
            graphics::plot(0, 0, type = "n", bty = "n", xaxt = "n", 
                           yaxt = "n")
            graphics::legend("bottom", bty = "n", xjust = 0.5, 
                             horiz = TRUE, cex = 1, legend = c("Valores imputados", 
                                                               "real values", "Valores conocidos"), col = c("indianred2", 
                                                                                                       "green", "steelblue"), pch = c(20))
        }
    }
}

imputeTS::plotNA.imputations(x.withNA = temp_ts, x.withImputations = temp_kal, main = "Valores imputados", xlab = "fecha")
imputeTS::plotNA.imputations(x.withNA = temp_ts, x.withImputations = temp_seas_kal, 
                             main = "Valores imputados", xlab = "fecha", ylab = "Avisos", legend = F)
impute_spanish(x.withNA = temp_ts, x.withImputations = temp_seas_kal, 
               main = "Valores imputados", xlab = "fecha", ylab = "Avisos", legend = T)
imputeTS::plotNA.imputations(x.withNA = temp_ts, x.withImputations = temp_seas, main = "Valores imputados", xlab = "fecha")

# Luego estará la pregunta de que uno series con estacionalidades (o sin) diferentes, pero al final la 
# serie que se va a utilizar será desestacionalizada o en tendencia ciclo, por lo cual esto no la va a 
# afectar, notar a continuación:
seasonal::seas(temp_seas) %>% plot()
seasonal::seas(temp_seas)[["data"]][,"trend"] %>% plot()
lines(temp_ts)
# Excelente.

# Paso 8.
dt[, av_umcg := as.vector(temp_seas_kal)]

# Por último:
# Recalcular los dest y trend, o en su defector borrarlos? Mejor dejarlos y luego comparar con 
# av_umcg desestacionalidad y trend.
rm(list = ls()[grepl(ls(), pattern = "temp_|ts|ponderador")])

# Serie final de gallito
dt[, plot_ly(.SD) %>% 
         add_trace(x =fecha, y = av_umcg, mode = "markers+lines", type = "scatter", name = "muestreos")]


# bj y ct -----------------------------------------------------------------
bj_ct <- readRDS("./Datos/Finales/serie_trim_bj_ct.rds")
# dt <- readRDS("./Datos/Finales/series_todas_final.rds")
setkey(bj_ct, "fecha")
setkey(dt, "fecha")

# # Columnas para agregar
str(bj_ct)
(cols <- names(bj_ct)[grepl(names(bj_ct), pattern = "av_")])

# Join y renombrar
dt[bj_ct, (cols) := mget(cols)]

# (pond_ene08 <- dt[fecha == "2008-07-01", (av_umcg + av_ct_bj_s_dup_c_fil)/av_umcg])

dt[, 
   plot_ly(.SD) %>%
        # add_trace(x = fecha, y = av_umcg*pond_ene08, mode = "lines+markers", type = "scatter", name = "union_final") %>%
        add_trace(x = fecha, y = av_umcg, mode = "lines+markers", type = "scatter", name = "union_final") %>%
        add_trace(x = fecha, y = av_ct_bj_s_dup_c_fil, mode = "lines+markers", type = "scatter", name = "bj_ct_s_f") %>%
        add_trace(x = fecha, y = av_umcg + av_ct_bj_s_dup_c_fil, mode = "lines+markers", type = "scatter", name = "union_final_ct_bj") %>% 
        add_trace(x = fecha, y = av_umcg + av_ct_bj_s_dup_c_fil - av_umcg*0.05, mode = "lines+markers", type = "scatter", name = "union_final_ct_bj") %>% 
        add_trace(x = fecha, y = av_umc, mode = "lines+markers", type = "scatter", name = "union") #%>% 
        # add_trace(x = fecha, y = av_umc/ponderador_s_dup, mode = "lines+markers", type = "scatter", name = "union_pond")       
]
# FALTA RESTAR EL % DE REPETIDOS DEL GALLITO CON DICHAS PÁGINAS. HACERLO SOLAMENTE A PARTIR DE 2007-01-01
# que es cuando comienza a aumentar el volumen de buscojobs y computrabajo.
# Las series son iguales en 2003-04-01 y prácticamente iguales en 2003-07-01 que es cuando comienza a existir computrabajo.
# El pico de 2008 se explica por un aumento fuera de lo común en buscojobs.

# Genero una serie que una las 3 restantes. (gallito, bj y ct  SIN duplicados link y CON filtro de avisos repetidos entre páginas.
# Gallito tiene ~ 4% de repetidos en cada página.
dt[fecha < "2003-07-01", av_final := av_umcg
   ][data.table::between(fecha, "2003-07-01", "2007-04-01"), av_final := av_umcg + av_ct_bj_s_dup_c_fil
   ][fecha >= "2007-07-01", av_final := av_umcg + av_ct_bj_s_dup_c_fil - av_umcg*0.04]
dt[, plot_ly(.SD) %>% 
       add_trace(x = fecha, y = av_final, mode = "lines+markers", type = "scatter", name = "vacantes") %>% 
       add_trace(x = fecha, y = av_ceres, mode = "lines+markers", type = "scatter", name = "ceres")]
# # La caída de 2019 es por computrabajo. Eso es relevante, porque entre 2015-2019 tiene un crecimiento muy relevante, que
# # empuja toda la serie, a la vez que la caída tan drámatica a partir de 2019 genera una caída muy abrupta.
# # Por seguridad, podría cortarse en 2018, hasta no tener seguridad de que paso con dicha página web y si esos avisos, no 
# # terminaron volcandose a otra página.

# Finalmente calculo la serie en tendencia-ciclo en vez de desestacionalizada.
ts <- seasonal::seas(ts(dt$av_final, start = c(1980,1), frequency = 4))
plot(ts)
ts[["data"]]
plot(ts[["data"]])
(ts[["data"]][, "seasonaladj"] %>% autoplot(.)) %>% ggplotly(.)
(ts[["data"]][, "trend"] %>% autoplot(.)) %>% ggplotly(.)
dt[!is.na(av_ceres), plot(x = fecha, y = av_ceres, type = "l")]

dt[, av_final_tc := ceiling(as.vector(ts[["data"]][, "trend"]))]
dt[, av_final_dest := ceiling(as.vector(ts[["data"]][, "seasonaladj"]))]

dt[, plot_ly(.SD) %>% 
       # add_trace(x = fecha, y = av_final, mode = "lines+markers", type = "scatter", name = "vacantes") %>% 
       add_trace(x = fecha, y = av_final_tc, mode = "lines+markers", type = "scatter", name = "vacantes") %>% 
       # add_trace(x = fecha, y = av_final_dest, mode = "lines+markers", type = "scatter", name = "vacantes") %>% 
       # add_trace(x = fecha, y = av_ceres, mode = "lines+markers", type = "scatter", name = "ceres") %>% 
       plotly::layout(xaxis = list("fecha"), yaxis = list("vacantes laborales"), title = "Índice Molina de vacantes laborales")
   ]


subplot(
    plot_ly(dt, x = ~fecha, y = ~av_final_tc) %>% 
        layout(annotations = list(x = 0.2 , y = 1.05, text = "AA", showarrow = F, 
                                  xref='paper', yref='paper'), 
               showlegend = FALSE))
saveRDS(dt, "./Datos/Finales/series_version_final.rds")
