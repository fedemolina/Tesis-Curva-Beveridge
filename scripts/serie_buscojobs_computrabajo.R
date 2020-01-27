# Buscojobs ---------------------------------------------------------------
paquetes <- c("ggplot2", "forecast", "plotly", "imputeTS", "data.table")
sapply(paquetes, require, character.only = TRUE)

# Datos
bj <- readxl::read_excel("./Datos/Originales/waybackMachine/buscoJobs.xlsx", range = readxl::cell_cols("A:B"))
scraping <- readRDS("./Datos/Finales/AvisosCompatibilizados.rds")

# Genero la serie de avisos para 2019.
(bj_19 <- scraping[ano == 2019 & pagina == "buscojobs", 
                  .(avisos = .N,
                    fecha =as.Date(paste(unique(ano), unique(mes), 01, sep = "-")),
                    ano = unique(ano))
                  , keyby = .(mes)])
# Acá ya están limpiados los duplicados? Revisar -> SI
scraping[duplicated(scraping, by = "id"), .N, by = .(ano, mes, pagina)]
# Solo gallito en 2018, porque el id (link) es NA, no se tenía. OK

setDT(bj)
bj[, `:=`(mes = data.table::month(fecha),
          # dia = data.table::mday(fecha),
          ano = data.table::year(fecha))]
bj[, .N, keyby = .(ano, mes)
   ][, .(N = N,
         fecha = as.Date(paste(ano, mes, 01, sep = "-")))
     ][, ggplot(.SD, aes(x = fecha, y = N)) +
           geom_line() +
           labs(x = "fecha", y = "Cantidad de observaciones") +
           theme_bw()] #%>% 
    # ggsave(filename = "cantidad-obs-buscojobs.png", path = "./output")
setkey(bj, fecha)
# bj <- bj[!duplicated(bj, by = c("ano", "mes"), fromLast = FALSE),] 
bj <- bj[, .(avisos = mean(avisos, na.rm = TRUE),
             fecha  = as.Date(paste(ano, mes, 01, sep = "-"))), 
         keyby = .(ano, mes)]

# me quedo con el último día que aparezca del mes? Probando parece tener más sentido usar fromLast = FALSE, no es
# muy sensato que venga con una tendencia a la baja en pleno auge de la economía y ganando terreno como portal
# Suponer que la cantidad de avisos observados en cualquier punto del mes es representativo
# Generar serie mensual bajo dicho supuesto

bj[, plot_ly(.SD) %>% 
       add_trace(x = fecha, y = avisos, mode = "lines+markers")]

# Uno las series de buscojobs.
bj <- rbindlist(l = list(bj, bj_19), use.names = TRUE)
rm(bj_19)

bj[fecha %in% c(as.Date("2019-08-01"), as.Date("2019-09-01")), avisos := NA_real_]

bj[, plot_ly(.SD) %>% 
       add_trace(x = fecha, y = avisos, mode = "lines+markers")]

# Imputar los valores faltantes
bj_ts <- ts(bj[, avisos], start = c(2007, 6),
            end = c(2019, 12), frequency = 12)
imputeTS::plotNA.distribution(bj_ts, xlab = "fecha", y = "Cantidad de avisos", main = "")
bj_imp_ts <- imputeTS::na_kalman(bj_ts, smooth = TRUE, model = "StructTS", optim.control = list(maxit = 1000))
# bj_imp_ts_sm_no <- imputeTS::na_kalman(bj_ts, smooth = FALSE, model = "StructTS", optim.control = list(maxit = 1000))
# bj_imp_ts1 <- imputeTS::na_kalman(bj_ts, smooth = FALSE, model = "StructTS", optim.control = list(maxit = 1000))
bj_imp_ts2 <- imputeTS::na_kalman(bj_ts, model = "auto.arima", optim.control = list(maxit = 500), 
                                  stepwise = T, parallel = TRUE, seasonal = TRUE) 
bj_imp_seas <- imputeTS::na_seasplit(bj_ts, algorithm = "interpolation")
bj_imp_seas_kal <- imputeTS::na_seasplit(bj_ts, algorithm = "kalman")
bj_imp_seasdec <- imputeTS::na_seadec(bj_ts, algorithm = "interpolation")
bj_imp_seasdec_kal <- imputeTS::na_seadec(bj_ts, algorithm = "kalman")

(autoplot(bj_ts) +
    autolayer(bj_imp_ts) +
    # autolayer(bj_imp_ts_sm_no) +
    autolayer(bj_imp_ts2) + 
    autolayer(bj_imp_seas) +
    # autolayer(bj_imp_seas_ma) +
    autolayer(bj_imp_seas_kal) +
    autolayer(bj_imp_seasdec) +
    autolayer(bj_imp_seasdec_kal)) %>% 
    ggplotly()
# Voy a elegir el método seasdec con filtro de kalman, en general elijo seas pero el pico que tiene al comienzo no parece ser
# correcto (no hay como saberlo, pero dado que la página recién surge no tiene sentido)
rm(bj_imp_ts, bj_imp_ts2, bj_imp_seas, bj_imp_seas_kal, bj_imp_seasdec)

# # Splines
# bj_imp_linear <- imputeTS::na_interpolation(bj_ts, option = "linear")
# bj_imp_spline <- imputeTS::na_interpolation(bj_ts, option = "spline", method = "fmm", xmin = 55, xmax = 65, ties = min)
# bj_imp_stine <- imputeTS::na_interpolation(bj_ts, option = "stine", method = "parabola")
# 
# autoplot(bj_imp_ts) +
#     autolayer(bj_imp_linear) +
#     autolayer(bj_imp_spline) +
#     autolayer(bj_imp_stine)

# asigno avisos imputados
bj[, avisos_bj_imp := ceiling(as.vector(bj_imp_seasdec_kal))]
bj[, plot_ly(.SD) %>% 
       add_trace(x = fecha, y = avisos_bj_imp, mode = "lines+markers")]
bj[, avisos := NULL]


# Computrabajo ------------------------------------------------------------

ct <- readxl::read_excel("./Datos/Originales/waybackMachine/computrabajo.xlsx", range = readxl::cell_cols("A:B"))
setDT(ct)
ct[, `:=`(mes = data.table::month(fecha),
          ano = data.table::year(fecha))]
setkey(ct, fecha)
ct <- ct[!duplicated(ct, by = c("ano", "mes"), fromLast = FALSE),] 
# Repondero porque los avisos publicados, no se corresponden a los avisos de los últimos 30 días
ct[, plot_ly(.SD) %>% 
       add_trace(x = fecha, y = avisos, mode = "lines+markers")]
ct[fecha >= "2011-07-01", avisos := ceiling(avisos*0.42)
   ][fecha < "2011-07-01", avisos := ceiling(avisos*0.63)]
# ct[, avisos := ceiling(avisos*0.42)] # Repondero a un 42%
ct[, fecha := as.Date(paste(ano, mes, 01, sep ="-"))]

# Agrego los scraping de 2019 y q4 2018
(ct_19 <- scraping[pagina == "computrabajo", 
                   .(avisos = .N,
                     fecha =as.Date(paste(unique(ano), unique(mes), 01, sep = "-"))
                     )
                   , keyby = .(ano, mes)])
ct_19[fecha %in% c(as.Date("2019-06-01"), as.Date("2019-08-01"), as.Date("2019-09-01")), avisos := NA_real_]

# Uno las series de computrabajo
ct <- rbindlist(l = list(ct[fecha < "2018-10-01",], ct_19), use.names = TRUE)
rm(ct_19)


ct[, plot_ly(.SD) %>% 
       add_trace(x = fecha, y = avisos, mode = "lines+markers")]
# Imputar los valores faltantes
ct_ts <- ts(ct[, avisos], start = c(2003, 5),
            end = c(2019, 12), frequency = 12)
imputeTS::plotNA.distribution(ct_ts)
ct_imp_kal <- imputeTS::na_kalman(ct_ts, smooth = TRUE, model = "StructTS")
ct_imp_arima <- imputeTS::na_kalman(ct_ts, model = "auto.arima")
ct_imp_seas <- imputeTS::na_seasplit(ct_ts, algorithm = "interpolation")
ct_imp_seas_kal <- imputeTS::na_seasplit(ct_ts, algorithm = "kalman")
ct_imp_seasdec <- imputeTS::na_seadec(ct_ts, algorithm = "interpolation")
ct_imp_seasdec_kal <- imputeTS::na_seadec(ct_ts, algorithm = "kalman", type = "BSM")
ct_imp_seasdec_kal_level <- imputeTS::na_seadec(ct_ts, algorithm = "kalman", type = "level")
ct_imp_seasdec_kal_trend <- imputeTS::na_seadec(ct_ts, algorithm = "kalman", type = "trend")

(autoplot(ct_ts) +
        autolayer(ct_imp_kal) + 
        # autolayer(ct_imp_arima) + 
        autolayer(ct_imp_seas) +
        autolayer(ct_imp_seas_kal) +
        autolayer(ct_imp_seasdec) +
        autolayer(ct_imp_seasdec_kal) +
        autolayer(ct_imp_seasdec_kal_level) +
        autolayer(ct_imp_seasdec_kal_trend)) %>% 
    ggplotly(.)
# ct_imp_kal

# asigno avisos imputados
ct[, avisos_ct_imp := ceiling(as.vector(ct_imp_kal))]
ct[, plot_ly(.SD) %>% 
       add_trace(x = fecha, y = avisos_ct_imp, mode = "lines+markers")]


# Unión BJ-CT -------------------------------------------------------------

setkey(bj, fecha)
setkey(ct, fecha)
bj[ct]
ct[bj, avisos_bj_imp := avisos_bj_imp]
# ct <- ct[3:NROW(ct), ]

# Caso 1. Ambas series sumadas en su totalidad
# Caso 2. Hay avisos repetidos entre series, (e intra series) que no se deben tomar en cuenta.
# Del análisis de texto, aproximadamente 12% de computrabajo están en buscojobs y ~ viceversa.
# Pero, allí ya están limpiados los avisos repetidos. Por ende ese % puede ser mayor. Tomo 15%
# Antes de julio de 2007 son muy pocos avisos, muy pocas empresas. Es más probable que no existieran repeticiones
# en dichos años.
ct[is.na(avisos_bj_imp), avisos_bj_imp := 0]
ct[, avisos_ct_bj_s_fil := avisos_bj_imp + avisos_ct_imp]
ct[fecha <= "2007-04-01", avisos_ct_bj_c_fil := (avisos_bj_imp + avisos_ct_imp)
   ][fecha > "2007-04-01",
    avisos_ct_bj_c_fil := (avisos_bj_imp + avisos_ct_imp) - ceiling((avisos_bj_imp + avisos_ct_imp)*0.15)]
ct[, plot_ly(.SD) %>% 
       add_trace(x = fecha, y = avisos_ct_bj_s_fil, mode = "lines+markers") %>% 
       add_trace(x = fecha, y = avisos_ct_bj_c_fil, mode = "lines+markers")]

ct[, q := data.table::quarter(fecha)]
ct_q <- ct[, .(avisos_ct_bj_c_fil = sum(avisos_ct_bj_c_fil),
               avisos_ct_bj_s_fil = sum(avisos_ct_bj_s_fil),
               avisos_bj = sum(avisos_bj_imp),
               avisos_ct = sum(avisos_ct_imp),
               fecha = min(fecha),
               mes = min(mes)), keyby = .(ano, q)]
setkey(ct_q, fecha)

# Como quedaron?
ct_q[, plot_ly(.SD) %>% 
         add_trace(x = fecha, y = avisos_ct_bj_s_fil, mode = "lines+markers", name = "bj_ct_s_filtro") %>%
         add_trace(x = fecha, y = avisos_ct_bj_c_fil, mode = "lines+markers", name = "bj_ct_c_filtro") %>%
         add_trace(x = fecha, y = avisos_bj, mode = "lines+markers", name = "bj") %>% 
         add_trace(x = fecha, y = avisos_ct, mode = "lines+markers", name = "ct")]

# reordeno
data.table::setcolorder(ct_q, neworder = c("fecha", "ano", "q", "mes"))
data.table::setcolorder(ct, neworder = c("fecha", "ano", "q", "mes"))

# Guardar las series para luego ser reutilizadas.

# avisos_ct_bj_c_fil = Suma de avisos laborales, computrabajo y buscojobs, con filtro de 15% de avisos repetidos
#                      entre páginas. Dicho % surge del análisis de texto (~12% aprox)
# avisos_ct_bj_s_fil = Suma de avisos laborales, computrabajo y buscojobs, sin filtro ninguno, suma bruta.
# avisos_bj          = Avisos laborales de buscojobs
# avisos_ct          = Avisos laborales de computrabajo
# Obs: Ambas series (bj, ct) fueron imputadas de forma individual dados los NA.
#      Ambas series fueron filtradas de avisos repetidos por link.
# Las observaciones surgen de 2 fuentes:
# 1. WaybackMachine
# 2. Scraping de cada página en 2019 (bj) y 2018q4 y 2019 (ct)

# Para que no queden dudas, avisos_bj, avisos_ct los renombre como avisos_bj_s_dup, avisos_ct_s_dup
setnames(ct_q, old = c("avisos_bj", "avisos_ct", "avisos_ct_bj_c_fil", "avisos_ct_bj_s_fil"),
               new = c("av_bj_s_dup", "av_ct_s_dup", "av_ct_bj_s_dup_c_fil", "av_ct_bj_s_dup_s_fil"))
setnames(ct, old = c("avisos_bj_imp", "avisos_ct_imp", "avisos_ct_bj_c_fil", "avisos_ct_bj_s_fil"),
         new = c("av_bj_s_dup", "av_ct_s_dup", "av_ct_bj_s_dup_c_fil", "av_ct_bj_s_dup_s_fil"))

saveRDS(object = ct_q, file = "./Datos/Finales/serie_trim_bj_ct.rds", compress = FALSE)
saveRDS(object = ct, file = "./Datos/Finales/serie_mensual_bj_ct.rds", compress = FALSE)
