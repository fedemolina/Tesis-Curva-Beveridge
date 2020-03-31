# Estimación Curva de Beveridge -------------------------------------------
paquetes <- c("ggplot2", "forecast", "plotly", "data.table", "bvarsv") # También esta bvars
sapply(paquetes, require, character.only = TRUE)
dt <- readRDS("./Datos/Finales/series_version_final.rds")
pib <- readxl::read_xlsx(here::here("Datos", "Originales", "pib80-19.xlsx"))
setDT(pib)

# Formatear la fecha del pib al formato donde el mes del trimestre es el 1er mes.
pib[, fecha := as.Date(stringi::stri_replace_all_fixed(fecha, 
                                                       pattern = c("-03-", "-06-", "-09-", "-12-"), 
                                                       replacement = c("-01-", "-04-", "-07-", "-10-"), 
                                                       vectorize_all = FALSE))
    ]


# cols <- c("fecha", "av_final_tc", "av_final", "ano", "mes", "q", "pea")
# cols_clean <- names(dt)[!names(dt) %in% cols]
# dt[, (cols_clean) := NULL]
# dt <- dt[complete.cases(dt), ]

dt[, plot_ly(x = fecha, y = ind_vac, type = "scatter", mode = "marker+lines")]
dt[, plot_ly(x = td, y = ind_vac, type = "scatter", mode = "marker+lines", color = decada)]
dt[, plot_ly(x = td_dest, y = ind_vac_dest, type = "scatter", mode = "markers+lines", color = decada)]
dt[, plot_ly(x = td_dest, y = ind_vac_dest, type = "scatter", mode = "lines", color = decada)]

dt[, plot_ly(x = td, y = pib, type = "scatter", mode = "markers+lines", color = decada)]
dt[, plot_ly(x = ind_vac, y = pib, type = "scatter", mode = "markers+lines", color = decada)]

xaxis = list(title = "Tasa de desempleo")
yaxis = list(title = "Tasa de vacantes")
t <- list(
    family = "sans serif",
    size = 10,
    color = toRGB("grey50"))

dt[, .(td = mean(td), ind_vac = mean(ind_vac), pib = mean(pib), decada = min(decada)), keyby = .(ano)
   ][, plot_ly(x = ind_vac, y = pib, type = "scatter", mode = "markers+lines", color = decada, text = ~ ano) %>% 
         add_text(textfont = t, textposition = "top right") %>%
         layout(showlegend = TRUE,
                xaxis = list(title = "Índice de vacantes"),
                yaxis = list(title = "IVF PIB"))]

dt[, .(td = mean(td, na.rm = T), ind_vac = mean(ind_vac, na.rm = T), pib = mean(pib, na.rm = T), decada = min(decada)), keyby = .(ano)
   ][, plot_ly(x = td, y = pib, type = "scatter", mode = "markers+lines", color = decada, text =~ ano) %>% 
         add_text(textfont = t, textposition = "top right") %>%
         layout(showlegend = TRUE,
                xaxis = list(title = "Tasa de desempleo"),
                yaxis = list(title = "IVF PIB"))]

dt[, plot_ly(x = td, y = pib, type = "scatter", mode = "markers+lines", color = decada, text =~ ano) %>% 
         add_text(textfont = t, textposition = "top right") %>%
         layout(showlegend = TRUE,
                xaxis = list(title = "Tasa de desempleo"),
                yaxis = list(title = "IVF PIB"))]

t <- list(
    family = "sans serif",
    size = 8,
    color = toRGB("grey50"))
plot_ly(dt, x = ~td_dest, y = ~ind_vac_dest, text =~ ano, color =~ decada) %>%
    # add_markers() %>%
    add_trace(mode = "lines+markers") %>% 
    add_text(textfont = t, textposition = "top right") %>%
    layout(showlegend = TRUE)
plot_ly(dt[, .(td = mean(td), ind_vac = mean(ind_vac), decada = min(decada)), keyby = .(ano)], 
        x = ~td, y = ~ind_vac, text =~ ano, color =~ decada) %>%
    # add_markers() %>%
    add_trace(mode = "lines+markers", type = "scatter") %>% 
    add_text(textfont = t, textposition = "top right") %>%
    layout(showlegend = FALSE,
           xaxis = list(title = "Tasa de desempleo"),
           yaxis = list(title = "Tasa de vacantes"))

# Transformo a serie de tiempo
dt_ts <- ts(data = dt[data.table::between(fecha, "1981-01-01", "2018-10-01"), 
                      .(pib, ind_vac, td)], 
            start = c(1981, 1), frequency = 4)

# Desestacionalizo con método x13 y configuración default
td      <- seasonal::final(seasonal::seas(dt_ts[, "td"]))
ind_vac <- seasonal::final(seasonal::seas(dt_ts[, "ind_vac"]))
pib2 <- seasonal::final(seasonal::seas(dt_ts[, "pib"]))

# tasa de crecimiento del pib ~ log(diff(pib))
pib <- diff(
            ts(log(pib$pib), start = c(1980, 2), frequency = 4),
            lag = 1, differences = 1
            )
pib <- window(pib, start = c(1981, 1), end = c(2018, 4))
dt_ts <- ts.union(pib, ind_vac, td)
dt_ts2 <- ts.union(pib2, delta_pib = pib, ind_vac, td)
plot(dt_ts)
plot(dt_ts2)

lm(data = dt_ts2, formula = ind_vac ~ td + pib2 + delta_pib) %>%
    summary()
lm(data = dt_ts2, formula = ind_vac ~ td + pib2 + 1) %>%
    summary()

# gráfico
ggplot(dt, aes(y = ind_vac, x = td, color = decada)) + 
    geom_point() + 
    labs(x = "Tasa de desempleo", y = "Tasa de vacantes") +
    geom_smooth(method = "loess", formula = y ~ x) +
    theme(legend.position = "none")

p1 <- ggplot(dt, aes(y = ind_vac, x = td, color = decada)) + 
    geom_point() +
    geom_path() +
    labs(x = "Tasa de desempleo", y = "Índice de vacantes") +
    theme(legend.position = "none")

p2 <- ggplot(dt, aes(y = ind_vac, x = td, color = decada)) + 
    geom_point() +
    # geom_path() +
    # geom_text(data = dt, aes(label = ano)) +
    labs(x = "Tasa de desempleo", y = "Índice de vacantes") +
    geom_smooth(method = "lm", formula = y ~ x) +
    theme(legend.position = "none")

p3 <- dt[, .(td = mean(td), ind_vac = mean(ind_vac), pib = mean(pib), decada = min(decada)), keyby = .(ano)] %>% 
    ggplot(., aes(y = ind_vac, x = td, color = decada, label = ano)) + 
    geom_point() +
    geom_path() +
    # position=position_jitter(width=.2,height=.02)
    geom_text(fontface = "bold", size = 2, position=position_jitter(width=.1,height=.02), color = "black") +
    labs(x = "Tasa de desempleo", y = "Índice de vacantes") +
    # geom_smooth(method = "lm", formula = y ~ x) +
    theme(legend.position = "none")
library(gridExtra)
grid.arrange(p1,                             # First row with one plot spaning over 2 columns
             arrangeGrob(p2, p3, ncol = 2), # Second row with 2 plots in 2 different columns
             nrow = 2
             )


dt[, .(td = mean(td), ind_vac = mean(ind_vac), pib = mean(pib), decada = min(decada)), keyby = .(ano)
   ] %>% 
ggplot(., aes(x = ind_vac, y = pib, color = decada, label = ano)) +
    geom_point() +
    geom_path() +
    labs(x = "Índice de vacantes", y = "IVF PIB") +
    geom_text(size = 2.5, position=position_jitter(width=.02,height=.2), color = "black") +
    theme(legend.position = "none")

dt[, .(td = mean(td), ind_vac = mean(ind_vac), pib = mean(pib), decada = min(decada)), keyby = .(ano)
   ] %>% 
    ggplot(., aes(x = td, y = pib, color = decada, label = ano)) +
    geom_point() +
    geom_path() +
    labs(x = "Tasa de desempleo", y = "IVF PIB") +
    geom_text(size = 2.5, position=position_jitter(width=.02,height=.2), color = "black") +
    theme(legend.position = "none")
grid.arrange(p1, p2, ncol = 2)

# Modelo VAR n=3 ----------------------------------------------------------
# Set random seed
set.seed(1)

# Fix number of Gibbs sampler draws used in this vignette
nburn.vignette <- 5000
nrep.vignette  <- 50000

# Run estimation
# tau = 36 = 9 años. Por lo tanto le periodo queda de 1990 a 2018.
fit <- bvarsv::bvar.sv.tvp(ts.union(pib = dt_ts[,1], ind_vac = log(dt_ts[, 2]), td = log(dt_ts[,3])), 
                           p = 2, nburn = nburn.vignette, nrep = nrep.vignette, tau = 36)
saveRDS(fit, file = here::here("Datos", "Finales", "modelo.rds"), compress = FALSE)

# Estimate simple VAR using vars package
library(vars)
fit.ols <- VAR(dt_ts, p = 2)
saveRDS(fit.ols, here::here("Datos", "Finales", "modelo-var-comun.rds"), compress = FALSE)

# Replicate Figure 9 in Del Negro and Primiceri (2015)
# Some auxiliary definitions for plotting
matplot2 <- function(...) matplot(..., type = "l", lty = 1, lwd = 2,
                                  bty = "n", ylab = "")
stat.helper <- function(z) c(mean(z), quantile(z, c(0.16, 0.84)))[c(2, 1, 3)]
# xax <- seq(1981, 2018, length.out = 110) # x axis
gp <- seq(1985, 2020, 5) # marks for vertical lines
# colors, taken from http://www.cookbook-r.com
cols <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cols1 <- cols[c(2, 4, 2)]
# Residual variance from simpler benchmark model
sd.residuals.ols <- apply(residuals(fit.ols), 2, sd)
# Make plot
# sd_inf <- parameter.draws(fit, type = "vcv", row = 1, col = 1)
# x1     <- t(apply(sqrt(sd_inf), 2, stat.helper))    

par(mfrow = c(3, 1))
make_plot <- function(.fit = fit, .type = "vcv", .var = 1, .title = "") {
    gp <- seq(1990, 2020, 5) # marks for vertical lines
    if(.type == "vcv") {
        # SD of unemployment residual
        # Get posterior draws
        sd_inf <- parameter.draws(.fit, type = .type, row = .var, col = .var)
        x1     <- t(apply(sqrt(sd_inf), 2, stat.helper))    
    } else if (grepl(x = .type, pattern = "lag")) {
        beta <- parameter.draws(fit, type = .type, row = .var, col = .var)
        x1   <- t(apply(beta, 2, stat.helper))    
    } else {
        beta_0 <- parameter.draws(fit, type = .type, row = .var, col = .var)
        x1     <- t(apply(beta_0, 2, stat.helper))       
    }
    xax <- seq(1990, 2018, length.out = NROW(x1)) # x axis
    # Plot
    if(.type == "vcv") {
        var <- sd.residuals.ols[.var]
    } else {
        var <- NULL
    }
    matplot2(x = xax, y = x1, ylim = c(min(x1), max(x1)), col = cols1, main = .title , xlab = "Fecha")
    abline(h = seq(min(x1), max(x1), length.out = 10), v = gp, lty = 4, lwd = 0.3)
    if(.type == "vcv") {
        abline(h = var, col = cols[1], lwd = 1.4, lty = 5)
    }
}

make_plot(.fit = fit, .type = "vcv", .var = 1, .title = "pib")
make_plot(.fit = fit, .type = "vcv", .var = 2, .title = "vacantes")
make_plot(.fit = fit, .type = "vcv", .var = 3, .title = "desempleo")

make_plot(.fit = fit, .type = "intercept", .var = 1)
make_plot(.fit = fit, .type = "intercept", .var = 2)
make_plot(.fit = fit, .type = "intercept", .var = 3)

make_plot(.fit = fit, .type = "lag1", .var = 1)
make_plot(.fit = fit, .type = "lag1", .var = 2)
make_plot(.fit = fit, .type = "lag1", .var = 3)
# make_plot(.fit = fit, .type = "lag2", .var = 1)
# make_plot(.fit = fit, .type = "lag2", .var = 2)
# make_plot(.fit = fit, .type = "lag2", .var = 3)

# 3.2 Impulse Response Analysis
# We next estimate the response of the inflation rate to a shock in the interest rate, based on the model from
# Section 3.1. We refer to Primiceri (2005 Section 4.2) for an economic justification of the variable ordering
# chosen here.

par(mfrow = c(1, 1))
plot_irf <- function(.fit = fit, impulse, response, scenario = 2) {
    ira <- impulse.responses(fit, impulse.variable = impulse, response.variable = response, scenario = scenario)
    # OLS impulse responses for comparison
    ira.ols <- irf(fit.ols, n.ahead = 20)[[impulse]][[response]][-1, 1]
    # Add to plot
    lines(x = 1:20, y = ira.ols, lwd = 1, lty = 5, col = "red")
}
plot_irf(impulse = 1, response = 2)
plot_irf(impulse = 1, response = 3)
plot_irf(impulse = 2, response = 3)
plot_irf(impulse = 3, response = 2)

# Modelo VAR n=2 ----------------------------------------------------------
library(vars)
library(bvarsv)
set.seed(123)
nburn.vignette <- 5000
nrep.vignette  <- 50000

# Run estimation
# tau = 36 = 9 años. Por lo tanto le periodo queda de 1990 a 2018.
fit  <- bvarsv::bvar.sv.tvp(log(dt_ts[, 2:3]), p = 2, nburn = nburn.vignette, nrep = nrep.vignette, tau = 36)
# fit2 <- bvarsv::bvar.sv.tvp(diff(dt_ts[, 2:3]), p = 2, nburn = nburn.vignette, nrep = nrep.vignette, tau = 36)
saveRDS(fit, file = here::here("Datos", "Finales", "modelo2.rds"), compress = FALSE)

# Estimate simple VAR using vars package
fit.ols <- VAR(diff(dt_ts[, 2:3]), p = 2)
fit.ols <- VAR(dt_ts[, 2:3], p = 2)
saveRDS(fit.ols, here::here("Datos", "Finales", "modelo2-var-comun.rds"), compress = FALSE)

par(mfrow = c(2,1))
make_plot(.fit = fit, .type = "vcv", .var = 1, .title = "vacantes")
make_plot(.fit = fit, .type = "vcv", .var = 2, .title = "desempleo")

make_plot(.fit = fit, .type = "intercept", .var = 1, .title = "vacantes")
make_plot(.fit = fit, .type = "intercept", .var = 2, .title = "desempleo")

make_plot(.fit = fit, .type = "lag1", .var = 1, .title = "vacantes")
make_plot(.fit = fit, .type = "lag1", .var = 2, .title = "desempleo")

make_plot(.fit = fit, .type = "lag2", .var = 1, .title = "vacantes")
make_plot(.fit = fit, .type = "lag2", .var = 2, .title = "desempleo")

par(mfrow = c(1,1))
plot_irf(impulse = 1, response = 2)
plot_irf(impulse = 2, response = 1)

# Raíces Unitarias --------------------------------------------------------
# Realizamos los test de quiebre estructural para la tasa de vacantes y desempleo.
library(urca)
urca::ca.po()


# Cointegración -----------------------------------------------------------
# Relación de cointegración entre vacantes y desempleo??? Ninguna tiene tendencia, bah tal vez las vacantes si.
library(urca)
urca::ca.po(dt_ts, demean = "none") %>% 
    summary()

urca::ca.po(diff(log(dt_ts[, 2:3])), demean = "none", type = "Pz") %>% 
    summary()

urca::ca.po(diff(log(dt_ts[, 2:3]), differences = 1), demean = "none", type = "Pz") %>% 
    plot()

urca::ca.po(diff(log(dt_ts[, 2:3]), differences = 2), demean = "none", type = "Pz") %>% 
    plot()

urca::ca.jo(diff(log(dt_ts[,2:3])), ecdet = "const", type = "trace", spec = "longrun") %>% summary()

tseries::po.test(dt_ts, demean = TRUE)
library(tseries)
tseries::po.test(diff(log(dt_ts[, 3:2])), lshort = FALSE, demean = FALSE)
tseries::po.test(diff(log(dt_ts[, 3:2]), 2), lshort = FALSE, demean = FALSE)
tseries::po.test(dt_ts[, 3:2], lshort = FALSE, demean = TRUE)
tseries::po.test(dt_ts[, 3:2], lshort = TRUE, demean = TRUE)
tseries::po.test(dt_ts[, 3:2], lshort = TRUE, demean = FALSE)

tseries::po.test(dt_ts[, 2:3], lshort = TRUE, demean = FALSE)
tseries::po.test(dt_ts[, 2:3], lshort = TRUE, demean = TRUE)
tseries::po.test(dt_ts[, 2:3], lshort = FALSE, demean = FALSE)
tseries::po.test(dt_ts[, 2:3], lshort = FALSE, demean = TRUE)

tseries::po.test(diff(dt_ts[, 2:3]), lshort = TRUE, demean = TRUE) # Aha! Las tasas de variación están cointegradas.

# Lo más sensato es vacantes-pib
tseries::po.test(dt_ts[, 1:2], lshort = TRUE, demean = TRUE)
tseries::po.test(dt_ts[, c(1,3)], lshort = FALSE, demean = TRUE)

ca.jo(dt_ts[,2:3], ecdet = "const", type = "trace") %>% summary() # No se rechaza la no cointegración, OK.
ca.jo(dt_ts[,2:3], ecdet = "none", type = "trace") %>% summary() # No se rechaza la no cointegración, OK.
ca.jo(log(ts.union(1/dt_ts[, "ind_vac"], dt_ts[, "td"])), ecdet = "const", type = "trace") %>% summary() # No se rechaza la no cointegración, OK.
ca.jo(log(ts.union(1/dt_ts[, "ind_vac"], dt_ts[, "td"])), ecdet = "none", type = "trace") %>% summary() # No se rechaza la no cointegración, OK.
ca.jo((ts.union(1/dt_ts[, "ind_vac"], dt_ts[, "td"])), ecdet = "const", type = "trace") %>% summary() # No se rechaza la no cointegración, OK.
ca.jo((ts.union(1/dt_ts[, "ind_vac"], dt_ts[, "td"])), ecdet = "none", type = "trace") %>% summary() # No se rechaza la no cointegración, OK.


ca.jo(dt_ts[,3:2], ecdet = "none", type = "trace") %>% summary() # No se rechaza la no cointegración, OK.
ca.jo(dt_ts[,3:2], ecdet = "none", type = "eigen") %>% summary() # No se rechaza la no cointegración, OK.

# con el pib
ca.jo(dt_ts[,1:3], ecdet = "none", type = "trace") %>% summary() # Existe 1 relación de cointegración.
ca.jo(dt_ts[,c(1,2)], ecdet = "none", type = "trace") %>% summary() # Existe 1 relación de cointegración.
ca.jo(dt_ts[,c(1,3)], ecdet = "none", type = "trace") %>% summary() # Existe 1 relación de cointegración.

ca.jo(diff(dt_ts), ecdet = "none", type = "trace") %>% summary() # Existe 3 relación de cointegración usando delta_deta_pib What? zarpado.
ca.jo(ts.union(pib = diff(dt_ts[,1]), dt_ts[-1, 2:3]), ecdet = "none", type = "trace") %>% 
    summary() # Existe 1 relación de cointegración usando la aceleración del pib
ca.jo(dt_ts[,c(1,3)], ecdet = "none", type = "trace") %>% summary() # Existe 1 relación de cointegración.

# con delta
ca.jo(ts.union(dt_ts[-1,1], diff(log(dt_ts[, 2:3]))), ecdet = "trend", type = "trace", spec = "longrun") %>%
    summary() # Hay 3 relaciones de cointegración cuando se usan las tasas de crecimiento, tremendo.
# Asi use none, const o trend se mantiene.

ca.jo(ts.union(dt[-(c(1:4, NROW(dt):(NROW(dt)-3))), pib], dt_ts[, 2:3]), ecdet = "trend", type = "trace") %>%
    summary() # Ninguna relación de cointegración cuando se usa el pib en vez de la tasa de crecimiento del pib.

po.test(ts.union(dt[-(c(1:4, NROW(dt):(NROW(dt)-3))), pib], dt_ts[, 2])) # Lo mismo. No hay cointegración con el pib
po.test(ts.union(dt[-(c(1:4, NROW(dt):(NROW(dt)-3))), pib], dt_ts[, 3])) # Lo mismo. No hay cointegración con el pib
# Llamativo, no hay relación de cointegración con el pib. Pero SI la hay con la tasa de crecimiento del pib.

po.test(ts.union(dt_ts[-1,1], diff(log(dt_ts[, 2]))))
po.test(ts.union(dt_ts[-1,1], diff(log(dt_ts[, 3]))))

# plot
plot(ts.union(dt_ts[,1], diff(log(dt_ts[, 2:3]))), plot.type = "single", col = 1:3)
legend("topleft", c("pib", "vacantes", "desempleo"), bty = "n", col = 1:3, lty = rep(1,3))

dt[, plot(x = pib, y = ind_vac, type = "l")]
dt[, plot(x = pib, y = td, type = "l")]
plot(dt_ts[, 1], dt_ts[,2])
plot(dt_ts[, 1], dt_ts[,3])
plot(dt_ts[, 3], dt_ts[,2])


# Análisis quiebres ---------------------------------------------------
library(strucchange)
library(fxregime)

### Notar las densidades de vacantes y desempleo.
density(dt_ts[, 2]) %>% plot()
density(dt_ts[, 3]) %>% plot()
# Va a ser necesario aplicar una transformación, ej, log.
density(log(dt_ts[, 3])) %>% plot()
density(log(dt_ts[, 2])) %>% plot()
# Pero solamente para la tasa de desempleo.

# PASO A. Generalized fluctuation tests
# Paso 1. Empirical fluctuation processes: function efp
# 1. CUSUM PROCESS.
#       -Paper del 74, Brown, Durbin, Evans.
#       -OLS-CUSUM type
# 2. MOSUM processes
#       -Recursive
#       -OLS
# Estos (1 y 2) usan el test S_r
#
# 3. Estimates-based processes
#       -Ploberger, Kr¨amer, and Kontrus (1989), type = fluctuation (recursive)
#       -moving estimates (ME) process
#       Usando el test S_e
# 4. F-test.

# Defino la formula, como no hay relación de cointegración las mando en niveles. Son I(1) pero sin crecimiento
library(strucchange)
lag_dt <- window(ts.union(ind_vac = dt_ts[, 2], td = dt_ts[, 3], ind_vac_1 = lag(dt_ts[, 2], -1)), start = c(1981, 2), end = c(2018,4))
reg  <- log(ind_vac) ~ log(td) + 1
reg2  <- log(ind_vac) ~ log(td) + log(ind_vac_1) - 1
mod1 <- strucchange::efp(formula = reg, type = "Score-CUSUM", data = dt_ts[, 2:3], h = .15, dynamic = F)
plot(mod1, functional = NULL)
print(sctest(mod1)) # Quiebre.
test_plot <- function(test, .data = dt_ts[, 2:3], .formula = reg, .h = 0.15, .dynamic = FALSE, .test = TRUE, 
                      .main = "", .ylab = "") {
    mod1 <- strucchange::efp(formula = .formula, type = test, data = .data, h = .h, dynamic = .dynamic)
    # Boundaries
    if(test == "Score-CUSUM"| test == "Score-MOSUM") {
      colnames(mod1$process) <- c("Intercepto", "log(td)", "Varianza")
    } else if (test == "fluctuation") {
      colnames(mod1$process) <- c("Intercepto", "log(td)")
    }
    print(plot(mod1, functional = NULL, main = .main, ylab = .ylab, xlab = "Fecha"))
    # print(plot(mod1, functional = NULL, xlab = "Fecha"))
    # boundary(mod1, alpha = 0.05)
    # Test
    if(.test) {
      print(sctest(mod1))  
    }
}
# y = Proceso de fluctuación empírico
# main =  Test ...
# 1981.1

# 1ro. CUSUM en base al paper del 74.
test_plot(test = "Rec-CUSUM", .main = "", .ylab = "Proceso fluctuación empírica") # Quiebre. Test CUSUM recursivo
test_plot(test = "Rec-CUSUM", .data = lag_dt, .formula = reg2) # QUIEBRE
# AL USAR LOS DATOS EN LOG, SI HAY QUIEBRE!
# 2do. CUSUM-OLS
test_plot(test = "OLS-CUSUM") # Quiebre. Test CUSUM OLS
# 2do y 1/2. Score-CUSUM
test_plot(test = "Score-CUSUM") # Quiebre. Test CUSUM basado en score

# Repito bajo supuestos más debiles con dynamic, agregando rezagos
test_plot(test = "Rec-CUSUM", .dynamic = TRUE) # QUIEBRE
test_plot(test = "Rec-CUSUM", .dynamic = TRUE, .formula = log(ind_vac) ~ log(td))     # QUIEBRE
test_plot(test = "Rec-CUSUM", .dynamic = TRUE, .formula = log(ind_vac) ~ log(td) - 1) # QUIEBRE
# Pero porque se corrio con constante y autoregresivo, no tiene sentido la constante ahí.
# quiebre siempre en log o sin log.

test_plot(test = "OLS-CUSUM", .dynamic = TRUE, .formula = ind_vac ~ log(td) - 1)       # NO se rechaza la nula
test_plot(test = "OLS-CUSUM", .dynamic = TRUE, .formula = ind_vac ~ log(td) + 1)       # NO se rechaza la nula
test_plot(test = "OLS-CUSUM", .dynamic = TRUE, .formula = log(ind_vac) ~ log(td) - 1)  # NO se rechaza la nula
test_plot(test = "OLS-CUSUM", .dynamic = TRUE, .formula = log(ind_vac) ~ log(td) + 1)  # NO se rechaza la nula
test_plot(test = "OLS-CUSUM", .dynamic = FALSE, .formula = reg2, .data = lag_dt)       # NO QUIEBRE (Es lo mismo que con dynamic TRUE)
# Único test que rechaza la nula.

# 3ro. MOSUM-Recursive
test_plot(test = "Rec-MOSUM")                         # Quiebre.
test_plot(test = "Rec-MOSUM", .data = lag_dt, .formula = reg2, .dynamic = F)   # Quiebre.
# 4to. MOSUM-OLS
test_plot(test = "OLS-MOSUM")                         # Quiebre.
test_plot(test = "OLS-MOSUM", .data = lag_dt, reg2)   # Quiebre.
# 4to y 1/2. Score-MOSUM
test_plot(test = "Score-MOSUM")                       # Quiebre
test_plot(test = "Score-MOSUM", .data = lag_dt, reg2) # Quiebre
# 5to. Recursive
test_plot(test = "fluctuation")                       # Quiebre
test_plot(test = "fluctuation", .data = lag_dt, reg2) # NO Quiebre
test_plot(test = "fluctuation", .data = lag_dt, log(ind_vac) ~ log(td) + log(ind_vac_1) + 1) # NO Quiebre
# 6to. ME
test_plot(test = "ME")                                # Quiebre.
test_plot(test = "ME", .data = lag_dt, reg2)          # NO Quiebre
test_plot(test = "ME", .data = lag_dt, log(ind_vac) ~ log(td) + log(ind_vac_1) + 1) # NO Quiebre
# Los test con dynamic (o lag_dt) no tiene sentido ponerle intercepto, un proceso

# 7mo. F-test
ftest_plot <- function(.formula = reg, .data = dt_ts[, 2:3], .from = 0.15, .to = NULL, .alpha = 0.05, bp = FALSE,
                       .vcov = "NeweyWest", HAC = TRUE) {
    if(HAC) {
        mod <- strucchange::Fstats(formula = .formula, data = .data, from = .from, to = .to,
                                   vcov = function(x, ...) sandwich::vcovHAC(x))    
    } else {
        mod <- strucchange::Fstats(formula = .formula, data = .data, from = .from, to = .to,
                                   vcov = sandwich::NeweyWest)    
    }
    # function(x, ...) vcovHC(x, type = "HC0", ...)
    print(plot(mod, .alpha, aveF = TRUE))
    # Test
    print(sctest(mod, type = "expF"))
    print(sctest(mod, type = "aveF"))
    print(sctest(mod, type = "supF"))
    if(bp) {
        ## visualize the breakpoint implied by the argmax of the F statistics
        plot(.data)
        lines(breakpoints(mod, breaks = 5))
    }
}
ftest_plot(.formula = reg,  .from = 0.2, bp = T, HAC = T)                 # QUIEBRE supF
ftest_plot(.formula = reg2, .from = 0.2, bp = T, HAC = F, .data = lag_dt) # NO QUIEBRE.
ftest_plot(.formula = reg2, .from = 0.2, bp = T, HAC = T, .data = lag_dt) # NO QUIEBRE, supF SI QUIEBRE
ftest_plot(.formula = reg,  .from = 0.2, bp = T, HAC = F)                 # QUIEBRE TODAS
ftest_plot(.formula = reg2, 
           .data = lag_dt, 
           .from = 0.2, bp = T, HAC = F)
ftest_plot(.formula = reg, .from = c(1997, 1), 
           .to = c(2010, 4), bp = T, HAC = T)
ftest_plot(.formula = reg, .from = c(1986, 4), 
           .to = c(2010, 1), bp = T, HAC = F)
breakpoints(reg, data = dt_ts[,2:3], breaks = 5)
breakpoints(reg, data = dt_ts[,2:3], breaks = 4)
breakpoints(reg, data = dt_ts[,2:3], breaks = 3)
breakpoints(reg, data = dt_ts[,2:3], breaks = 2)

breakpoints(reg2, data = lag_dt, breaks = 5)
breakpoints(reg2, data = dt_ts[, 2:3], breaks = 5)

breakpoints(formula = log(ind_vac) ~ log(td) + log(ind_vac_1) - 1, data = lag_dt, breaks = 2)
breakpoints(formula = ind_vac ~ td + ind_vac_1, data = lag_dt, breaks = 5)
breakpoints(formula = log(ind_vac) ~ log(td) - 1, data = lag_dt, breaks = 5)
breakpoints(formula = log(ind_vac) ~ log(td) + 1, data = lag_dt, breaks = 5)
breakpoints(formula = log(ind_vac) ~ log(td) + 1, data = dt_ts[, 2:3], breaks = 5)

# Resultados contradictorios dependiendo de la especificación.
# El supremo rechaza la nula, el aveF y expF no la rechazan.
# El error al bajar el .from = 0.1
# https://stackoverflow.com/questions/38961221/uniroot-solution-in-r


# Análisis siguiendo paper ------------------------------------------------
# Exchange Rate Regime Analysis for the Chinese
# Se complementa completamente con lo analizado anteriormente. De hecho, se podría unir.
# https://stackoverflow.com/questions/29591693/r-strucchange-bootstrap-test-statistic-due-to-nonspherical-disturbances

# Corremos un modelo lineal
library(fxregime)
mod <- lm(formula = reg, data = dt_ts)
summary(mod)

# Empirical fluctuation process
mod_efp <- gefp(mod, fit = NULL)
plot(mod_efp, aggregate = FALSE, ylim = c(-4, 4))

# testeamos
sctest(mod_efp) # Resulta ser totalmente significativo.

# Buscamos los quiebres cada 5 años
mod_reg <- fxregimes(formula = reg, data = zoo(dt_ts), h = 20, breaks = 5)
plot(mod_reg) # 3 quiebres, según LWZ. 5 quiebres según negative log likelihood. Se elige LWZ en el paquete y e
# el paper, siguiend a Bai y Perron. Osea 3 quiebres, 4 periodos.
summary(mod_reg)
confint(mod_reg, level = 0.9)

# Parámetros estimados para cada segmento
coef(mod_reg)

# Resúmen completo, primero re-estimar el modelo en los subperiodos y luego aplicando summary
mod_rf <- refit(mod_reg)
lapply(mod_rf, summary)
####

######################## Volvemos a correr el modelo en niveles
mod <- lm(formula = (ind_vac) ~ (td) + 1, data = dt_ts)
summary(mod)
# Empirical fluctuation process
mod_efp <- gefp(mod, fit = NULL)
plot(mod_efp, aggregate = FALSE, ylim = c(-4, 4))
# testeamos
sctest(mod_efp) # Resulta ser totalmente significativo.
# Buscamos los quiebres cada 5 años
mod_reg <- fxregimes(formula = (ind_vac) ~ (td) + 1, data = zoo(dt_ts), h = 20, breaks = 5)
plot(mod_reg) # 3 quiebres, según LWZ. 5 quiebres según negative log likelihood. Se elige LWZ en el paquete y e
# el paper, siguiend a Bai y Perron. Osea 3 quiebres, 4 periodos.
summary(mod_reg)
confint(mod_reg, level = 0.9)
# Parámetros estimados para cada segmento
coef(mod_reg)
# Resúmen completo, primero re-estimar el modelo en los subperiodos y luego aplicando summary
mod_rf <- refit(mod_reg)
lapply(mod_rf, summary)
################## CASI los mismos resultados de quiebres

################## Ahora agregamos rezagos.
mod <- lm(formula = reg2, data = lag_dt)
summary(mod) # En logs, Al plantearlo como un autoregresivo deja de ser significativo el desempleo.
mod <- lm(formula = ind_vac ~ td + ind_vac_1 - 1, data = lag_dt)
summary(mod) # Mismo resultado en niveles-
# Probando gefp
mod <- gefp(reg, fit = lm, vcov = sandwich::kernHAC, data = dt_ts)
plot(mod)
sctest(mod)   # No hay quiebre.
bp <- breakpoints(reg, data = dt_ts)
confint(bp)


# Empirical fluctuation process
mod_efp <- gefp(mod, fit = NULL)
plot(mod_efp, aggregate = FALSE, ylim = c(-4, 4)) # Con este modelo no parece haber ningún problema.

# testeamos
sctest(mod_efp) # Resulta ser totalmente NO significativo.

# Buscamos los quiebres cada 5 años
mod_reg <- fxregimes(formula = ind_vac ~ td + diff_ind_vac + diff_td - 1, 
                     data = zoo(union), h = 10, breaks = 5)
plot(mod_reg) # Wow, problema. Resultado totalmente contrapuesto. No hay quiebres según el LWZ, si lo hay según
# negative-log-like.
summary(mod_reg)
confint(mod_reg, level = 0.9)

# Parámetros estimados para cada segmento
coef(mod_reg)

# Resúmen completo, primero re-estimar el modelo en los subperiodos y luego aplicando summary
mod_rf <- refit(mod_reg)
lapply(mod_rf, summary)

# El problema es que al agregar las vacantes rezagadas no hay quiebre, no agregarlas. Es correcto?
mod <- lm(formula = ind_vac ~ + diff_td + td + 1, data = union)
summary(mod) # La diferencia del desempleo NO es signitificativa. Y lo es sin constante?
mod <- lm(formula = ind_vac ~ td + diff_td +1, data = union) # Tampoco.
# O sea que estamos en el caso inicial, vacantes ~ desempleo.
# última prueba es diff_td en vez de td
mod <- lm(formula = ind_vac ~ diff_td +1, data = union) # Tampoco.
summary(mod) # Vamos a probar.

# Empirical fluctuation process
mod_efp <- gefp(mod, fit = NULL)
plot(mod_efp, aggregate = FALSE, ylim = c(-4, 4)) # Con este modelo no parece haber ningún problema.

# testeamos
sctest(mod_efp) # Resulta ser totalmente significativo.

# Buscamos los quiebres cada 5 años
mod_reg <- fxregimes(formula = ind_vac ~ + diff_td + 1, 
                     data = zoo(union), h = 10, breaks = 5)
plot(mod_reg) # Ahora hay 4 quiebres, ie, 5 periodos. Notar como repiten 96 y 2013.
summary(mod_reg)
confint(mod_reg, level = 0.9)

# Parámetros estimados para cada segmento
coef(mod_reg)

# Resúmen completo, primero re-estimar el modelo en los subperiodos y luego aplicando summary
mod_rf <- refit(mod_reg)
lapply(mod_rf, summary)



# Quiebres -------------------------------------------------------------
dif_vac <- window(ts.union(v =diff(dt_ts[,2]), v_1 = lag(diff(dt_ts[, 2]), -1)),
                  start = c(1981, 3), end = c(2018, 4))
lm(data = dif_vac, v ~ v_1 - 1) %>% summary()
reg <- v ~ v_1 - 1
# 1ro. CUSUM en base al paper del 74.
test_plot(test = "Rec-CUSUM", .data = dif_vac) # No hay quiebre.
temp = ts.union(a = dt_ts[,2], b = dt_ts[,3])
test_plot(test = "Rec-CUSUM", .data = temp, .formula = a ~ b, .dynamic = TRUE) # No hay quiebre.
# 2do. CUSUM-OLS
test_plot(test = "OLS-CUSUM", .data = dif_vac) # No hay Quiebre
# 2do y 1/2. Score-CUSUM
test_plot(test = "Score-CUSUM", .data = dif_vac) # Si hay quiebre.
# Repito bajo supuestos más debiles con dynamic, agregando rezagos
test_plot(test = "Rec-CUSUM", .dynamic = F, .data = dif_vac)
test_plot(test = "Rec-CUSUM", .dynamic = TRUE, .data = dt_ts[, 2], .formula = ind_vac ~ -1) 
test_plot(test = "OLS-CUSUM", .dynamic = TRUE) 
# Resultados totalmente diferentes! Según esto no habría cambio estructural.
# 3ro. MOSUM-Recursive
test_plot(test = "Rec-MOSUM") # Quiebre.
# 4to. MOSUM-OLS
test_plot(test = "OLS-MOSUM") # Quiebre.
# 4to y 1/2. Score-MOSUM
test_plot(test = "Score-MOSUM") # Quiebre
# 5to. Recursive
test_plot(test = "fluctuation") # Quiebre
# 6to. ME
test_plot(test = "ME") # Quiebre.
# 7mo. F-test
ftest_plot(.formula = reg, .from = 0.2, bp = F)
reg = log(ind_vac+1) ~ log(td) + 1
ftest_plot(.formula = reg, .from = 0.2, bp = F)
# Resultados contradictorios.
# El supremo rechaza la nula, el aveF y expF no la rechazan.
# El error al bajar el .from = 0.1
# https://stackoverflow.com/questions/38961221/uniroot-solution-in-r



# Quiebres ------------------------------------------------------------
reg <- log(ind_vac) ~ log(td) + 1
ft <- function(test = "supF", .formula = reg, .data = dt_ts[, 2:3], .from = 0.15, .to = NULL, pval = F) {
    mod <- strucchange::Fstats(formula = .formula, data = .data, from = .from, to = .to,
                               vcov = sandwich::kernHAC)
    # function(x, ...) vcovHC(x, type = "HC", ...)
    if(pval) {
        round(sctest(mod, type = test)$p.value[[1]], 2)
    } else {
        round(sctest(mod, type = test)$statistic[[1]], 2)
    }
}
rt <- function(test, .data = dt_ts[, 2:3], .formula = reg, .h = 0.15, .dynamic = FALSE, pval = TRUE) {
    # Modelo
    mod1 <- strucchange::efp(formula = .formula, type = test, data = .data, h = .h, 
                             dynamic = .dynamic, vcov = sandwich::kernHAC)
    # Test
    if(pval) {
        round(sctest(mod1)$p.value[[1]],2)
    } else {
        round(sctest(mod1)$statistic[[1]],2)
    }
}

test = c("Rec-CUSUM", "OLS-CUSUM", "Score-CUSUM", "Rec-CUSUM(d)", "OLS-CUSUM(d)" ,"Score-CUSUM(d)", "Rec-MOSUM", 
         "OLS-MOSUM", "Score-MOSUM", "fluctuation", "ME", "expF", "aveF", "supF")
mat = matrix(data = NA, nrow = NROW(test), ncol = 4)
colnames(mat) <- c("Test", "est", "p-valor", "resultado")
mat <- as.data.frame(mat)
i = 0
for(t in test) {
    i = i + 1
    mat[i , 1] <- t
        for(bool in c(FALSE, TRUE)) {
            if(bool) {
                if(t %in% c("Rec-CUSUM(d)", "OLS-CUSUM(d)" ,"Score-CUSUM(d)")) {
                    mat[i, "p-valor"] <- rt(test = gsub(t, pattern = "\\(d\\)", replacement = ""), pval = bool, .dynamic = T)
                } else if (t %in% c("expF", "aveF", "supF")) {
                    mat[i, "p-valor"] <- ft(test = t, pval = bool)
                } else {
                    mat[i, "p-valor"] <- rt(test = t, pval = bool, .dynamic = F)
                }
            } else {
                if(t %in% c("Rec-CUSUM(d)", "OLS-CUSUM(d)" ,"Score-CUSUM(d)")) {
                    mat[i, "est"] <- rt(test = gsub(t, pattern = "\\(d\\)", replacement = ""), pval = bool, .dynamic = T)
                } else if (t %in% c("expF", "aveF", "supF")) {
                    mat[i, "est"] <- ft(test = t, pval = bool)
                } else {
                    mat[i, "est"] <- rt(test = t, pval = bool)
                }   
            }
        }
        if(mat[i, "p-valor"] > 0.05) {
            mat[i, "resultado"] <- "No rechaza"
        } else {
            mat[i, "resultado"] <- "Rechaza"
        }
    mat[, 2:3] <- sapply(mat[,2:3], as.numeric)
}
mat

ftest_plot <- function(.formula = reg, .data = dt_ts[, 2:3], .from = 0.15, .to = NULL, .alpha = 0.05, bp = FALSE) {
    mod <- strucchange::Fstats(formula = .formula, data = .data, from = .from, to = .to,
                               vcov = sandwich::kernHAC)
    print(plot(mod, .alpha, aveF = TRUE))
    # Test
    print(sctest(mod, type = "expF"))
    print(sctest(mod, type = "aveF"))
    print(sctest(mod, type = "supF"))
    if(bp) {
        ## visualize the breakpoint implied by the argmax of the F statistics
        plot(.data)
        lines(breakpoints(mod))
    }
}
ftest_plot(.formula = reg, .from = 0.2, bp = T)
ft(.formula = reg, .from = 0.2, pval = F, test = "expF")

# Breakpoints
reg <- log(ind_vac) ~ log(td) + 1
reg2 <- log(td) ~ log(ind_vac) + 1
mod1 <- strucchange::efp(formula = reg, type = "Score-CUSUM", data = dt_ts[, 2:3], h = .15, 
                         dynamic = FALSE, vcov = sandwich::kernHAC)
plot(mod1, functional = NULL)
print(sctest(mod1)) # Quiebre.
coef(mod1)
mod1$Q12

breakpoints(mod1)
break_mod <- breakpoints(reg2, data = dt_ts, breaks = 4)
plot(break_mod)
breakpoints(break_mod)
summary(break_mod)
breakdates(break_mod)
refit(break_mod)
refit_break <- function(object, breaks = NULL, ...) {
    if(is.null(breaks)) breaks <- time(object$data)[object$breakpoints]
    else if(length(breaks) == 1 & is.numeric(breaks) & !inherits(breaks, "Date"))
        breaks <- breakdates(object, breaks = breaks)
    breaks <- breaks[!is.na(breaks)]
    
    if(length(breaks) < 1) {
        sbp <- start(object$data)
        ebp <- end(object$data)
    } else {  
        sbp <- c(start(object$data), sapply(breaks, function(z)
            index(object$data)[min(which(index(object$data) > z))]))
        ebp <- c(breaks, end(object$data))
    }
    
    rval <- lapply(1:length(sbp), function(i)
        fxlm(object$formula, data = window(object$data, start = sbp[i], end = ebp[i]), ...))
    names(rval) <- paste(as.character(sbp), as.character(ebp), sep = "--")
    return(rval)  
}
refit_break(break_mod, breaks = 4)

mod <- strucchange::Fstats(formula = reg, data = dt_ts[,2:3], from = .15, to = NULL
                           ,vcov. = NeweyWest
                           )
plot(mod)
breakpoints(mod, breaks = 4) # 1 en 1990.
summary(breakpoints(mod))

# Test
print(sctest(mod, type = "expF"))
print(sctest(mod, type = "aveF"))
plot(mod)
print(sctest(mod, type = "supF"))
## visualize the breakpoint implied by the argmax of the F statistics
breakpoints(mod)
breakpoints(mod)
confint(breakpoints(mod), breaks = 2, vcov = NeweyWest)
library(lmtest)
coeftest(mod, vcov = NeweyWest)
coeftest(mod, vcov = vcovHC)
coeftest(mod, df = Inf)
coeftest(mod, df = Inf, vcov = vcovHC, type = "HC3")


# FXREGIME 
reg <- log(ind_vac) ~ log(td) + 1
reg2 <- ind_vac ~ td + 1
mod <- lm(formula = reg, data = dt_ts)
summary(mod)
# Empirical fluctuation process
mod_efp <- gefp(mod, fit = NULL)
plot(mod_efp, aggregate = FALSE, ylim = c(-4, 4))
# testeamos
sctest(mod_efp) # Resulta ser totalmente significativo.
# Buscamos los quiebres cada 5 años
mod_reg <- fxregimes(formula = reg, data = zoo(dt_ts, frequency = 4), h = 20, 
                     breaks = 5)
# The computing engine behind fxregime is gbreakpoints that generalizes various aspects about breakpoints
plot(mod_reg) # 3 quiebres, según LWZ. 5 quiebres según negative log likelihood. Se elige LWZ en el paquete y e
# el paper, siguiend a Bai y Perron. Osea 3 quiebres, 4 periodos.
summary(mod_reg)
breakdates(mod_reg)
confint(mod_reg, level = 0.95, vcov = kernHAC)
# Parámetros estimados para cada segmento
coef(mod_reg)
# Resúmen completo, primero re-estimar el modelo en los subperiodos y luego aplicando summary
mod_rf <- refit(mod_reg)
lapply(mod_rf, summary)
data.table::rbindlist(lapply(mod_rf, function(x) {
    broom::tidy(summary(x, digits = 1))
}), use.names = T, idcol = "modelo")
get_model <- function(.mod, .mat = sandwich::vcovHAC) {
    a = data.table::rbindlist(
        lapply(.mod, function(x) {
        broom::tidy(
            lmtest::coeftest(x, .mat)
            )
    }), 
    use.names = TRUE, idcol = "modelo")
    setnames(a, old = names(a), 
            new = c("Modelo", "Término", "Estimación", "Estándar error", 
                    "Estadístico", "p-valor"))
    a[`p-valor` > 0.1, sigf := ""]
    a[between(`p-valor`, lower = 0.05, 0.1),   sigf := "."]
    a[between(`p-valor`, lower = 0.01, 0.05),  sigf := "*"]
    a[between(`p-valor`, lower = 0.001, 0.01), sigf := "**"]
    a[between(`p-valor`, lower = 0, 0.001),    sigf := "***"]
    print(a, digits = 2)
}
get_model(.mod = mod_rf, .mat = NULL)
get_model(.mod = mod_rf, .mat = sandwich::vcovHAC) # Pesos default, weightsAndrews
get_model(.mod = mod_rf, .mat = function(x) vcovHAC(x, weights = weightsLumley))
get_model(.mod = mod_rf, .mat = kernHAC)
get_model(.mod = mod_rf, .mat = function(x) kernHAC(x, kernel = "Parzen", prewhite = 2, adjust = FALSE,
                                                    bw = bwNeweyWest, verbose = FALSE))
get_model(.mod = mod_rf, .mat = NeweyWest)
# lapply(mod_rf, coeftest, sandwich::vcovHAC) # Pesos default, weightsAndrews
# lapply(mod_rf, coeftest, sandwich::vcovHAC, weights = weightsLumley)
lapply(mod_rf, coeftest, sandwich::kernHAC)
lapply(mod_rf, function(x) {
  coeftest(x, .vcov = function(x) kernHAC(x, kernel = "Parzen", prewhite = 2, adjust = FALSE,
                      bw = bwNeweyWest, verbose = TRUE))  
})
# Quiebres con log y sin log.
get_breaks <- function(.formula = reg, .data = dt_ts) {
    mod <- fxregimes(formula = .formula, data = zoo(.data, frequency = 4), h = 20, 
                     breaks = 5)
    print(coef(mod))
    cat("\n")
    strucchange::breakdates(mod)
}
get_breaks(reg)
get_breaks(reg2)
# Mismos periodos

# Reestimar el periodo no significativo sin constante
mod <- lm(log(ind_vac) ~ log(td) + 1, 
          data = window(dt_ts, start = c(1990,3), end = c(1996, 2)))
coeftest(mod, kernHAC)
coeftest(mod, sandwich::vcovHAC)


#### Usando fxlm
fm <- fxlm(reg, data = dt_ts)  # Es lo mismo que correr lm(..)
coef(fm)
summary(fm)
## test parameter stability (with double max test)
scus <- gefp(fm, fit = NULL)
plot(scus, aggregate = FALSE)
## alternative tests: Andrews' supLM ...
plot(scus, functional = supLM(0.1))
## ... or Nyblom-Hansen test (Cramer-von Mises type test)
plot(scus, functional = meanL2BB)


# Usando solo breakpoints
mod <- breakpoints(reg, data = dt_ts, h = 0.15)
summary(mod)
lines(mod, breaks = 2)
plot(mod)
breakpoints(mod, breaks = 4)
logLik(mod)
for(i in 1:5) {temp <- breakpoints(mod, breaks = i); v[i] <- logLik(temp)}
plot(1:5, v, type = "b")

mod2 <- breakpoints(mod, breaks = 3)
fm0 = lm(reg, data = dt_ts)
fm1 = lm(ind_vac ~ breakfactor(mod2)/(td) - 1, data = dt_ts)
plot(dt_ts[, 2])
time <- as.vector(time(dt_ts))
lines(time, fitted(fm0), col = 3)
lines(time, fitted(fm1), col = 4)
lines(mod2)

ci2 <- confint(mod, breaks = 3, vcov. = vcovHAC)
ci2
lines(ci2)
breakdates(mod2)


# Resumen -----------------------------------------------------------------
# Tengo un proceso ar(1), que quiere regresar contra la tasa de desempleo y calcular los quiebres.
# Opciones, revisar y discutir.
# Estimar el modelo en niveles o en logaritmos
# ind_vac ~ 1 + td
# Usar matriz de varianzas y covarianzas robustas a la heteroscedasticidad y autocorrelación. 
# HAC ó kernHAC ó NeweyWest
