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

# tasa de crecimiento del pib ~ log(diff(pib))
pib <- diff(
            ts(log(pib$pib), start = c(1980, 2), frequency = 4),
            lag = 1, differences = 1
            )
pib <- window(pib, start = c(1981, 1), end = c(2018, 4))
dt_ts <- ts.union(pib, ind_vac, td)
plot(dt_ts)


# Modelo VAR n=3 ----------------------------------------------------------
# Set random seed
set.seed(1)

# Fix number of Gibbs sampler draws used in this vignette
nburn.vignette <- 5000
nrep.vignette  <- 50000

# Run estimation
# tau = 36 = 9 años. Por lo tanto le periodo queda de 1990 a 2018.
fit <- bvarsv::bvar.sv.tvp(dt_ts, p = 1, nburn = nburn.vignette, nrep = nrep.vignette, tau = 36)
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
    matplot2(x = xax, y = x1, ylim = c(min(x1, var), max(x1, var)), col = cols1, main = .title , xlab = "Fecha")
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
set.seed(123)
nburn.vignette <- 5000
nrep.vignette  <- 50000

# Run estimation
# tau = 36 = 9 años. Por lo tanto le periodo queda de 1990 a 2018.
fit <- bvarsv::bvar.sv.tvp(dt_ts[, 2:3], p = 2, nburn = nburn.vignette, nrep = nrep.vignette, tau = 36)
saveRDS(fit, file = here::here("Datos", "Finales", "modelo2.rds"), compress = FALSE)

# Estimate simple VAR using vars package
fit.ols <- VAR(dt_ts[, 2:3], p = 2)
saveRDS(fit.ols, here::here("Datos", "Finales", "modelo2-var-comun.rds"), compress = FALSE)



# Raíces Unitarias --------------------------------------------------------
library(urca)
urca::ca.po()


# Cointegración -----------------------------------------------------------
# Relación de cointegración entre vacantes y desempleo??? Ninguna tiene tendencia, bah tal vez las vacantes si.
urca::ca.po(dt_ts, demean = "none") %>% 
    summary()

urca::ca.po(diff(log(dt_ts[, 2:3])), demean = "none", type = "Pz") %>% 
    summary()

urca::ca.po(diff(log(dt_ts[, 2:3]), differences = 1), demean = "none", type = "Pz") %>% 
    plot()

urca::ca.po(diff(log(dt_ts[, 2:3]), differences = 2), demean = "none", type = "Pz") %>% 
    plot()

ca.jo(diff(log(dt_ts[,2:3])), ecdet = "const", type = "trace", spec = "longrun") %>% summary()

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

library(urca)
ca.jo(dt_ts[,2:3], ecdet = "const", type = "trace") %>% summary() # No se rechaza la no cointegración, OK.
ca.jo(dt_ts[,2:3], ecdet = "none", type = "trace") %>% summary() # No se rechaza la no cointegración, OK.
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


# Quiebres estructurales --------------------------------------------------
library(strucchange)
dt_ts
