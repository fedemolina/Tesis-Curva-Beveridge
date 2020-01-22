# Estimación Curva de Beveridge -------------------------------------------
paquetes <- c("ggplot2", "forecast", "plotly", "data.table", "bvarsv") # También esta bvars
sapply(paquetes, require, character.only = TRUE)
td <- readRDS("./Datos/Finales/Tasas-Montevideo.rds")
dt <- readRDS("./Datos/Finales/series_version_final.rds")

cols <- c("fecha", "av_final_tc", "ano", "mes", "q", "pea")
cols_clean <- names(dt)[!names(dt) %in% cols]
dt[, (cols_clean) := NULL]

setkey(dt, "fecha")
setkey(td, "fecha")
# cols <- c("ta", "te", "td")
cols <- "td"
dt[td, (cols) := mget(cols)]
dt <- dt[complete.cases(dt), ]

# Índice de vacantes (a_t/a_0)/(pea_t/pea_0)
# Que año base? julio del 98? ene 2010?
base = dt[fecha == "2010-01-01", av_final_tc]
base_pea <- dt[fecha == "2010-01-01", pea]

dt[, ind_vac := (av_final_tc/base)/(pea/base_pea)]
dt[, plot_ly(x = fecha, y = ind_vac, type = "scatter", mode = "marker+lines")]

# Genero por intervalo
dt[between(fecha, "1981-01-01", "1989-10-01"), decada := "80"
   ][between(fecha, "1990-01-01", "1999-10-01"), decada := "90"
     ][between(fecha, "2000-01-01", "2009-10-01"), decada := "2000"
       ][between(fecha, "2010-01-01", "2018-10-01"), decada := "2010"]
dt[, plot_ly(x = td, y = ind_vac, type = "scatter", mode = "marker+lines", color = decada)]
# Puta madre, hermoso!!





# Estimación quiebres estructurales
# Set random seed
set.seed(1)
# Load data
data(usmacro)
# Fix number of Gibbs sampler draws used in this vignette
nburn.vignette <- 5000
nrep.vignette <- 50000
# Run estimation
fit <- bvar.sv.tvp(usmacro, p = 2, nburn = nburn.vignette, nrep = nrep.vignette)

# Estimate simple VAR using vars package
library(vars)
fit.ols <- VAR(usmacro, p = 2)

# Replicate Figure 9 in Del Negro and Primiceri (2015)
# Some auxiliary definitions for plotting
matplot2 <- function(...) matplot(..., type = "l", lty = 1, lwd = 2,
                                  bty = "n", ylab = "")
stat.helper <- function(z) c(mean(z), quantile(z, c(0.16, 0.84)))[c(2, 1, 3)]
xax <- seq(1963.5, 2001.5, 0.25) # x axis

gp <- seq(1965, 2000, 5) # marks for vertical lines
# colors, taken from http://www.cookbook-r.com
cols <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
          "#D55E00", "#CC79A7")
cols1 <- cols[c(2, 4, 2)]
# Residual variance from simpler benchmark model
sd.residuals.ols <- apply(residuals(fit.ols), 2, sd)
# Make plot
par(mfrow = c(3, 1))
# SD of inflation residual
# Get posterior draws
sd_inf <- parameter.draws(fit, type = "vcv", row = 1, col = 1)
x1 <- t(apply(sqrt(sd_inf), 2, stat.helper))
# Plot
matplot2(x = xax, y = x1, ylim = c(0, 0.8), col = cols1,
         main = "Inflation", xlab = "Time")
abline(h = c(0.2, 0.4, 0.6), v = gp, lty = 4, lwd = 0.3)
abline(h = sd.residuals.ols[1], col = cols[1], lwd = 1.4)
# SD of unemployment residual
# Get posterior draws
sd_une <- parameter.draws(fit, type = "vcv", row = 2, col = 2)
x2 <- t(apply(sqrt(sd_une), 2, stat.helper))
# Plot
matplot2(x = xax, y = x2, ylim = c(0, 1), col = cols1,
         main = "Unemployment", xlab = "Time")
abline(h = c(0.5, 1), v = gp, lty = 4, lwd = 0.3)
abline(h = sd.residuals.ols[2], col = cols[1], lwd = 1.4)
# SD of interest rate residual
# Get posterior draws
sd_tbi <- parameter.draws(fit, type = "vcv", row = 3, col = 3)
x3 <- t(apply(sqrt(sd_tbi), 2, stat.helper))
# Plot
matplot2(x = xax, y = x3, ylim = c(0, 5), col = cols1,
         main = "Interest rate", xlab = "Time")
abline(h = 1:4, v = gp, lty = 4, lwd = 0.3)
abline(h = sd.residuals.ols[3], col = cols[1], lwd = 1.4)

# 3.2 Impulse Response Analysis
# We next estimate the response of the inflation rate to a shock in the interest rate, based on the model from
# Section 3.1. We refer to Primiceri (2005 Section 4.2) for an economic justification of the variable ordering
# chosen here.
par(mfrow = c(1, 1))
ira <- impulse.responses(fit, impulse.variable = 3, response.variable = 1)
# OLS impulse responses for comparison
ira.ols <- irf(fit.ols, n.ahead = 20)[[1]][[3]][-1, 1]

# Add to plot
lines(x = 1:20, y = ira.ols, lwd = 3, lty = 5)

