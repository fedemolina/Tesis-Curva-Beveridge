paquetes <- c("data.table", "magrittr", "ggplot2", "forecast")
sapply(paquetes, require, character.only = TRUE)

tasas_urr <- readxl::read_excel(here::here("Datos", "Originales", "vacantes-1981-1995.xlsx")) %>% as.data.table

pea_ts <- ts(tasas_urr[, PEA], start = c(1981, 3), frequency = 4)

mod <- forecast::auto.arima(pea_ts, stepwise = FALSE, parallel = TRUE, approximation = FALSE, 
                            seasonal = TRUE, allowdrift = TRUE, allowmean = TRUE, stationary = FALSE, D = 1)

pred <- forecast::forecast(pea_ts, robust = TRUE, model = mod, lambda = "auto", biasadj = TRUE, h = 12)

autoplot(pred)

pred$x %>% autoplot +
    geom_line(pred$fitted)
pred$fitted %>% autoplot()
plot(pred$fitted)
lines(pred$x)
ts.union(pred$mean, pea_ts, )
ts.intersect(pred$mean, pea_ts)

c(pea_ts, pred$mean)
rbind(pred$mean, pea_ts)

pred_pea <- ts(c(pea_ts, pred$mean), start = c(1981, 3), frequency = 4)
pred_pea <- data.table(pea = c(pea_ts, pred$mean)*1000, fecha = seq.Date(from = as.Date("1981-07-01"), to = as.Date("1998-10-01"), by = "quarter"))
saveRDS(object =  pred_pea ,here::here("Datos", "Finales", "pea-urrest-pred.rds"))


# Estimar con el nuevo método fforma del paquete metalearning
# https://github.com/robjhyndman/M4metalearning/blob/master/docs/metalearning_example.md
# https://www.sciencedirect.com/science/article/pii/S0169207019301876

# Estimación de vacantes
