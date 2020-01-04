librerias <- c("data.table", "ggplot2", "forecast", "urca")
sapply(librerias, require, character.only = TRUE)

repo <- here::here("Datos", "Finales", "desempleo.rds")
dt <- readRDS(repo)
dt_long <- data.table::melt(dt, id.vars = c("fecha", "ano", "mes", "dia"), measure.vars = c("ta", "te", "td"))
dt_ts <- ts(dt[,c("ta", "te", "td")], start = c(1981,1), frequency = 4)

dt[, cor(td,te, use = "complete")]
dt[, cor(ta,td, use = "complete")]
dt[, cor(te,ta, use = "complete")]

dtf_ts <- diff(dt_ts, 1)
ddtf_ts <- diff(dt_ts, 2)

plot(dtf_ts)
plot(dtf_ts[,"te"], dtf_ts[,"td"])
plot(ddtf_ts[,"te"], ddtf_ts[,"td"])
plot(dt_ts[,"td"], dtf_ts[,"td"])
plot(dt_ts[,"td"], ddtf_ts[,"td"])
plot(ddtf_ts[,"td"], dtf_ts[,"td"])

ggplot(data = dt_long) +
    aes(x = fecha, y = value, col = variable) +
    geom_line()

dt[, pairs(dt[2:4])]
