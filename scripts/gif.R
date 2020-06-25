library(fredr)
library(zoo)
library(tidyverse)
library(lubridate)
library(gganimate)
library(gifski)

dt <- readRDS("./Datos/Finales/series_version_final.rds")
pib <- readxl::read_xlsx(here::here("Datos", "Originales", "pib80-19.xlsx"))

dt_ts <- ts(data = dt[data.table::between(fecha, "1981-01-01", "2018-10-01"), 
                      .(pib, ind_vac, td, ind_av)], 
            start = c(1981, 1), frequency = 4)
ind_av <- ts(data = dt[, .(ind_av)], 
             start = c(1980, 1), frequency = 4)

# Desestacionalizo con método x13 y configuración default
td      <- seasonal::final(seasonal::seas(dt_ts[, "td"]))
ind_vac <- seasonal::final(seasonal::seas(dt_ts[, "ind_vac"]))
pib2    <- seasonal::final(seasonal::seas(dt_ts[, "pib"]))
# ind_av  <- seasonal::final(seasonal::seas(ind_av))
ind_av  <- seasonal::seas(ind_av)
ind_av  <- ind_av$data[, "trend"]

vacantes <- data.table(fecha = dt$fecha, ind_av = as.numeric(ind_av))
vacantes[, `:=`(ind = 1:.N,
                ano = year(fecha),
                mes = month(fecha))]
b <- 
    ggplot(data=vacantes, aes(x=fecha,y=ind_av)) +
    geom_line() +
    view_follow() +
    geom_point(color="red") +
    transition_reveal(ind) +
    theme_minimal() +
    theme(plot.caption=element_text(hjust=0))+
    labs(x="",y="",title="Tendencia-Ciclo índice de vacantes laborales, Uruguay",
         caption="@FedeMolinaMagne Fuente: Gallito, Buscojobs, Computrabajo, UruguayConcursa")

animate(b, end_pause=25, nframes=350,fps=12)
save_animation(last_animation(), file=here::here("vacantes.gif"))


# Vi el mercado laboral de USA y quise mostrar lo mismo para Uruguay. Lo que ven es un índice de vacantes laborales que es una aproximación a la demanda laboral por parte de empresas (simplemente vt/vo, lo tengo normalizado por la PEA y no cambia nada) para Uruguay (Montevideo) 1/2
# 
# Si bien el índice y su construcción no esta exento de críticas, la caída de los últimos 5 años es relevante. Hay un caída sistemática de la demanda laboral del sector privado (incluye públicos, pero son un % minoritario) que debería prender luces de alerta y ahondar en las causas


#### GIF CURVA BEVERIDGE
y_lim <- c(0.1, 1.1)
# x_lim <- c(min(dt$td)+1, max(dt$td)+1)
x_lim <- c(5, 18)

cb_anual <- 
    dt[data.table::between(fecha, "1981-01-01", "2018-10-01"), 
   .(td = mean(td), ind_vac = mean(ind_vac), pib = mean(pib), decada = unique(decada), 
     ano2 = gsub(pattern = "\\d{2,2}(\\d{2,2})", replacement = "\\1", x = ano)), 
   keyby = .(ano)][, ind := 1:.N] %>%
    ggplot(., aes(y = ind_vac, x = td, label = ano2)) + 
    geom_point(color = "red") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = y_lim) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10), limits = x_lim) +
    geom_path() +
    # position=position_jitter(width=.2,height=.02)
    ggrepel::geom_text_repel(fontface = "bold", size = 6, 
                             position=position_jitter(width=.1,height=.02), color = "black") +
    gganimate::transition_reveal(ind) +
    # scale_color_manual(name = "", values = color_decada) +
    # coord_fixed(ratio = 15) +
    labs(x = "Tasa de desempleo", y = "Índice de vacantes", title = "Panel B. Datos Anuales") +
    theme_Publication() +
    theme(plot.caption=element_text(hjust=0)) +
    labs(x = "Tasa de desempleo", y = "Índice de vacantes",
         title="Curva de Beveridge, Uruguay",
         caption="@FedeMolinaMagne Fuente: Gallito, Buscojobs, Computrabajo, UruguayConcursa")

gganimate::animate(cb_anual, end_pause=125, nframes=350,fps=100)
save_animation(last_animation(), file=here::here("CB_anual.gif"))

cb_trimestral <-
    ggplot(dt[data.table::between(fecha, "1981-01-01", "2018-10-01"),
              .(ano2 = paste(gsub(pattern = "\\d{2,2}(\\d{2,2})", replacement = "\\1", x = ano),
                             quarter(fecha), sep = "-"),
                td, ind_vac, fecha, ind = 1:.N)], 
           aes(y = ind_vac, x = td, label = ano2)) + 
    geom_point(color = "red") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = y_lim) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10), limits = x_lim) +
    geom_path() +
    ggrepel::geom_text_repel(fontface = "bold", size = 6,
                             position=position_jitter(width=.1,height=.02), color = "black") +
    scale_color_manual(name = "", values = color_decada) +
    gganimate::transition_reveal(ind) +
    # coord_fixed(ratio = 15) +
    theme_Publication() +
    theme(plot.caption=element_text(hjust=0)) +
    labs(x = "Tasa de desempleo", y = "Índice de vacantes",
         title="Curva de Beveridge, Uruguay",
         caption="@FedeMolinaMagne Fuente: Gallito, Buscojobs, Computrabajo, UruguayConcursa")

gganimate::animate(cb_trimestral, end_pause=125, nframes=350,fps=100)
save_animation(last_animation(), file=here::here("CB_trimestral.gif"))

###### equilibrio

library(patchwork)
create_curves <- function(title_curva1, title_curva2, theta, x_axis, y_axis) {
    ggplot(curvas, aes(x = u, y = v)) +
        geom_line() +
        geom_line(aes(x = (1:25) + 1, y = 20/(u))) +
        scale_x_continuous(limits = c(0,20)) +
        scale_y_continuous(limits = c(0,20)) + 
        geom_text(aes(label = title_curva1, y = .5, x = 12)) +
        geom_text(aes(label = title_curva2, y = 20, x = 12)) +
        {if(theta) {
            annotate('text', x = 3, y = 0.5, 
                     label = "theta",parse = TRUE,size=5)
        }} +
        {if(theta) {
            geom_curve(aes(x = 6, y = 0, xend = 1.5, yend = 1.5), 
                       colour = "#555555", 
                       curvature = 1,
                       size=0.5, linetype = "dashed")
        }} +
        labs(x = x_axis, y = y_axis) +
        # geom_segment(aes(x=0, xend = 20 , y=0, yend = 0), size=1.5,
        #              arrow = arrow(length = unit(0.6,"cm"))) +
        theme(
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour="black"), 
            axis.ticks = element_blank(),
            axis.title.x = element_text(angle = 0, hjust = 1),
            axis.title.y = element_text(angle = 0))
}
curvas <- data.table(v = seq(1, 25, 1),
                     u = seq(1, 25, 1))

p1 <- create_curves(title_curva1 = "Curva de Beveridge", title_curva2 = "Creación de trabajo", 
                    theta = TRUE, x_axis = "u", y_axis = "v")
p2 <- create_curves(title_curva1 = "Creación de trabajo", title_curva2 = "Curva salarial", 
                    theta = FALSE, y_axis = "w", x_axis = expression(theta))
p1 + p2
