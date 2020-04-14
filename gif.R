library(fredr)
library(zoo)
library(tidyverse)
library(lubridate)
library(gganimate)
library(gifski)

fredr_set_key("d48fc54ef201e9d42cbe2240aae43b08")

gifski
av

#################################################
# get and wrangle data

# Seasonally adjusted weekly claims
ICSA<- 
    fredr(
        series_id = "ICSA",
        observation_start = as.Date("1967-01-01")
    )

# Non seasonally adjusted weekly claims
ICNSA<-
    fredr(
        series_id = "ICNSA",
        observation_start = as.Date("1967-01-01")
    )

# Monthly US Labor Force (Seasonally Adjusted)
CLF16OV <- 
    fredr(
        series_id = "CLF16OV",
        observation_start = as.Date("1967-01-01")
    )


# Monthly Nonfarm Payroll employment (seasonally adjusted)
# used later
PAYEMS<-
    fredr(
        series_id = "PAYEMS",
        observation_start = as.Date("1948-01-01")
    )

# wrangle data ----

df <- 
    left_join(ICNSA %>% pivot_wider(names_from=series_id,values_from=value),
              ICSA %>% pivot_wider(names_from=series_id,values_from=value),
              by="date") %>%
    mutate(year=year(date),month=month(date))

df2 <- 
    data.frame(CLF16OV %>% pivot_wider(names_from=series_id,values_from=value)) %>% 
    mutate(year=year(date),month=month(date)) %>%
    select(-date)

# merge monthly labor force stats with weekly claims data by year and month
# we could refine the interpolation, but it won't matter for the big picture

df3 <- left_join(df, df2, by=c("year","month")) %>%
    mutate(ind=row_number(),
           ratioSA=ICSA/CLF16OV/1000,
           ratioNSA=ICNSA/CLF16OV/1000) %>% 
    # add some dramatic pauses for the animation in the last 2 weeks
    mutate(ind=ifelse(date=="2020-03-21", ind+1200,ind)) %>%
    mutate(ind=ifelse(date==max(date),ind+2500,ind))


a <- 
    ggplot(data=df3, aes(x=date,y=ICSA/1000)) +
    geom_line() +
    gganimate::view_follow() +
    geom_point(color="red") +
    gganimate::transition_reveal(ind) +
    theme_minimal() +
    theme(plot.caption=element_text(hjust=0))+
    labs(x="",y="",title="Initial Jobless Claims (thousands, seasonally adjusted)",
         caption="@lenkiefer Source: U.S. Department of Labor")

animate(a,end_pause=25, nframes=350,fps=12)
save_animation(last_animation(), file=here::here("gif"))

a2 <- 
    ggplot(data=df3, aes(x=date,y=ICNSA/1000))+geom_line()+
    view_follow()+
    geom_point(color="red")+
    transition_reveal(ind)+
    theme_minimal()+
    theme(plot.caption=element_text(hjust=0))+
    labs(x="",y="",title="Initial Jobless Claims (thousands, not seasonally adjusted)",
         caption="@lenkiefer Source: U.S. Department of Labor")


animate(a2,end_pause=25, nframes=350,fps=12)
save_animation(last_animation(), file="PATH_FOR_GIF2")


a3 <- 
    ggplot(data=df3, aes(x=date,y=ratioSA ))+geom_line()+
    view_follow()+
    geom_point(color="red")+
    scale_y_continuous(labels=scales::percent)+
    transition_reveal(ind)+
    theme_minimal()+
    theme(plot.caption=element_text(hjust=0))+
    labs(x="",y="",
         title="Initial Jobless Claims as a % of Labor Force (seasonally adjusted)",
         caption="@lenkiefer Source: U.S. Department of Labor")


animate(a3,end_pause=25, nframes=350,fps=12)
save_animation(last_animation(), file="PATH_FOR_GIF3")

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


Vi el mercado laboral de USA y quise mostrar lo mismo para Uruguay. Lo que ven es un índice de vacantes laborales que es una aproximación a la demanda laboral por parte de empresas (simplemente vt/vo, lo tengo normalizado por la PEA y no cambia nada) para Uruguay (Montevideo) 1/2

Si bien el índice y su construcción no esta exento de críticas, la caída de los últimos 5 años es relevante. Hay un caída sistemática de la demanda laboral del sector privado (incluye públicos, pero son un % minoritario) que debería prender luces de alerta y ahondar en las causas
