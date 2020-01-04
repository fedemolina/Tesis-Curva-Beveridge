# Análisis semanal para ver que semanas de que mes muestrear.
paquetes <- c("data.table", "magrittr", "ggplot2", "plotly")
sapply(paquetes, require, character.only = TRUE)
archivos = list.files("./Datos/Originales", pattern = "Gallito-[0-9]{4}(-[0-9]{4})?(-[0-9]{2})?(-[0-9]{2})?(.xlsx)", full.names = TRUE)
# No tomar en cuenta los gallitos de 2013 a 2018, que son los entregados por el país.
archivos <- archivos[!archivos %in% "./Datos/Originales/Gallito-2013-2018.xlsx"]

lista = list()
for (i in archivos) {
    lista[[i]] <- readxl::read_excel(i) %>% 
                    as.data.table(.)
}
dt <- data.table::rbindlist(lista, use.names = TRUE, fill = TRUE, idcol = "tabla")
dt[, names(dt)[!names(dt) %in% c("f_ini", "f_fin", "avisos", "tabla")] := NULL]
dt <- dt[!is.na(f_ini),]
# Genero mes y ano.
dt[, `:=`(mes = month(f_ini),
          ano = year(f_ini),
          sem = week(f_ini),
          q = quarter(f_ini))]
dt[, .(avisos =sum(avisos, na.rm = TRUE)) , by = .(ano, mes, sem)] 
dt[, .(avisos =sum(avisos)) , by = .(ano, mes, sem)][is.na(avisos),] # Todos los casos son correctos. Son datos faltantes.

# genero los trimestres y días en el mes
dt[, `:=`(q_ini = quarter(f_ini),
          q_fin = quarter(f_fin))]
dt[, diames := lubridate::days_in_month(f_ini)]
dt[, f_ini-f_fin] # Como siempre son 7 días la diferencia es de 6 dias, ok.
dt[month(f_ini) != month(f_fin), .(lubridate::day(f_ini)-diames)] 

# Casos en los cuales no se corresponden los meses entre f_ini y f_fin
# Si es mayor a 3 entonces hay más días en f_fin.
dt[, mes_c := mes]
dt[month(f_ini) != month(f_fin) & abs(lubridate::day(f_ini)-diames) < 3, mes_c := month(f_fin)]

# Series agrupadas
dt[, sum(avisos), by = .(ano, mes_c)]
dt[, sum(avisos), by = .(ano, q_ini)]
dt[, sum(avisos), by = .(year(f_fin), q_fin)]

# Urrestarazu incluyo los avisos en el trimestre en el cual estuvieron más días.
dt[q_ini != q_fin,] # semanas problemáticos al trimestralizar
# Corrección de trimestres
dt[, q_c := q_ini][q_ini != q_fin & mes != mes_c, q_c := q_fin]
# Corrección de año
dt[, ano_c := ano]
dt[q_ini - q_fin > 1 & q_c == 1, ano_c := year(f_fin)]
# corección de semana (1ra semana del año)
dt[, sem_c := sem][q_ini - q_fin > 1 & q_c == 1, sem_c := 1]

# Remueve filas vacias (sin tomar en cuenta avisos, que serían a imputar)
dt <- dt[complete.cases(dt[, !"avisos"]),]
dt[is.na(avisos),]

# Análisis semanal necesito agrupar por semana. NO ESTOY IMPUTANDO 

dt[, .(avisos = sum(avisos, na.rm = TRUE)), keyby = .(mes_c, sem_c)] %>% plot()

# Notar como no parecen haber grandes diferencias
dt[, plot_ly(x = f_ini, y = avisos) %>% 
       add_markers(), by = ]

# Graficar lo mismo pero agrupado por semana
dt[, .(avisos = sum(avisos, na.rm = TRUE), fecha = unique(f_ini)), keyby = .(ano_c, mes_c, sem_c)
   ][, plot_ly(x = fecha, y = avisos,
               hoverinfo = 'text',
               text = ~paste('<br>Mes: ', mes_c,
                             '</br> semana: ', sem_c,
                             '</br> avisos: ', avisos,
                             '</br> fecha: ', fecha
                             )) %>% 
         add_markers(color = factor(mes_c)) #%>% 
         # add_annotations(x = fecha,
         #                 y = avisos,
         #                 text = sem_c,
         #                 xref = "x",
         #                 yref = "y",
         #                 showarrow = FALSE,
         #                 arrowhead = 4,
         #                 arrowsize = .5,
         #                 xanchor = 'left',
         #                 ax = 20,
         #                 ay = -40)
     ]
