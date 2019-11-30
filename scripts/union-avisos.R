paquetes <- c("magrittr", "data.table")
sapply(paquetes, require, character.only = TRUE)

# Carga de funciones
source("./scripts/delete.R")

files <- list.files(path = "./union-avisos", pattern = ".rds", full.names = TRUE)
lista = list()
for (i in files){
  lista[[i]] <- readRDS(i)
}
lista
lapply(lista, class)

# Compatibilizar nombres
# dpto = departamento
# fecha = fecha_pub (no calculada para computrabajo PENDIENTE)
# ID = link --> id

# Variables comunes a usar (pueden haber otras que refieren a lo mismo con nombre distinto)
# 1. puesto
# 2. empresa
# 3. dpto
# 4. fecha_pub
# 5. ano
# 6. mes
# 7. dia
# 8. fecha_scraping
# 9. area (2) y categorias
# 10. detalle o descripción

setnames(lista$`./union-avisos/gallito-compatabilizado.rds`, old = "departamento", new = "dpto")




# unión
dt <- rbindlist(lista, use.names = TRUE, fill = TRUE, idcol = "pagina")

# renombrar ID = link
# Computrabajo = ID
# Buscojobs = ID
# gallito = link

dt[!is.na(link), ID := link]

# Repetidos
dt[duplicated(dt, by = c("puesto", "empresa", "dpto", "ano", "mes"))] %>% 
  dim()
dt[duplicated(dt, by = c("puesto", "empresa", "dpto"))] %>% 
  dim()

# Importante generar ano, mes, (día si fuese posible) de la fecha de publicación NO de la fecha de scraping.

dt[, .N, by = .(ano, mes)] 
# Esto no debería pasar todas tiene fecha_scraping, salvo que sea el ano, mes de la fecha_pub? Tampoco, revisar los scripts previos.

dt[, `:=`(ano = year(fecha_scraping),
          mes = month(fecha_scraping),
          dia = mday(fecha_scraping))]

dt[duplicated(dt, by = c("puesto", "empresa", "dpto", "ano", "mes")), .N, keyby = .(ano, mes)
   ][, plot(x = as.Date(paste(ano,mes,1, sep = "-"), format = "%Y-%m-%d"), N, type = "l")]

# Recordar que el link/ID se puede repetir en meses distintos, en ese caso no serían avisos repetidos, tomar eso en cuenta.
# PENDIENTE DE HACER.

delete(dt, del.idxs = which(duplicated(dt, by = c("ID"))))

# Cantidad de avisos por página web
dt[, table(pagina, mes, useNA = "a")]

# cambio el formato
dt[, avisos := 1]
data.table::melt(dt, id.vars = "pagina", measure.vars = "avisos")

# Avisos compartidos por páina web