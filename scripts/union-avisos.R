paquetes <- c("ggplot2","magrittr", "data.table")
sapply(paquetes, require, character.only = TRUE)

# Carga de funciones
source("./scripts/delete.R")
source(here::here("scripts", "clean_column.R"))
source(here::here("scripts", "clean_column_puesto.R"))
source(here::here("scripts", "clean_column_empresa.R"))
source(here::here("scripts", "clean_column_dpto.R"))

files <- list.files(path = "./Datos/Intermedias", pattern = "compatibilizado", full.names = TRUE)
lista = list()
for (i in files){
  lista[[i]] <- readRDS(i)
}
lista
lapply(lista, names)
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

setnames(lista$`./Datos/Intermedias/gallito-compatibilizado.rds`, old = "departamento", new = "dpto")
setnames(lista$`./Datos/Intermedias/gallito-compatibilizado.rds`, old = "link", new = "id")
setnames(lista$`./Datos/Intermedias/computrabajo-compatibilizado.rds`, old = "ID", new = "id")

# unión
dt <- rbindlist(lista, use.names = TRUE, fill = TRUE, idcol = "pagina")

# Limpiar pagina
dt[grepl(x = pagina, pattern =  "gallito"), pagina := "gallito"]
dt[grepl(x = pagina, pattern =  "buscojobs"), pagina := "buscojobs"]
dt[grepl(x = pagina, pattern =  "computrabajo"), pagina := "computrabajo"]

# Avisos sin link
dt[is.na(id), .N]
# 0 ? Pero deberían haber NA desde gallito. REVISAR.

# Repetidos
dt[duplicated(dt, by = c("puesto", "empresa", "dpto", "ano", "mes")), .N]
dt[duplicated(dt, by = c("puesto", "empresa")), .N]
dt[duplicated(dt, by = c("id")), .N]

dt[, .N, keyby = .(ano, mes)]
dt[, .N, keyby = .(pagina, ano, mes)]

dt[duplicated(dt, by = c("puesto", "empresa", "dpto", "ano", "mes")), .N, keyby = .(ano, mes)
   ][, plot(x = as.Date(paste(ano,mes,1, sep = "-"), format = "%Y-%m-%d"), N, type = "l")]

# Cantidad de avisos por página web
dt[ano == 2018, table(pagina, mes, useNA = "a")]
dt[ano == 2019, table(pagina, mes, useNA = "a")]
# ERROR NO APARECEN AVISOS DE COMPUTRABAJO EN 2019 11 Y 12??? REVISAR

# cambio el formato
dt[, avisos := 1]
data.table::melt(dt, id.vars = "pagina", measure.vars = "avisos")

# Avisos compartidos por páina web

#
dt[pagina == "gallito", .N, keyby = .(ano, mes)]
ggplot(dt[pagina == "gallito" & ano == 2019 & mes == 11, ], aes(x = forcats::fct_infreq(factor(nivel)))) +
    geom_bar() +
    coord_flip() +
    labs(y = "Cantidad de avisos", x = "", title = "Avisos gallito noviembre 2019") +
    theme(axis.text.x = element_text(angle = 90)) +
    theme_classic()

# Nombres
names(lista$`./Datos/Intermedias/buscojobs-compatibilizado.rds`)
dt[pagina == "buscojobs", table(area)]
    


# Analisis de texto -------------------------------------------------------

dt[pagina == "buscojobs", text_comun := detalles]
dt[pagina == "gallito", text_comun := paste(responsabilidades, funciones)]
dt[pagina == "computrabajo", text_comun := desc]

# Limpiar text_comun nuevamente
clean_column(dt, column = "text_comun", pagina = "none")

# Limpiar avisos y empresas (nuevamente por cualquier posible error)
clean_column_empresa(dt, column = "empresa")
# LOS AVISOS DE GALLITO EN EMPRESAS TIENEN PATRON "Para ....."
clean_column_puesto(dt, column = "puesto")

write.table(x = dt[, .N, keyby = .(empresa, pagina)], file = here::here("Datos", "Intermedias", "paginasEmpresa.txt"), row.names = FALSE, sep = ",")

# Guardo para probar, pero tengo que arreglar los errores en fechas en buscojobs y computrabajo.
saveRDS(file = "./Datos/Intermedias/unionAvisos.rds", object = dt, compress = FALSE)

dt[, text_comun %>% tail]

# Avisos repetidos
dt[pagina == "buscojobs" & duplicated(dt, by = c("text_comun", "empresa", "puesto")),
   .(repetidos = .N), by = pagina]
dt[pagina == "buscojobs" & duplicated(dt, by = c("empresa", "puesto")),
   .(repetidos = .N), by = pagina]
dt[duplicated(dt, by = c("empresa", "puesto")),
   .(repetidos = .N), by = pagina]

# Calcular similaridad entre avisos de páginas basado en la columna text_comun
dt[nchar(text_comun) == 0, .N]

# Reordenar en otra parte del script, limpio página y ID
dt[, id := gsub(id, pattern = ".*/(.*)", replacement = "\\1")]


# Text Mining -------------------------------------------------------------
paquetes <- c("text2vec", "data.table", "magrittr")
sapply(paquetes, require, character.only = TRUE)
options(max.print = 100)
# Voy a usar el paquete text2vec
dt <- readRDS("./Datos/Intermedias/unionAvisos.rds")
# Necesito calcular la similaridad entre avisos de las distintas páginas.
# Cada aviso debería ser un documento, y comparar la similaridad entre los documentos.
# Cada fila debe ser un aviso y las columnas el contenido del aviso.
dt[, id := gsub(id, pattern = ".*/(.*)", replacement = "\\1")]

# Ejemplo:

# Solo avisos con texto en text_comun
dt[is.na(text_comun), .N]
dt[is.null(text_comun), .N]
dt2 <- dt[text_comun != "", ]
dt2[, id_int := 1:.N]

# Creamos los token
it1 <- text2vec::itoken(dt2[pagina == "buscojobs", text_comun], ids = dt2[pagina == "buscojobs", id])
it2 <- text2vec::itoken(dt2[pagina == "gallito", text_comun], ids = dt2[pagina == "gallito", id])

# Vectorizamos el vocabulario
it = itoken(dt2$text_comun, progressbar = FALSE)
v = create_vocabulary(it, ngram = c(ngram_min = 1L, ngram_max = 1L))
v = prune_vocabulary(v, doc_proportion_max = 1L, term_count_min = 1L)
vectorizer = vocab_vectorizer(v)

# creamos DTM (en el mismo espacio vectorial)
dtm1 = create_dtm(it1, vectorizer)
dtm2 = create_dtm(it2, vectorizer)

sim_mat <- sim2(dtm1, dtm2)
dim(sim_mat)
max(sim_mat)
which.max(sim_mat)
class(sim_mat)
sim_mat[1, 1:100]
which.max(sim_mat[2000, ])
max(sim_mat[2000, ])
sim_mat[2, ]
max(sim_mat[1:3, 1:3])

# 1 SOLO dtm
it = itoken(dt2$text_comun, ids = dt2$id_int, progressbar = FALSE)
v = create_vocabulary(it)
v = prune_vocabulary(v, doc_proportion_max = 0.1, term_count_min = 5)
vectorizer = vocab_vectorizer(v)

dtm = create_dtm(it, vectorizer)
tfidf = TfIdf$new()
dtm_tfidf = fit_transform(dtm, tfidf)

cos_sim = sim2(x = dtm_tfidf, method = "cosine", norm = "l2")
cos_sim[1:2, 1:5]
diag(cos_sim) = 0

# Obtengo el máximo y la posición por fila
dim(cos_sim)
n = as.integer(NROW(cos_sim))
vec_max = vector(mode = "numeric", length = n)
pos_max = vector(mode = "integer", length = n)
text_max = vector(mode = "character", length = n)
pag_max  = vector(mode = "character", length = n)
puesto_max <- vector(mode = "character", length = n)

pos_max <- max.col(cos_sim)
dd <- as.matrix(cos_sim)
for(i in 1L:n) {
  print(i)
  vec_max[i]  = dd[i, pos_max[i]]
  text_max[i] = dt2[pos_max[i], text_comun]
  pag_max[i]  = dt2[pos_max[i], pagina]
  puesto_max[i] = dt2[pos_max[i], puesto]
}

dt2[, `:=`(vec_max = vec_max,
           pos_max = pos_max,
           text_max = text_max,
           pag_max = pag_max,
           puesto_max = puesto_max)]

dt2[, .(vec_max, pos_max, pagina, pag_max, puesto, puesto_max, text_comun, text_max)] %>% View
