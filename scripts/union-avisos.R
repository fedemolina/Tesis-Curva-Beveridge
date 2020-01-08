# Limpieza ----------------------------------------------------------------
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

# Revisar posibles empresas, u otras variables que hayan quedado como "".
# Para volver a revisar duplicados
dt[pagina == "buscojobs" & puesto == "", ]
cols <-  names(dt)[!sapply(dt, function(x) {
  lubridate::is.Date(x) | lubridate::is.POSIXct(x)
})]

for(l in cols) {
  set(x = dt, j = l, i = which(dt[[l]] == ""), value = NA_character_)
}


# Avisos sin link (son de galltio)
dt[is.na(id), .N]
dt[is.na(id), .N, by = pagina]

# Repetidos
dt[duplicated(dt, by = c("puesto", "empresa", "dpto", "ano", "mes")), .N]
dt[duplicated(dt, by = c("puesto", "empresa")), .N]
dt[duplicated(dt, by = c("id")), .N] # los NA

dt[, .N, keyby = .(ano, mes)]
dt[, .N, keyby = .(ano, mes, pagina)]

dt[duplicated(dt, by = c("puesto", "empresa", "dpto", "ano", "mes")), .N, keyby = .(ano, mes)
   ][, plot(x = as.Date(paste(ano,mes,1, sep = "-"), format = "%Y-%m-%d"), N, type = "l")]

# Cantidad de avisos por página web
dt[ano == 2018, table(pagina, mes, useNA = "a")]
dt[ano == 2019, table(pagina, mes, useNA = "a")]
# Computrabajo tiene una caída muy importante a qué se debe? Es un error?
# Ver: https://web.archive.org/web/20181210032508/https://www.computrabajo.com.uy/ofertas-de-trabajo/
# 10/12/2018 hay 1485 avisos en los últimos 30 días, lo cual va en linea con los 1276 avisos que están scrapeados.
# Buscojobs arranca en enero 2019.
# Gallito y computrabajo se pueden tomar desde Ocutbre de 2018.
# Los tomo 2 a 2 para ver repetidos?

# Arreglo empresas para que coincidan lo mejor posible. Leer archivo empresasAvisos.txt y construir una columna a mapear.
map_empresa <- data.table::fread("empresasAvisos.txt", header = FALSE, fill = TRUE, na.strings = "", stringsAsFactors = FALSE)
clean_comillas <- function(var) {
  map_empresa[, (var) := gsub(x = get(var), pattern = "^['\"]|['\"']$", replacement = "")]  
}
cols <- paste0("V", 1:6)
for(i in cols){
  clean_comillas(i)
}

mapeo_empresas <- c("regency", "abc service", "abitab", "academia de choferes alvaro", "acamar sa", "acce sa", "acher ceramicas", "acompaña", "adt", "aee ltda asesoria económica empresarial",
                    "ag equipamientos", "aglh", "alcance servicio de compañia", "aliados bienes inmobiliarios", "amv store", "apunto aeropuerto", "arnaldo castro", "arocena srl", 
                    "asem servicio de acompañantes", "asesores de reclutamiento", "asociación española", "assist", "avanza externalización de servicios uruguay sa", "bella flor",
                    "biriden", "bienestar laboral y cultural", "blanco & etcheverry", "borcam equipamientos srl", "boyerco", "bqn", "brigth centro de estética", "britt shop uruguay",
                    "cash studio & academy", "cap uruguay ltda", "capital humano", "casa tres contract center", "cash sa", "casinos importaciones sa aparicio saravia", "certus", 
                    "cesa management soluciones", "chivipizza", "cielo azul cementos", "cismo software", "clanis sa", "cnglobal arcadia srl", "cnt uruguay", "coca cola", "codigo del sur", 
                    "colegio bilingue", "colegio católico", "conamil sa", "coneban sa", "human phi", "muñoz y asociados", "auren", "convixion", "corporación de maquinaria sa", "cpa ferrere", 
                    "crame iampp", "creando bienestar", "curbelo & cianci", "dca contadores y asociados", "del sol", "despegar.com", "depilife", "dimm futuro", "dogrin company sa", 
                    "duty free americas", "e lace media services", "easymail", "ecocredit", "ecologicink", "el viajero hostels", "emprendedores sa", "enjoy punta del este", "entrust",
                    "equifax", "espina sa", "essen ltda", "express", "forum", "frigorifico matadero pando", "full human", "fundación julio ricaldoni", "gaya importaciones", "gbs company", 
                    "geocom", "gh consultores", "glocal chemical", "glu corp", "go nova asesoria", "goddard catering group", "grupo morgal", "gts uruguay", "guf consultancy services", 
                    "caterpillar", "hardworking", "grido", "herbalife", "hertz rent a car", "hlc impresoras", "hometal", "iat efg", "ids ltda", "industrias pugliese sa", "ingenieria contra incendios",
                    "instalar srl", "j y s cadeteria", "jcdecaux", "joaquin y mateo fernández", "kavehome", "ketlark high urban security", "lavadero industrial", "leopoldo gross y asociados",
                    "level bienes raices", "limpex", "lithium", "look y arte", "manpower", "market point", "mazars", "mb uruguay", "mekatronic", "mercado del este", "mini mercado", "mirtrans cargo postal",
                    "mjobo", "montes del plata", "montevideo beer company", "mosca", "movie group", "mta ingenieria", "multiline", "multiworks", "myg asociados", "natural mp", "ncc latam", 
                    "obras y servicios", "onix-hr consulting", "opción consultores", "ormen sa", "own hotel montevideo", "panda", "bimbo", "ta-ta", "parisien", "pass card", "pedidos ya",
                    "petit amies", "pez globo", "pingakol", "pnud", "poder judicial","polybag ltda", "posadas & vecino consultores", "possum consulting", "pwc", "pro hygiene", "prosegur", "randstad", 
                    "rci latinoamerica", "remax costa", "repartos ya", "rh consultores", "rodriguez de almeida & asociados", "rsm", "rua asistencia", "ruiz masse", "s & d consultores", 
                    "sabre", "safegroup", "san francisco", "sanatorio americano", "sap sa", "securitas", "segurpas srl", "shopping tu mejor opción", "sigelor", "sira ltda", "sofitel",
                    "soler hr consulting", "sonda", "spymovil", "stadium", "starbucks", "stavros moyal", "stella barboza", "talento consultores", "tayma", "team sales", "tgu terminales graneleras",
                    "tienda inglesa", "totvs", "tu esencia", "uhyhubba & asociados", "upm", "urudata", "utec", "vector seguridad", "veo juguetes", "verifone", "vida saludable", "vita body",
                    "vselex", "woow", "work office", "zaros")

# Mapear a los nombres elegidos de las empresas.
map_empresa <- as.data.frame(t(map_empresa), stringsAsFactors = FALSE, row.names = FALSE)
setDT(map_empresa)
map_empresa[, V2]
dt[empresa %in% map_empresa[, V22] & !is.na(empresa), empresa]
for(i in 1:NCOL(map_empresa)) {
  var = paste0("V",i)
  dt[empresa %in% map_empresa[, get(var)] & !is.na(empresa), empresa := mapeo_empresas[i]]
} # listo

# Y ahora cómo quedan las empresas?
dt[, table(empresa)]
dt[, table(empresa, useNA = "a")][order(dt[, table(empresa, useNA = "a")], decreasing = TRUE)] %>% View
# Quedo pendiente:
# importanteempresadelsector
# confidencial
dt[empresa == "confidencial", empresa := NA_character_]
dt[empresa == "importanteempresadelsector", empresa := NA_character_]
dt[empresa == "--------------------", empresa := NA_character_]
dt[, table(empresa, useNA = "a")][order(dt[, table(empresa, useNA = "a")], decreasing = TRUE)] %>% View

dt[, empresa :=  empresa %>% 
     gsub(x = ., pattern = "s\\.a\\.$", replacement = "sa") %>% 
     gsub(x = ., pattern = "^\\.", replacement = "") %>% 
     gsub(x = ., pattern = "\\s{2,}", replacement = " ") %>% 
     gsub(x = ., pattern = "^-", replacement = "")
   ]
dt[empresa == "", empresa := NA_character_]

# Limpiar puesto de forma similar. Se limpio computrabajo y buscojobs, pero no gallito.
dt[, tail(.SD[, puesto], 40), by = pagina]
clean_column_puesto(dt = dt, column = "puesto")

# Probar duplicados por puesto y página. O por empresas y fecha
dt[, .N, keyby = .(empresa)][order(N, decreasing = TRUE), ]
dt[duplicated(dt, by = c("empresa", "puesto")), .N, by = .(pagina)]
dt[pagina == "gallito" & duplicated(dt, by = c("empresa", "puesto")), .N, by = .(pagina)]


# Avisos compartidos por páina web

#

# for + table

# Algunas columnas se pueden seguir limpian y unir, barrio_ciudad y ciudad.

# Limpiar lo que quedo mal de unicode, que no se ve como tal, ej: 
# Limpiar los números también
dt[16801, desc]

# Limpiar los acentos
unwanted_array = list('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                      'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                      'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                      'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                      'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' ) #'ñ'='n' 'Ñ'='N'
dt$desc[16801] %>% 
chartr(paste(names(unwanted_array), collapse=''),
       paste(unwanted_array, collapse=''),
       .)

# Borrar columnas sin uso
dt[, `:=`(file = NULL,
          dia_scr = NULL,
          fecha = NULL)]

# detalle y detalles, distinta página similar información????
# detalle es del gallito
# detalles de buscojobs
DT = copy(dt)
# Forma lenta e ineficiente de volver a limpiar todas las columnas.
cols <- c("puesto", "desc", "dpto", "detalles", "detalle", "requisitos", "barrio_ciudad", "resumen", 
          "ciudad", "categorias", "nivel", "responsabilidades", "funciones")
for(j in cols) {
  for(i in 1:NROW(dt)) {
    # set(ppp, i=which(ppp[[col]]==6), j=col, value=NA)
    set(dt, 
        i = i, 
        j = j, 
        value = quanteda::tokens(x = dt[[i, j]], what = "word", remove_numbers = TRUE, remove_punct = TRUE, 
                                 remove_symbols = TRUE, remove_separators = TRUE, ngrams = 1L) %>% 
          paste(., collapse = " ") %>% 
          chartr(paste(names(unwanted_array), collapse=''),
                 paste(unwanted_array, collapse=''),
                 .))
  }  
}

# Guarda archivo final.
saveRDS(object = dt, file = here::here("Datos", "Finales", "AvisosCompatibilizados.rds"))


# Analisis de texto -------------------------------------------------------

# Seleccionar solo algunos meses para revisar la cantidad de avisos repetidos entre páginas.


#

# dt[pagina == "buscojobs", text_comun := detalles]
# dt[pagina == "gallito", text_comun := paste(responsabilidades, funciones)]
# dt[pagina == "computrabajo", text_comun := desc]

# # Limpiar text_comun nuevamente
# clean_column(dt, column = "text_comun", pagina = "none")

# Limpiar avisos y empresas (nuevamente por cualquier posible error)
# clean_column_empresa(dt, column = "empresa")
# LOS AVISOS DE GALLITO EN EMPRESAS TIENEN PATRON "Para ....."
# clean_column_puesto(dt, column = "puesto")
# Todas las empresas: importante empresa, confidencial y similares son todos missing.
# Por lo que deben ser fijados como NA o en su defecto "" para el análisis de texto.

# write.table(x = dt[, .N, keyby = .(empresa, pagina)], file = here::here("Datos", "Intermedias", "paginasEmpresa.txt"), row.names = FALSE, sep = ",")

# Guardo para probar, pero tengo que arreglar los errores en fechas en buscojobs y computrabajo.
# saveRDS(file = "./Datos/Intermedias/unionAvisos.rds", object = dt, compress = FALSE)

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
gsub(dt$id, pattern = ".*/(.*)", replacement = "\\1")

# Text Mining -------------------------------------------------------------
paquetes <- c("text2vec", "data.table", "magrittr")
sapply(paquetes, require, character.only = TRUE)
options(max.print = 100)
# Voy a usar el paquete text2vec
dt <- readRDS("./Datos/Finales/AvisosCompatibilizados.rds")
# Necesito calcular la similaridad entre avisos de las distintas páginas.
# Cada aviso debería ser un documento, y comparar la similaridad entre los documentos.
# Cada fila debe ser un aviso y las columnas el contenido del aviso.
dt[, id := gsub(dt$id, pattern = ".*/(.*)", replacement = "\\1")]
dt[, dpto := dplyr::case_when(
  dpto == "" ~ NA_character_,
  dpto == "artigas" ~ "artigas",
  dpto == "canelones" ~ "canelones",
  dpto == "cerro-largo" ~ "cerrolargo",
  dpto == "colonia" ~ "colonia",
  dpto == "durazno" ~ "durazno",
  dpto == "flores" ~ "flores",
  dpto == "florida" ~ "florida",
  dpto == "lavalleja" ~ "lavalleja",
  dpto == "maldonado" ~ "maldonado",
  dpto == "montevideo" ~ "montevideo",
  dpto == "paysandu" ~ "paysandu",
  dpto == "lasflores" ~ "flores",
  dpto == "rio-negro" ~ "rionegro",
  dpto == "rivera" ~ "rivera",
  dpto == "rocha" ~ "rocha", 
  dpto == "salto" ~ "salto",
  dpto == "san-jose" ~ "sanjose",
  dpto == "soriano" ~ "soriano",
  dpto == "tacuarembo" ~ "tacuarembo",
  dpto == "treinta-y-tres" ~ "treintaytres",
  TRUE ~ "otros"
)]
dt[, table(dpto, useNA = "a")]
# Que meses utilizar
dt[, .N, keyby = .(ano, mes, pagina)]
# Para las 3 páginas:
# enero, febrero, marzo 2019.
# octubre, noviembre, diciembre 2019.
# Para computrabajo y gallito:
# Además de los mencionados, octubre, noviembre y diciembre de 2018? NO. no hay información en gallito (text_comun).


# Texto común.
dt[pagina == "buscojobs", text_comun := detalles]
dt[pagina == "gallito", text_comun := paste(responsabilidades, funciones)]
dt[pagina == "computrabajo", text_comun := desc]
dt[, text_comun := trimws(text_comun)]
# NO se consideran avisos sin text_comun o con menos de 5 palabras.
dt2 <- dt[text_comun != "" & stringi::stri_count_words(text_comun) >=5, ] 
# otra forma de contar palabras: sapply(gregexpr("[[:alpha:]]+", X), function(x) sum(x > 0))
dt2[, id_int := 1L:.N]

# Generación de similaridad entre avisos
# Enero 2019 no debería elegirse porque buscojobs solo tiene ~ 263 avisos con text_comun no nulo.
gen_mat <- function(.vectorizer, .it1, .it2, ..fun = .fun, ..method = .method, ..norm = .norm) {
  # creamos DTM (en el mismo espacio vectorial)
  dtm1 = create_dtm(.it1, .vectorizer)
  dtm2 = create_dtm(.it2, .vectorizer)
  
  # Transformación
  tfidf = TfIdf$new()
  dtm1_tf = fit_transform(dtm1, tfidf)
  dtm2_tf = fit_transform(dtm2, tfidf)
  
  # Similaridad
  # dtm1 filas, dtm2 columnas
  sim_mat <- match.fun(..fun)(dtm1_tf, dtm2_tf, method = ..method, norm = ..norm)
  return(sim_mat)
}
get_max_id <- function(.dt) {
  as.integer(
    colnames(.dt)[max.col(.dt)]
  )
}
get_max_val <- function(.dt) {
  matrixStats::rowMaxs(as.matrix(.dt))
} 
get_id <- function(.dt) {
  as.integer(
    rownames(.dt)
  )
}
get_max <- function(.dt, .pag_sel, .ano, .mes_ti, .mes_tj, .fun = "sim2", .method = "jaccard", .norm = "l2") {
  # Parámetros:
  # data.frame con id y texto común
  # Año
  # Mes base
  # Mes siguiente (puede ser igual base)
  # Página
  # Función (match.fun)
  sub_dt = .dt[(pagina == .pag_sel) & (ano == .ano) & (mes == .mes_ti), .(text_comun, id_int)]
  it = itoken(.dt[, text_comun], progressbar = FALSE)
  v  = create_vocabulary(it, ngram = c(ngram_min = 1L, ngram_max = 1L))
  v  = prune_vocabulary(v, doc_proportion_max = 1L, term_count_min = 1L)
  vectorizer = vocab_vectorizer(v)
  
  it1 <- text2vec::itoken(sub_dt[, text_comun], 
                          ids = sub_dt[, id_int], progressbar = FALSE)
  # Columnas, que van a ir variando
  paginas = c("gallito", "buscojobs", "computrabajo")
  temp = NULL
  k = 0
  for(pag in paginas) {
    k = k + 1
    it2 <- text2vec::itoken(.dt[pagina == pag & ano == .ano & mes == .mes_tj, text_comun],
                            ids = .dt[pagina == pag & ano == .ano & mes == .mes_tj, id_int], progressbar = FALSE)    
    temp_mat = gen_mat(.vectorizer = vectorizer, .it1 = it1, .it2 = it2, ..fun = .fun, ..method = .method, ..norm = .norm)
    # Cuando la matriz es simétrica es necesario darle valor 0 a la diagonal
    if(.pag_sel == pag & .mes_ti == .mes_tj) {
      diag(temp_mat) = 0
    }
    # Nombre de la columna: var + mes + pagina
    # Ejemplo: 
    # max_pos_ene_g, max_pos_ene_bj, max_pos_ene_ct
    page_name <- switch(pag,
                        "gallito" = "g", "buscojobs" = "bj", "computrabajo" = "ct")
    page_mes <- switch (.mes_tj,
                        "1" = "ene", "2" = "feb", "3" = "mar", "4" = "abr", "5" = "may", "6" = "jun", "7" = "jul", "8" = "ag",
                        "9" = "set", "10" = "oct", "11" = "nov", "12" = "dic")
    name_pos = paste0("max_pos_", page_mes, "_", page_name)
    name_val = paste0("max_val_", page_mes, "_", page_name)
    temp_dt <- data.table(id_int = get_id(.dt = temp_mat), 
                          name_pos = get_max_id(.dt = temp_mat),
                          name_val = get_max_val(.dt = temp_mat))
    setnames(temp_dt, old = names(temp_dt), new = c("id_int", name_pos, name_val))
    if(k == 1) {
      temp <- cbind(temp, temp_dt)    
    } else {
      temp <- cbind(temp, temp_dt[, 2:3])
    }
  }
  return(temp)
}

temp_dt = NULL
for(p in c("gallito", "buscojobs", "computrabajo")) {
  for(l in c(1:12)) {
    for(j in l:(l+1)) {
      # if(j == 4)  next
      if(j == 13) next
      print(c(l, j))
      temp_dt <- rbindlist(list(temp_dt,
                                get_max(.dt = dt2, .pag_sel = p, .ano = 2019, .mes_ti = l, .mes_tj = j, .method = "cosine")),
                                use.names = TRUE, fill = TRUE)
    }
  }    
}
# Van a quedar todas las filas repetidas de esta forma, con huecos. Hay que colapsar en 1 sola fila.
temp_dt[, .N, keyby = id_int]
temp_dt[id_int == 1, ]

# Colapso las filas por id_int.
temp_dt <- dcast(
          data = (melt(temp_dt, id.vars = "id_int", variable.name = "var", value.name = "val")[
          !is.na(val), ]), 
          formula = id_int ~ var
          )
# Cuáles avisos de dt2 no están en dt?
dplyr::anti_join(dt2, temp_dt, by = "id_int") %>% 
  dplyr::count(ano)
# Los avisos de 2018 y 2020 que quedaron en dt2. Ok.

# Join.
# cols = names(temp_dt)[names(temp_dt) != "id_int"]
# dt2[temp_dt, (cols) := mget(cols), on = "id_int"]
# rm(temp_dt)
# Mejor hacer el join al revez, solo necesito ano, mes y página. Y si se quiere ver algún ejemplo text_comun.
cols <- c("ano", "mes", "pagina", "text_comun")
temp_dt[dt2, (cols) := mget(cols), on = "id_int"]

# Es necesario fijar un umbral para decir cuando están duplicados
pagina <-  c("gallito", "buscojobs", "computrabajo")
vars <- c("max_val_feb_g", "max_val_feb_bj", "max_val_feb_ct")
# vars <- c("max_val_mar_g", "max_val_mar_bj", "max_val_mar_ct")
par(mfrow = c(3,3))
for(p in pagina) {
  for(i in vars) {
    temp_dt[ano == 2019 & mes == 2 & pagina == p, density(get(i), from = 0, to = 1) 
        %>% plot(., main = paste(p, i), ylab = "Densidad")]    
  }
}
library(ggplot2)
k = 0
lista = list()
vars <- c("max_val_ene_g", "max_val_ene_bj", "max_val_ene_ct")
for(p in pagina) {
  for(i in vars) {
  k = k + 1  
  lista[[k]]  <- ggplot(dt2[ano == 2019 & mes == 1 & pagina == p], aes_string(x = i)) +
                  geom_histogram(aes(y=..density..)) +
                  geom_density(col="red") +
                  labs(x = "", y = "Densidad", title = paste(p, i))
  }
}
gridExtra::grid.arrange(grobs = lista, nrow = 3)

# Esta es la parte más importante. Calcular la cantidad de avisos repetidos entre páginas para cada mes.
# Idealmente debería ser un mes móvil, pero no tengo las fechas exactas.
# Lo mejor entonces es 2 meses móviles, Todos? No, solo tomo febrero y noviembre (feb-feb, feb-mar; non-nov, nov-dic)
# gallito-buscojobs; gallito-computrabajo; buscojobs-computrabajo
# Los máximos son asimétricos, hacia el 0. Por tanto, tomar la media no la mediana. o algún cuantil
# Tomar en cuenta si son del mismo departamento.
# Y la empresa

#
temp_dt[pagina == "gallito" & mes == 2, quantile(max_val_feb_g, probs = seq(0,1,0.1))]
temp_dt[pagina == "gallito" & mes == 2 & max_val_feb_g >= 0.6, .(pagina, max_pos_feb_g, max_val_feb_g, text_comun, id_int)] %>%
  View()
dt2[c(19351, 18705), .(puesto, empresa, text_comun, id_int, mes)]
dt2[c(18716, 18717), .(puesto, empresa, text_comun, id_int, mes)]
dt2[c(18703, 18720), .(puesto, empresa, text_comun, id_int, mes)]
dt2[c(19160, 18721), .(puesto, empresa, text_comun, id_int, mes)]
dt2[c(19117, 18767), .(puesto, empresa, text_comun, id_int, mes)]
dt2[c(19078, 18846), ]




#
library(evmix)
n=100
x = rgamma(n, shape = 1, scale = 2)
xx = seq(-0.5, 12, 0.01)
plot(xx, dgamma(xx, shape = 1, scale = 2), type = "l")
rug(x)
lines(xx, dbckden(xx, x, lambda = 1), lwd = 2, col = "red")
lines(density(x), lty = 2, lwd = 2, col = "green")
legend("topright", c("True Density", "Simple boundary correction",
                     "KDE using density function", "Boundary Corrected Kernels"),
       lty = c(1, 1, 2, 1), lwd = c(1, 2, 2, 1), col = c("black", "red", "green", "blue"))


n=100
x = rbeta(n, shape1 = 3, shape2 = 2)*5
xx = seq(-0.5, 5.5, 0.01)
plot(xx, dbeta(xx/5, shape1 = 3, shape2 = 2)/5, type = "l", ylim = c(0, 0.8))
rug(x)
lines(xx, dbckden(xx, x, lambda = 0.1, bcmethod = "beta2", proper = TRUE, xmax = 5),
      lwd = 2, col = "red")
lines(density(x), lty = 2, lwd = 2, col = "green")
legend("topright", c("True Density", "Modified Beta KDE Using evmix",
                     "KDE using density function"),
       lty = c(1, 1, 2), lwd = c(1, 2, 2), col = c("black", "red", "green"))


n=1000
x = rgamma(n, shape = 1, scale = 2)
xx = seq(-0.5, 15, 0.01)
plot(xx, dgamma(xx, shape = 1, scale = 2), type = "l")
rug(x)
lines(xx, dbckden(xx, x, lambda = 0.5, bcmethod = "simple", proper = TRUE),
      lwd = 2, col = "purple")
lines(xx, dbckden(xx, x, lambda = 0.5, bcmethod = "simple", proper = FALSE),
      lwd = 2, col = "red", lty = 2)
legend("topright", c("True Density", "Simple BC with renomalisation", 
                     "Simple BC without renomalisation"),
       lty = 1, lwd = c(1, 2, 2), col = c("black", "purple", "red"))
# https://rdrr.io/cran/evmix/man/bckden.html
