read_files <- function(.path, .pattern) {
  # La función toma como inputs el path de los archivos y el patrón de archivos que se quiere leer, ie, ".csv", ".rds"...
  # Se lee cada archivo en forma de data.table y se carga en una lista.
  # Se devuelve un data.table 
  archivos <- list.files(path = .path, pattern = .pattern, full.names = TRUE)
  lista = list()
  for (i in archivos) {
    lista[[i]] <- readRDS(i)
  }
  dt <- data.table::rbindlist(l = lista, use.names = TRUE, fill = TRUE, idcol = "file")
  return(dt)
}
