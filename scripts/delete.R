delete <- function(DT, del.idxs) { 
  varname = deparse(substitute(DT))
  
  keep.idxs <- setdiff(DT[, .I], del.idxs)
  cols = names(DT);
  DT.subset <- data.table(DT[[1]][keep.idxs])
  setnames(DT.subset, cols[1])
  
  for (col in cols[2:length(cols)]) 
  {
    DT.subset[, (col) := DT[[col]][keep.idxs]]
    DT[, (col) := NULL];  # delete
  }
  
  assign(varname, DT.subset, envir = globalenv())
  return(invisible())
}