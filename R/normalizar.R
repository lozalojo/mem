#' standarize data to [0,1] interval
#'
#' @keywords internal
normalizar <- function(normalizar.i.datos) {
  maximo <- max.fix.na(normalizar.i.datos)
  minimo <- min.fix.na(normalizar.i.datos)
  resultado <- (normalizar.i.datos - minimo) / (maximo - minimo)
  return(resultado)
}
