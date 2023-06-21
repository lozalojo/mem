#' standarize data to [0,1] interval
#'
#' @keywords internal
normalizar <- function(normalizar.i.datos) {
  maximo <- maxFixNA(normalizar.i.datos)
  minimo <- minFixNA(normalizar.i.datos)
  resultado <- (normalizar.i.datos - minimo) / (maximo - minimo)
  return(resultado)
}
