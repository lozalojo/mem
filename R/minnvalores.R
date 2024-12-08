#' lowest n values of a data set, removing Inf and -Inf
#'
#' @keywords internal
minnvalores <- function(mis.datos, n.min = 1) {
  mis.datos[mis.datos == Inf] <- NA
  mis.datos[mis.datos == -Inf] <- NA
  ordenado <- sort(mis.datos, decreasing = FALSE)
  if (n.min > 0) resultado <- ordenado[1:n.min] else resultado <- ordenado
  return(resultado)
}
