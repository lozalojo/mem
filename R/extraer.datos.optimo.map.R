#' Extract optimum
#'
#' @keywords internal
extraer.datos.optimo.map <- function(i.epi) {
  resultado <- as.data.frame(i.epi[[1]]$optimum.map)
  m <- ncol(resultado)
  n <- length(names(i.epi))
  if (n > 1) {
    for (i in 2:n) {
      resultado <- cbind(resultado, i.epi[[i]]$optimum.map)
    }
  }
  names(resultado) <- rep(names(i.epi), each = m)
  return(resultado)
}
