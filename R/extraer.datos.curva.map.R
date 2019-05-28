#' Extract map curve
#'
#' @keywords internal
extraer.datos.curva.map <- function(i.epi, i.val) {
  # i.epi<-optimo
  # i.val<-duracion.media
  resultado <- as.data.frame(i.epi[[1]]$map.curve[i.epi[[1]]$map.curve[, 1] == i.val])
  m <- ncol(resultado)
  n <- length(names(i.epi))
  if (n > 1) {
    for (i in 2:n) {
      resultado <- cbind(resultado, i.epi[[i]]$map.curve[i.epi[[i]]$map.curve[, 1] == i.val])
    }
  }
  names(resultado) <- rep(names(i.epi), each = m)
  return(resultado)
}
