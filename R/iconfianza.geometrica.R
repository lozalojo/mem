#' confidence interval for the geometric mean using the log-normal approximation
#'
#' @keywords internal
iconfianza.geometrica <- function(datos, nivel = 0.95, ic = TRUE, colas = 2, use.t = FALSE) {
  iconfres <- rep(NA, 3)
  if (all(is.na(datos))) {
    iconfres <- rep(NA, 3)
  } else if (all(datos != 0, na.rm = TRUE)) {
    lx <- log(datos)
    med <- exp(iconfianza.aritmetica(lx, nivel, ic, colas, use.t))
    iconfres <- med
  } else {
    datos.no.cero <- datos + 1
    lx <- log(datos.no.cero)
    med <- exp(iconfianza.aritmetica(lx, nivel, ic, colas, use.t)) - 1
    iconfres <- med
  }
  iconfres
}
