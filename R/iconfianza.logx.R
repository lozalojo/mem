#' confidence interval for a point (using log transformation and geometric mean)
#'
#' @keywords internal
iconfianza.logx <- function(datos, nivel = 0.95, ic = TRUE, colas = 2, use.t = FALSE) {
  if (all(is.na(datos))) {
    iconfres <- rep(NA, 3)
  } else if (all(datos != 0, na.rm = TRUE)) {
    lx <- log(datos)
    med <- exp(iconfianza.x(lx, nivel, ic, colas, use.t))
    iconfres <- med
  } else {
    datos.no.cero <- datos + 1
    lx <- log(datos.no.cero)
    med <- exp(iconfianza.x(lx, nivel, ic, colas, use.t)) - 1
    iconfres <- med
  }
  iconfres
}
