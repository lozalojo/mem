#' generic confidence interval calculation function
#'
#' @keywords internal
iconfianza <- function(datos, nivel = 0.95, tipo = 1, ic = TRUE, tipo.boot = "normal", iteraciones.boot = 10000, colas = 2, use.t = FALSE) {
  datos[datos == -Inf] <- NA
  datos[datos == Inf] <- NA
  iconfres <- rep(NA, 3)
  if (tipo == 1) {
    iconfres <- iconfianza.aritmetica(datos, nivel, ic, colas, use.t)
  } else if (tipo == 2) {
    iconfres <- iconfianza.geometrica(datos, nivel, ic, colas, use.t)
  } else if (tipo == 3) {
    iconfres <- iconfianza.percentil.eqnpar(datos, 0.5, nivel, ic, colas)
  } else if (tipo == 4) {
    iconfres <- iconfianza.percentil.boot(datos, 0.5, nivel, ic, tipo.boot, iteraciones.boot, colas)
  } else if (tipo == 5) {
    iconfres <- iconfianza.x(datos, nivel, ic, colas, use.t)
  } else if (tipo == 6) {
    iconfres <- iconfianza.logx(datos, nivel, ic, colas, use.t)
  }
  iconfres
}
