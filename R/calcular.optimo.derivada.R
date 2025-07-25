#' calculates optimum: second derivative equals 0 (change signs from - to +, or + to -)
#'
#' @keywords internal
calcular.optimo.derivada <- function(i.curva.map) {
  x <- i.curva.map[, 1]
  y <- i.curva.map[, 2]
  dloess <- data.frame(x = x, y = y)
  y.s <- loess(y ~ x, dloess)$fitted
  y.d <- diff(y.s)
  y.d2 <- diff(y.d)
  x.d2 <- seq_len(length(y.d2))
  y.d2.s <- sign(y.d2)
  cambio.signo <- abs(diff(y.d2.s))
  if (any(cambio.signo != 0)) {
    optimo <- 1 + which.max(cambio.signo)
  } else {
    optimo <- 1 + length(cambio.signo)
  }
  resultados <- i.curva.map[x == optimo, ]
  datos <- data.frame(weeks = x.d2, slope = y.d2)
  return(list(resultados = resultados, datos = datos, umbral = 0))
}
