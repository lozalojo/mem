#' calculates optimum: original method: second derivative + axis change
#'
#' @keywords internal
calcular.optimo.original <- function(i.curva.map) {
  x <- i.curva.map[, 1]
  y <- i.curva.map[, 2]
  y.d <- diff(y)
  x.d <- x[2:length(x)]
  y.s <- suavizado(y.d)
  x.n <- normalizar(x.d)
  y.n <- normalizar(y.s)
  u <- (x.n - y.n) / sqrt(2)
  v <- sqrt(x.n^2 + y.n^2 - u^2)
  optimo <- which.min(v)
  resultados <- i.curva.map[x == optimo, ]
  datos <- data.frame(weeks = x.d, slope = y.s)
  return(list(resultados = resultados, datos = datos, umbral = NA))
}
