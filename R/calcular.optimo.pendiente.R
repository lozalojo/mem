#' calculates optimum: slope matches the overall slope
#'
#' @keywords internal
calcular.optimo.pendiente <- function(i.curva.map) {
  x <- i.curva.map[, 1]
  y <- i.curva.map[, 2]
  dloess <- data.frame(x = x, y = y)
  y.s <- loess(y ~ x, dloess)$fitted
  x.range <- (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  y.range <- (max(y, na.rm = TRUE) - min(y, na.rm = TRUE))
  pendiente <- y.range / x.range
  y.d <- diff(y.s)
  x.d <- x[2:length(x)]
  optimo <- which.min(abs(y.d - pendiente))
  resultados <- i.curva.map[x == optimo, ]
  datos <- data.frame(weeks = x.d, slope = y.d)
  return(list(resultados = resultados, datos = datos, umbral = pendiente))
}
