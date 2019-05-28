#' calculates optimum: fixed criteria for slope
#'
#' @keywords internal
calcular.optimo.criterio <- function(i.curva.map, i.criterio = 2.8) {
  x <- i.curva.map[, 1]
  y <- i.curva.map[, 2]
  y.s <- suavizado(y, 1)
  d.y <- diff(y.s)
  d.x <- x[2:length(x)]
  if (any(d.y < i.criterio)) {
    optimo <- min(d.x[d.y < i.criterio], na.rm = T) - 1
    optimo <- max(optimo, 1)
  } else {
    optimo <- max(d.x, na.rm = T)
  }
  resultados <- i.curva.map[x == optimo, ]
  datos <- data.frame(weeks = d.x, slope = d.y)
  return(list(resultados = resultados, datos = datos, umbral = i.criterio))
}
