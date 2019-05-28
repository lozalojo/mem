#' calculates optimum generic function
#'
#' @keywords internal
calcular.optimo <- function(i.curva.map, i.metodo = 2, i.parametro = 2.8) {
  if (is.null(i.metodo)) i.metodo <- 2
  if (is.na(i.metodo)) i.metodo <- 2
  if (is.null(i.parametro)) i.parametro <- 2.8
  if (is.na(i.parametro)) i.parametro <- 2.8
  # Metodo 1: Original, segunda derivada
  if (i.metodo == 1) {
    temp1 <- calcular.optimo.original(i.curva.map)
  } # Metodo 3: Usando la pendiente de la derivada.
  else if (i.metodo == 3) {
    temp1 <- calcular.optimo.pendiente(i.curva.map)
  } # Metodo 4: Segunda derivada, igualando a 0
  else if (i.metodo == 4) {
    temp1 <- calcular.optimo.derivada(i.curva.map)
  } # Metodo 2: Usando un criterio de % sobre la pendiente
  else {
    temp1 <- calcular.optimo.criterio(i.curva.map, i.parametro)
  }
  resultados <- temp1$resultados
  datos <- temp1$datos
  umbral <- temp1$umbral
  return(list(resultados = resultados, datos = datos, umbral = umbral))
}
