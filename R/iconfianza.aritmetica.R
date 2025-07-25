#' confidence interval for the arithmetic mean using the normal approximation
#'
#' @keywords internal
iconfianza.aritmetica <- function(datos, nivel = 0.95, ic = TRUE, colas = 2, use.t = FALSE) {
  datos[datos == -Inf] <- NA
  datos[datos == Inf] <- NA
  n <- sum(!is.na(datos))
  if (n != 0) {
    if (use.t) pnor <- qt(1 - (1 - nivel) / colas, n - 1) * sqrt(1 + 1 / n) else pnor <- qnorm(1 - (1 - nivel) / colas)
    med <- mean(datos, na.rm = TRUE)
    std <- sqrt(var(datos, na.rm = TRUE))
    str <- std / sqrt(n)
    l.i <- med - pnor * str
    l.s <- med + pnor * str
  } else {
    med <- NA
    l.i <- NA
    l.s <- NA
  }
  if (ic) iconfres <- c(l.i, med, l.s) else iconfres <- rep(med, 3)
  iconfres
}
