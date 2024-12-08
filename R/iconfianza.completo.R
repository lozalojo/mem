#' Int. Confianza de 2 y 1 cola, por encima y por debajo. De la media y el punto.
#'
#' @keywords internal
iconfianza.completo <- function(i.datos, tipo = 1, i.nivel = 0.95, use.t = FALSE) {
  datos <- as.numeric(i.datos)
  datos[datos == -Inf] <- NA
  datos[datos == Inf] <- NA
  n <- sum(!is.na(datos))
  iconfres <- list(media = NA)
  if (n != 0) {
    med <- mean(datos, na.rm = TRUE)
    if (tipo == 1) std <- sqrt(var(datos, na.rm = TRUE) / n) else std <- sqrt(var(datos, na.rm = TRUE))
    iconfres$media <- med
    if (use.t) pnor.2 <- qt(1 - (1 - i.nivel) / 2, n - 1) else pnor.2 <- qnorm(1 - (1 - i.nivel) / 2)
    if (use.t) pnor.1 <- qt(1 - (1 - i.nivel), n - 1) else pnor.1 <- qnorm(1 - (1 - i.nivel))
    iconfres$dos.colas <- c(med - pnor.2 * std, med + pnor.2 * std)
    iconfres$mayor <- c(med - pnor.1 * std, Inf)
    iconfres$menor <- c(-Inf, med + pnor.1 * std)
  }
  iconfres
}
