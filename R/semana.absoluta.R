#' Transforms relative weeks to absolute weeks in a 1-52 normal season
#'
#' @keywords internal
semana.absoluta <- function(i.semana, i.semana.inicio) {
  return((i.semana <= 53 - i.semana.inicio) * (i.semana + i.semana.inicio - 1) + (i.semana > 53 - i.semana.inicio) * (i.semana + i.semana.inicio - 53))
}
