#' path_length is used by conv_meas
#'
#' @keywords internal
path_length <- function(x, y) {
  sum(sqrt(diff(x)^2 + diff(y)^2))
}
