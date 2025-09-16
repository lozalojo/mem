#' conv_meas_y is 1 param version of conv_meas
#'
#' @keywords internal
conv_meas_y <- function(y) {
  x <- 1:length(y)
  conv_meas(x, y)
}