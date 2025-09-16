#' conv_meas Calculate the convex hull for both up and down being the "inside" of the curve and take the ratio 
#' of the path lengths. A value < 1 indicates convex (concave up), while a value > 1 indicates concave (concave down). 
#' A value exactly 1 indicates a line (or that it is no more convex than concave).
#'
#' @keywords internal
#' @importFrom grDevices chull
conv_meas <- function(x, y) {
  yrng <- range(y)
  xhull <- c(x, range(x)[2:1])
  chDown <- chull(xhull, c(y, rep(yrng[1], 2)))
  chUp <- chull(xhull, c(y, rep(yrng[2], 2)))
  chDown <- sort(chDown[chDown <= length(x)])
  chUp <- sort(chUp[chUp <= length(x)])
  path_length(x[chDown], y[chDown])/path_length(x[chUp], y[chUp])
}
