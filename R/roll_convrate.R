#' roll_convrate is used by percentage.added
#'
#' @keywords internal
roll_convrate <- function(i.data, i.n){
  ldata <- length(i.data)
  convr <- numeric()
  for (i in 1:(ldata-i.n+1)) convr <- c(convr, conv_meas(x = i:(i+i.n-1), y = i.data[i:(i+i.n-1)]))
  convr
}
