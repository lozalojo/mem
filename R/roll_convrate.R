#' roll_convrate is used by percentage.added
#'
#' @keywords internal
roll_convrate <- function(i.data, i.n){
  if (requireNamespace("data.table", quietly = TRUE)) {
    convr <- data.table::frollapply(i.data, i.n, conv_meas_y, align = "left")[1:(length(i.data)-i.n+1)]
  }else{
    convr <- numeric()
    for (i in 1:(length(i.data)-i.n+1)) convr <- c(convr, conv_meas(x = i:(i+i.n-1), y = i.data[i:(i+i.n-1)]))
  }
  convr
}

