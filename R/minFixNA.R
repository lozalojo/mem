#' min function, removing Inf and -Inf
#'
#' @keywords internal
minFixNA <- function(minputdata) {
  minputdata[minputdata == Inf] <- NA
  minputdata[minputdata == -Inf] <- NA
  if (sum(!is.na(minputdata)) == 0) {
    return(NA)
  } else {
    return(min(minputdata, na.rm = TRUE))
  }
}
