#' For use with transformseries.multiple
#'
#' @keywords internal
#'
#' @importFrom RcppRoll roll_sum
percentage.added <- function(i.data, i.n) {
  if (i.n > length(i.data)) n <- length(i.data) else n <- i.n
  i.data[is.na(i.data)] <- 0
  rs <- roll_sum(i.data, n)
  rs.max <- which.max(rs)
  per <- min(i.data[rs.max:(rs.max + n - 1)]) / sum(i.data[rs.max:(rs.max + n - 1)])
  return(data.frame(percentage = per, start = rs.max, end = rs.max + n - 1, n = n, sum = sum(i.data[rs.max:(rs.max + n - 1)]), max = rs.max - 1 + which.max(i.data[rs.max:(rs.max + n - 1)]), stringsAsFactors = F))
}
