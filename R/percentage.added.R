#' For use with transformseries.multiple
#'
#' @keywords internal
#'
#' @importFrom RcppRoll roll_sum
percentage.added <- function(i.data, i.n, i.force.concave=T) {
  if (i.n > length(i.data)) n <- length(i.data) else n <- i.n
  i.data[is.na(i.data)] <- 0
  ldata <- length(i.data)
  # rs <- roll_sum(i.data, n)
  # covr <- roll_convrate(i.data, n)
  # rs.max <- which.max(adata$rs)
  # deno <- sum(i.data[rs.max:(rs.max + n - 1)])
  # convrate <- conv_meas(x = rs.max:(rs.max + n - 1), y = i.data[rs.max:(rs.max + n - 1)])
  covr <- isconcave <- rs <- NULL
  if (i.force.concave){
    adata <- data.frame(rs=roll_sum(i.data, n), covr=roll_convrate(i.data, n), start=1:(ldata-n+1), end=n:ldata) %>%
      mutate(isconcave=ifelse(!is.na(covr) & covr>1,1,0)) %>%
      arrange(-isconcave, -rs)
  }else{
    adata <- data.frame(rs=roll_sum(i.data, n), covr=NA, start=1:(ldata-n+1), end=n:ldata, isconcave=NA) %>%
      arrange(-rs)
  }
  rs.ini <- adata$start[1]
  rs.fin <- adata$end[1]
  rs.max <- rs.ini - 1 + which.max(i.data[rs.ini:rs.fin])
  deno <- adata$rs[1]
  convrate <- adata$covr[1]
  if (deno == 0) {
    per <- 0
  } else {
    per <- min(i.data[rs.ini:rs.fin]) / deno
  }  
  return(data.frame(percentage = per, start = rs.ini, end = rs.fin, n = n, sum = deno, max = rs.max, convrate = convrate, stringsAsFactors = FALSE))
}
