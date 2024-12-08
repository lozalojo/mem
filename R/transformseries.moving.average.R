#' moving average transformation
#'
#' @keywords internal
transformseries.moving.average <- function(x, i.number = 3) {
  x[is.nan(x)] <- NA
  x[is.infinite(x)] <- NA
  cx <- c(0, cumsum(ifelse(is.na(x), 0, x)))
  cn <- c(0, cumsum(ifelse(is.na(x), 0, 1)))
  rx <- cx[(i.number + 1):length(cx)] - cx[1:(length(cx) - i.number)]
  rn <- cn[(i.number + 1):length(cx)] - cn[1:(length(cx) - i.number)]
  rsum <- rx / rn
  radd <- length(x) - length(rsum)
  if (radd > 0) rsum <- c(rep(NA, radd), rsum)
  rsum[is.nan(rsum)] <- NA
  return(rsum)
}
