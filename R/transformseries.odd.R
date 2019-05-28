#' odd transformation
#'
#' @keywords internal
transformseries.odd <- function(x) {
  x[is.nan(x)] <- NA
  x[is.infinite(x)] <- NA
  # odd transformation requires p between 0 and 1, if I have percentage I have to divide by 100
  # in case other unit is used, first i detect the units x10, x100, x1000, x10000...
  mults <- 0:25
  mult <- min(mults[10^mults >= max(x, na.rm = T)])
  # if a value is 1, odd is Inf, to avoid it, i replace 1 by half the way from the max value (except 1) to 1
  max.val.no.1 <- max(x[x < 10^mult], na.rm = T)
  x[x == 10^mult] <- (10^mult - max.val.no.1) / 2 + max.val.no.1
  # errdat<-!is.na(x) & (is.infinite(x) | x==10^mult)
  if (all(is.na(x))) xt <- x else xt <- x / (10^mult - x)
  # xts<-suavizado(xt, hsuav=-1)
  # xt[errdat]<-xts[errdat]
  return(xt)
}
