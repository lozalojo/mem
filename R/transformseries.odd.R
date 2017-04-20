#' fills in missing values inside the season with smoothing regression
#'
#' @keywords internal
transformseries.odd<-function(x, mult=0){
  cat("odd multiplier: ",mult,"\n")
  x[is.nan(x)]<-NA
  x[is.infinite(x)]<-NA
  if (all(is.na(x))) xt<-x else xt<-x/(10^mult-x)
  return(xt)
}
