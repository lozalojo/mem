#' fills in missing values inside the season with smoothing regression
#'
#' @keywords internal
transformseries.odd<-function(x,mult){
  cat("odd multiplier: ",mult,"\n")
  x[is.nan(x)]<-NA
  x[is.infinite(x)]<-NA
  if (all(is.na(x))) xt<-x else xt<-x/(mult-x)
  return(xt)
}
