#' odd transformation
#'
#' @keywords internal
transformseries.odd<-function(x, mult=0){
  x[is.nan(x)]<-NA
  errdat<-!is.na(x) & (is.infinite(x) | x==0)
  x[errdat]<-NA
  if (all(is.na(x))) xt<-x else xt<-x/(10^mult-x)
  xts<-suavizado(xt, hsuav=-1)
  xt[errdat]<-xts[errdat]
  return(xt)
}
