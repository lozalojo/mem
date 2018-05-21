#' smoothing regression function
#'
#' @keywords internal
#' @importFrom sm sm.regression h.select
suavizado<-function(i.datos, hsuav=-1){
  m<-length(i.datos)
  datosx<-(1:m)[!is.na(i.datos)]
  datosy<-i.datos[!(is.na(i.datos) | is.infinite(i.datos))]
  # When there is only one point, h.select produces an error
  if (length(datosx)>1){
    if (hsuav==-1) hsuav <- h.select(datosx, datosy)
    smr<-sm.regression(datosx,datosy,h=hsuav,display="none",eval.points=1:m)
    salida<-smr$estimate
    salida[salida<0]<-0
    salida[is.nan(salida)]<-NA    
  }else{
    salida <- i.datos
  }
  return(salida)
}