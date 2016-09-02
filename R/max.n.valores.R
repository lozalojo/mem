#' highest n values of a data set, removing Inf and -Inf
#'
#' @keywords internal
max.n.valores<-function(mis.datos,n.max=1){
  mis.datos[mis.datos==Inf]<-NA
  mis.datos[mis.datos==-Inf]<-NA
  ordenado<-sort(mis.datos,decreasing=T,na.last=T)
  n.datos<-length(mis.datos)
  if (n.max>0) resultado<-ordenado[1:min(n.max,n.datos)] else resultado<-ordenado
  resultado<-(c(resultado,rep(NA,n.max)))[1:n.max]
  return(resultado)
}
