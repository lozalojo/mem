#' highest n values of a data set, removing Inf and -Inf
#'
#' @keywords internal
max.n.valores<-function(mis.datos,n.max=1){
  mis.datos[mis.datos==Inf]<-NA
  mis.datos[mis.datos==-Inf]<-NA
  ordenado<-sort(mis.datos,decreasing=T)
  if (n.max>0) resultado<-ordenado[1:n.max] else resultado<-ordenado
  return(resultado)
}
