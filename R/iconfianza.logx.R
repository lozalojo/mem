#' confidence interval for a point (using log transformation and geometric mean)
#'
#' @keywords internal
iconfianza.logx<-function(datos,nivel=0.95,ic=T,colas=2){
  if (all(is.na(datos))){
    return(rep(NA,3))
  }else if (all(datos!=0,na.rm=T)){
    lx<-log(datos)
    med<-exp(iconfianza.x(lx,nivel,ic,colas))
    return(med)
  }else{
    datos.no.cero<-datos+1
    lx<-log(datos.no.cero)
    med<-exp(iconfianza.x(lx,nivel,ic,colas))-1
    return(med)
  }
}
