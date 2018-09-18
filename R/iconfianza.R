#' generic confidence interval calculation function
#'
#' @keywords internal
iconfianza<-function(datos, nivel=0.95, tipo=1, ic=T, tipo.boot="normal", iteraciones.boot=10000, colas=2, use.t=F){
  datos[datos==-Inf]<-NA
  datos[datos==Inf]<-NA
  if(tipo==1){
    return(iconfianza.aritmetica(datos, nivel, ic, colas, use.t))
  }else if(tipo==2){
    return(iconfianza.geometrica(datos, nivel, ic, colas, use.t))
  }else if(tipo==3){
    return(iconfianza.percentil.eqnpar(datos, 0.5, nivel, ic,colas))
  }else if(tipo==4){
    return(iconfianza.percentil.boot(datos, 0.5, nivel, ic, tipo.boot, iteraciones.boot, colas))
  }else if(tipo==5){
    return(iconfianza.x(datos, nivel, ic, colas, use.t))
  }else if(tipo==6){
    return(iconfianza.logx(datos, nivel, ic, colas, use.t))
  }
}
