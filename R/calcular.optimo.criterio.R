#' calculates optimum: fixed criteria for slope
#'
#' @keywords internal
calcular.optimo.criterio<-function(i.curva.map, i.criterio=2.8){
  # y.100<-min((1:length(i.curva.map[,2]))[round(i.curva.map[,2],2)==100])
  # if (y.100==1){
  #   resultados<-i.curva.map[1,]  
  # }else{
  #   curva.map<-i.curva.map[1:y.100,]
  #   x<-curva.map[,1]
  #   y<-curva.map[,2]
  #   y.s<-suavizado(y,1)
  #   d.y<-diff(y.s)
  #   if (any(d.y<i.criterio)){
  #     optimo<-min((1:(length(x)-1))[d.y<i.criterio],na.rm=T)
  #   }else{
  #     optimo<-length(d.y)+1
  #   }
  #   resultados<-curva.map[x==optimo,]    
  # }
  x<-i.curva.map[,1]
  y<-i.curva.map[,2]
  y.s<-suavizado(y, 1)
  d.y<-diff(y.s)
  d.x<-x[2:length(x)]
  if (any(d.y<i.criterio)){
    optimo<-min(d.x[d.y<i.criterio], na.rm=T)-1
  }else{
    optimo<-max(d.x, na.rm=T)
  }
  resultados<-i.curva.map[x==optimo,]
  datos<-data.frame(weeks=d.x, slope=d.y)
  return(list(resultados=resultados, datos=datos, umbral=i.criterio))
}
