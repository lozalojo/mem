#' Find tickmarks for a given range of the y-axis that best fit an optimal number of tickmarks
#' you decide. f.i: what if i want to have a graph with 8 tickmarks in a range of 34 to 345
#'
#' @keywords internal
optimal.tickmarks<-function(minimo.y,maximo.y,ticks.objetivo=10,
  ticks.validos=c(1,2,5,10,20,25,50,100,200,250,500,1000,1500,2000,2500,5000,10000,20000,25000,50000)){
  # Y ahora calculo el tickmark que más se acerca a esos 10 tickmarks objetivo.
  ticks.min<-floor(minimo.y/ticks.validos)
  ticks.max<-ceiling(maximo.y/ticks.validos)
  ticks.maxmin<-ticks.max-ticks.min+1
  n.ticks.validos<-length(ticks.validos)
  posicion.ticks<-(1:n.ticks.validos)[min(abs(ticks.maxmin-ticks.objetivo))==abs(ticks.maxmin-ticks.objetivo)][1]
  ini<-(ticks.min*ticks.validos)[posicion.ticks]
  fin<-(ticks.max*ticks.validos)[posicion.ticks]
  salto<-ticks.validos[posicion.ticks]
  # Rango
  range.y<-c(ini,fin)
  # Tickmarks
  tickmarks<-seq(ini,fin,salto)
  # El número de ticks resultante
  numero.ticks<-length(range.y)
  # Y la secuencia para definir el eje Y final
  return(list(by=salto,number=numero.ticks,range=range.y,tickmarks=tickmarks))
}
