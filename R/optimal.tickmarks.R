#' Find tickmarks for a given range of the y-axis that best fit an optimal number of tickmarks
#' you decide. f.i: what if i want to have a graph with 8 tickmarks in a range of 34 to 345
#'
#' @keywords internal
optimal.tickmarks<-function(i.min,i.max,i.number.ticks=10,
  i.valid.ticks=apply(expand.grid(c(1,2,2.5,5), 10^(-10:10)), 1, FUN = function(x) {x[1] * x[2]})){
  # Y ahora calculo el tickmark que más se acerca a esos 10 tickmarks objetivo.
  ticks.min<-floor(i.min/i.valid.ticks)
  ticks.max<-ceiling(i.max/i.valid.ticks)
  ticks.maxmin<-ticks.max-ticks.min+1
  n.valid.ticks<-length(i.valid.ticks)
  posicion.ticks<-(1:n.valid.ticks)[min(abs(ticks.maxmin-i.number.ticks))==abs(ticks.maxmin-i.number.ticks)][1]
  ini<-(ticks.min*i.valid.ticks)[posicion.ticks]
  fin<-(ticks.max*i.valid.ticks)[posicion.ticks]
  salto<-i.valid.ticks[posicion.ticks]
  # Rango
  range.y<-c(ini,fin)
  # Tickmarks
  tickmarks<-seq(ini,fin,salto)
  # El número de ticks resultante
  numero.ticks<-length(tickmarks)
  # Y la secuencia para definir el eje Y final
  return(list(by=salto,number=numero.ticks,range=range.y,tickmarks=tickmarks))
}

