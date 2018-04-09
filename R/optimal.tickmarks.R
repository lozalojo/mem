#' Find tickmarks for a given range of the y-axis that best fit an optimal number of tickmarks
#' you decide. f.i: what if i want to have a graph with 8 tickmarks in a range of 34 to 345
#'
#' @keywords internal
# optimal.tickmarks<-function(i.min,i.max,i.number.ticks=10,
#                             i.valid.ticks=apply(expand.grid(c(1,2,2.5,5), 10^(-10:10)), 1, FUN = function(x) {x[1] * x[2]}),
#                             i.include.min=F,i.include.max=F){
#   # Y ahora calculo el tickmark que mas se acerca a esos 10 tickmarks objetivo.
#   # Opcion 1: libre, puedo poner ticks fuera de c(i.min,i.max)
#   if (!i.include.min){
#     ticks.min<-floor(i.min/i.valid.ticks)
#     ticks.max<-ceiling(i.max/i.valid.ticks)
#     ticks.maxmin<-ticks.max-ticks.min+1
#     n.valid.ticks<-length(i.valid.ticks)
#     posicion.ticks<-(1:n.valid.ticks)[min(abs(ticks.maxmin-i.number.ticks))==abs(ticks.maxmin-i.number.ticks)][1]
#     ini<-(ticks.min*i.valid.ticks)[posicion.ticks]
#     fin<-(ticks.max*i.valid.ticks)[posicion.ticks]
#     salto<-i.valid.ticks[posicion.ticks]
#     # Tickmarks
#     tickmarks<-seq(ini,fin,salto)
#     # El numero de ticks resultante
#     numero.ticks<-length(tickmarks)
#     # Rango
#     range.y<-c(ini,fin)
#   }else{
#     # Opcion 2: restringida, el primer tick se debe corresponder a i.min
#     ticks.maxmin<-1+floor((i.max-i.min)/i.valid.ticks)
#     n.valid.ticks<-length(i.valid.ticks)
#     posicion.ticks<-which.min(abs(ticks.maxmin-i.number.ticks))
#     ini<-i.min
#     fin<-i.min+((ticks.maxmin-1)*i.valid.ticks)[posicion.ticks]
#     salto<-i.valid.ticks[posicion.ticks]
#     # Tickmarks
#     tickmarks<-seq(ini,fin,salto)
#     # El numero de ticks resultante
#     numero.ticks<-length(tickmarks)
#     if (i.include.max) {
#       fin<-i.max
#       tickmarks[numero.ticks]<-i.max
#     }
#     # Rango
#     range.y<-c(ini,fin)
#   }
#   # Y la secuencia para definir el eje Y final
#   return(list(by=salto,number=numero.ticks,range=range.y,tickmarks=tickmarks))
# }
optimal.tickmarks<-function(i.min, i.max, i.number.ticks=10,
                            i.valid.ticks=apply(expand.grid(c(1,2,2.5,5), 10^(-10:10)), 1, FUN = function(x) {x[1] * x[2]}),
                            i.include.min=F, i.include.max=F){
  # Y ahora calculo el tickmark que más se acerca a esos 10 tickmarks objetivo.
  # Option 1: free, I can put tickmarks outside c(i.min,i.max)
  if (i.include.min) dif0<-i.min else dif0<-0
  e.min=i.min-dif0
  e.max=i.max-dif0
  ticks.min<-floor(e.min/i.valid.ticks)
  ticks.max<-ceiling(e.max/i.valid.ticks)
  ticks.maxmin<-ticks.max-ticks.min+1
  n.valid.ticks<-length(i.valid.ticks)
  posicion.ticks<-(1:n.valid.ticks)[min(abs(ticks.maxmin-i.number.ticks))==abs(ticks.maxmin-i.number.ticks)][1]
  ini<-(ticks.min*i.valid.ticks)[posicion.ticks]+dif0
  fin<-(ticks.max*i.valid.ticks)[posicion.ticks]+dif0
  salto<-i.valid.ticks[posicion.ticks]
  # Tickmarks
  tickmarks<-seq(ini,fin,salto)
  # Number of ticks
  numero.ticks<-length(tickmarks)
  if (i.include.max) {
    fin<-i.max
    tickmarks[numero.ticks] <- i.max
  }
  # Rank
  range.y<-c(ini,fin)
  # Returning
  return(list(by=salto,number=numero.ticks,range=range.y,tickmarks=tickmarks))
}
