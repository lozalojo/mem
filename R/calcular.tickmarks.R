#' Calculate optimal tickmarks for a graph
#'
#' @keywords internal
calcular.tickmarks<-function(
  # Este es el rango m?ximo del eje Y, calculado a partir del valor m?ximo de los datos que pinto, es decir, como max(x).
  maximo.y,
  # Este es el n?mero de tickmarks que pretendo que haya en cada gr?fico independientemente del rango del eje Y.
  ticks.objetivo=10,
  # Esto son los valores de tickmarks aceptados por m?, no voy a poner un rango de 3 en 3, por ejemplo, pero si de 1 en 1, de 2 en 2, de 5 en 5, de 10 en 10.
  ticks.validos=c(1,2,5,10,20,25,50,100,250,500,1000,1500,2000,2500,5000)){
  # Y ahora calculo el tickmark que m?s se acerca a esos 10 tickmarks objetivo.
  # Dos ordenes, primero el que m?s se acerque al objetivo de 10 tickmarks
  orden1<-abs(1+ceiling(maximo.y/ticks.validos)-ticks.objetivo)
  # Y segundo, si hay empate, por ejemplo cuando haya 7 y 13 tickmarks (los dos a 3 de los 10 tickmarks objetivo), selecciono el que menos tickmarks tiene.
  orden2<-1+ceiling(maximo.y/ticks.validos)-ticks.objetivo
  # Y aqui ya escojo el que est? en primera posici?n
  posicion.ticks<-ticks.validos[order(orden1,orden2)[1]]
  # El n?mero de ticks resultante
  numero.ticks<-1+ceiling(maximo.y/posicion.ticks)
  # Y la secuencia para definir el eje Y final
  range.y<-seq(0,ceiling(maximo.y/posicion.ticks)*posicion.ticks,posicion.ticks)
  return(list(posicion.ticks=posicion.ticks,numero.ticks=numero.ticks,range.y=range.y))
}
