#' returns position of missing values inside the season. Leading and trailing missing values are not considered.
#'
#' @keywords internal
missings.inside<-function(a){
  ene<-length(a)
  nas<-!is.na(a)
  primer.no.na<-min((1:ene)[nas])
  ultimo.no.na<-max((1:ene)[nas])
  posiciones.na<-!nas
  if (primer.no.na>1) posiciones.na[1:(primer.no.na-1)]<-FALSE
  if (ultimo.no.na<ene) posiciones.na[(ultimo.no.na+1):ene]<-FALSE
  return(posiciones.na)
}
