#' fills in missing values inside the season with smoothing regression
#'
#' @keywords internal
fill.missing<-function(i.datos){
  idatos.smo<-suavizado(i.datos,-1)
  donde.missing<-missings.inside(i.datos)
  idatos.fin<-i.datos
  idatos.fin[donde.missing]<-idatos.smo[donde.missing]
  return(idatos.fin)
}
