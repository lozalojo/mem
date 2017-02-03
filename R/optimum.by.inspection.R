#' Inspection calcultation of the optimum
#'
#' Function \code{optimum.by.inspection} perform an analysis of mem parameters
#' to find the one that fits better a panel of experts inspection criterium.
#'
#' To be written
#'
#' @name optimum.by.inspection
#'
#' @param i.data Data frame of input data.
#' @param i.detection.values values to use in the i.param value of \code{memtiming}.
#'
#' @return
#' \code{optimum.by.inspection} returns a list.
#' An object of class \code{mem} is a list containing at least the following components:
#'   \item{optimum}{optimum value.}
#'   \item{optimum.data}{Data related to the optimum value.}
#'   \item{summary.data}{Data for all values tested.}
#'   \item{inspection.data}{Detailed results of each iteration.}
#'
#' @examples
#' # Castilla y Leon Influenza Rates data
#' data(flucyl)
#' # Inspection. It runs interactively (uncomment to run)
#' #opt.ins<-optimum.by.inspection(flucyl,i.detection.values=seq(2.0,3.0,0.1))
#' #opt.ins$optimum.data
#'
#' @author Jose E. Lozano \email{lozalojo@@gmail.com}
#'
#' @references
#' Vega Alonso, Tomas, Jose E Lozano Alonso, Raul Ortiz de Lejarazu, and Marisol Gutierrez Perez. 2004. 
#' Modelling Influenza Epidemic: Can We Detect the Beginning and Predict the Intensity and Duration? 
#' International Congress Series, Options for the Control of Influenza V. Proceedings of the International 
#' Conference on Options for the Control of Influenza V, 1263 (June): 281-83. doi:10.1016/j.ics.2004.02.121.\cr
#' Vega, Tomas, Jose Eugenio Lozano, Tamara Meerhoff, Rene Snacken, Joshua Mott, Raul Ortiz de Lejarazu, and 
#' Baltazar Nunes. 2013. Influenza Surveillance in Europe: Establishing Epidemic Thresholds by the Moving 
#' Epidemic Method. Influenza and Other Respiratory Viruses 7 (4): 546-58. doi:10.1111/j.1750-2659.2012.00422.x.\cr
#' Vega, Tomas, Jose E. Lozano, Tamara Meerhoff, Rene Snacken, Julien Beaute, Pernille Jorgensen, Raul Ortiz 
#' de Lejarazu, et al. 2015. Influenza Surveillance in Europe: Comparing Intensity Levels Calculated Using 
#' the Moving Epidemic Method. Influenza and Other Respiratory Viruses 9 (5): 234-46. doi:10.1111/irv.12330.
#'
#' @keywords influenza
#'
#' @export
#' @importFrom graphics identify
optimum.by.inspection<-function(i.data,
                                i.detection.values=seq(1.5,4.5,0.1)){

  semanas<-dim(i.data)[1]
  anios<-dim(i.data)[2]
  nombre.semana<-rownames(i.data)
  nombre.anios<-colnames(i.data)
  numero.semana<-1:semanas
  n.values<-length(i.detection.values)

  i.timing.1<-array(dim=c(anios,2))
  resultados.i<-array(dim=c(anios,8,n.values),dimnames=list(year=names(i.data),indicator=LETTERS[1:8],parameter=i.detection.values))

  col.points<-c("#FF0000","#40FF40")
  col.points.alpha<-add.alpha(col.points,alpha=0.4)

  for (i in 1:anios){
    cur<-i.data[i]
    itsnotok<-T
    while(itsnotok){
      memsur<-memsurveillance(cur,NA,NA,i.graph.file=F,i.graph.title=nombre.anios[i],
                      i.range.x=as.numeric(rownames(cur))[c(1,semanas)])
      cat("Click on the FIRST epidemic week of this season\nDepending on the device used to plot the graph, you might need to click on FINISH (top-right corner)\n")
      i.timing.1.1<-identify(x=1:semanas,y=as.numeric(as.matrix(cur)),labels=nombre.semana,n=1,plot=F)
      if (is.numeric(i.timing.1.1)) points(x=i.timing.1.1,y=cur[i.timing.1.1,],pch=1,col=col.points.alpha[1],lwd=7)
      cat("Click on the LAST epidemic week of this season\nDepending on the device used to plot the graph, you might need to click on FINISH (top-right corner)\n\n")
      i.timing.1.2<-identify(x=1:semanas,y=as.numeric(as.matrix(cur)),labels=nombre.semana,n=1,plot=F)
      if (is.numeric(i.timing.1.2)) points(x=i.timing.1.2,y=cur[i.timing.1.2,],pch=1,col=col.points.alpha[2],lwd=7)
      cat("Epidemic week range selected is: [",nombre.semana[i.timing.1.1],",",nombre.semana[i.timing.1.2],"]\n\n")
      i.timing.1.3<-readline("Is that correct? (type Y or N)\n")
      if (tolower(i.timing.1.3) %in% c("y","ye","yes")) itsnotok<-F
    }
    i.timing.1.i<-c(i.timing.1.1,i.timing.1.2)
    i.timing.1[i,]<-i.timing.1.i
    curva.map<-calcular.map(as.vector(as.matrix(cur)))
    for (j in 1:n.values){
        i.param.deteccion<-i.detection.values[j]
        i.param.deteccion.label<-format(round(i.param.deteccion,1),digits=3,nsmall=1)
        i.timing.2<-calcular.optimo(curva.map,2,i.param.deteccion)[4:5]
        resultado.j<-calcular.indicadores.2.timings(cur,i.timing.1.i,i.timing.2,i.timing.labels=c("inspection",i.param.deteccion.label),
                                                i.graph.title="Comparing",i.graph.file=F)$indicadores
        resultados.i[i,,j]<-as.numeric(resultado.j)
    }
  }

  resultados<-cbind(i.detection.values,apply(resultados.i,c(3,2),sum,na.rm=T))
  colnames(resultados)<-c("parameter",colnames(resultado.j))
  resultados[,8]<-resultados[,4]/(resultados[,4]+resultados[,7])
  resultados[,9]<-resultados[,6]/(resultados[,6]+resultados[,5])


  qf<-abs(resultados[,8]-resultados[,9])
  qe<-2-resultados[,8]-resultados[,9]
  qs<-(1-resultados[,8])^2+(1-resultados[,9])^2

  rango1<-rank(qf)
  rango2<-rank(qe)
  rango3<-rank(qs)

  rango.t<-rango1+rango2+rango3

  optimo<-resultados[(1:n.values)[min(rango.t)==rango.t][1],]

  # Graph all data

  for (i in 1:anios){
    cur<-i.data[i]
    i.timing.1.i<-i.timing.1[i,]
    curva.map<-calcular.map(as.vector(as.matrix(cur)))
    i.param.deteccion<-optimo[1]
    i.param.deteccion.label<-format(round(i.param.deteccion,1),digits=3,nsmall=1)
    i.timing.2<-calcular.optimo(curva.map,2,i.param.deteccion)[4:5]
    dummmmyyyy<-calcular.indicadores.2.timings(cur,i.timing.1.i,i.timing.2,i.timing.labels=c("inspection",i.param.deteccion.label),
                                                  i.graph.title=nombre.anios[i],i.graph.file=F)
    dummmmyyyy<-readline("Press any key to continue\n")
    }


  resultados<-cbind(resultados,qf,qe,qs,rango1,rango2,rango3)

  optimum.by.inspection.output<-list(optimum=optimo[1],
                            optimum.data=optimo,
                            summary.data=resultados,
                            inspection.data=resultados,
                            param.data=i.data,
                            param.detection.values=i.detection.values)
  optimum.by.inspection.output$call<-match.call()
  return(optimum.by.inspection.output)
}
