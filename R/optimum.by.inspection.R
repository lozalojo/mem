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
#' @param i.param.values values to use in the i.param value of \code{memtiming}.
#' @param i.graph create a graph with the outputs (T/F).
#' @param i.graph.file write the graph to a file.
#' @param i.graph.file.name name of the output file.
#' @param i.graph.title title of the graph.
#' @param i.graph.subtitle subtitle of the graph.
#' @param i.output output directory.
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
#' #opt.ins<-optimum.by.inspection(flucyl,i.param.values=seq(2.0,3.0,0.1))
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
                                i.param.values=seq(1.5,4.5,0.1),
                                i.graph=T,
                                i.graph.file=F,
                                i.graph.file.name="",
                                i.graph.title="",
                                i.graph.subtitle="",
                                i.output="."){

  semanas<-dim(i.data)[1]
  anios<-dim(i.data)[2]
  nombre.semana<-rownames(i.data)
  nombre.anios<-colnames(i.data)
  numero.semana<-1:semanas
  n.values<-length(i.param.values)

  i.timing.1<-array(dim=c(anios,2))
  resultados.i<-array(dim=c(anios,14,n.values),
                      dimnames=list(year=names(i.data),indicator=LETTERS[1:14],parameter=i.param.values))

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
      i.timing.1.3<-readline("Is that correct? (type Y or N)\t")
      if (tolower(i.timing.1.3) %in% c("y","ye","yes")) itsnotok<-F
    }
    i.timing.1.i<-c(i.timing.1.1,i.timing.1.2)
    i.timing.1[i,]<-i.timing.1.i
    curva.map<-calcular.map(as.vector(as.matrix(cur)))
    for (j in 1:n.values){
        i.param.deteccion<-i.param.values[j]
        i.param.deteccion.label<-format(round(i.param.deteccion,1),digits=3,nsmall=1)
        i.timing.2<-calcular.optimo(curva.map,2,i.param.deteccion)[4:5]
        resultado.j<-calcular.indicadores.2.timings(cur,i.timing.1.i,i.timing.2,i.timing.labels=c("inspection",i.param.deteccion.label),
                                                i.graph.title="Comparing",i.graph.file=F)$indicadores
        resultados.i[i,,j]<-as.numeric(resultado.j)
    }
  }

  resultado<-data.frame(apply(resultados.i,c(3,2),sum,na.rm=T))
  # sensibilidad
  resultado[7]<-resultado[3]/(resultado[3]+resultado[6])
  # especificidad
  resultado[8]<-resultado[5]/(resultado[5]+resultado[4])
  # vpp
  resultado[9]<-resultado[3]/(resultado[3]+resultado[4])
  # vpn
  resultado[10]<-resultado[5]/(resultado[5]+resultado[6])
  # positive likehood ratio
  resultado[11]<-resultado[7]/(1-resultado[8])
  # negative likehood ratio
  resultado[12]<-(1-resultado[7])/resultado[8]
  # percentage agreement/accuracy
  resultado[13]<-(resultado[3]+resultado[5])/(resultado[3]+resultado[4]+resultado[5]+resultado[6])
  # Matthews correlation coefficient
  resultado[14]<-(resultado[3]*resultado[5]-resultado[4]*resultado[6])/sqrt((resultado[3]+resultado[4])*(resultado[3]+resultado[6])*(resultado[5]+resultado[4])*(resultado[5]+resultado[6]))

  resultado[resultado=="NaN"]<-NA

  resultados<-data.frame(value=i.param.values,resultado)
  names(resultados)<-c("value",tolower(colnames(resultado.j)))

  if (!any(!is.na(resultados$sensitivity)) | !any(!is.na(resultados$specificity))) {
    rankings.1<-NA
    rankings.2<-NA
    rankings.5<-NA
    optimo.1 <- NA
    optimo.2 <- NA
    optimo.5 <- NA
  } else {
    # rankings.1<-rank(resultados$sensitivity,na.last=F)+rank(resultados$specificity,na.last=F)
    # optimo.1<-i.param.values[which.max(rankings.1)]
    rankings.1 <- rank(-resultados$sensitivity, na.last = T) + rank(-resultados$specificity, na.last = T)
    optimo.1 <- i.param.values[which.min(rankings.1)]

    # rankings.2<-rank(resultados$sensitivity*resultados$specificity,na.last=F)
    # optimo.2<-i.param.values[which.max(rankings.2)]
    rankings.2 <- rank(-resultados$sensitivity * resultados$specificity, na.last = T)
    optimo.2 <- i.param.values[which.min(rankings.2)]

    qf <- abs(resultados$sensitivity - resultados$specificity)
    qe <- 2 - resultados$sensitivity - resultados$specificity
    qs <- (1 - resultados$sensitivity)^2 + (1 - resultados$specificity)^2

    rankings.5 <- rank(qf) + rank(qe) + rank(qs)
    optimo.5 <- i.param.values[which.min(rankings.5)]
  }
  if (!any(!is.na(resultados$positive.likehood.ratio))) {
    rankings.3<-NA
    optimo.3 <- NA
  } else {
    # rankings.3<-rank(resultados$positive.likehood.ratio,na.last=F)
    # optimo.3<-i.param.values[which.max(rankings.3)]
    rankings.3 <- rank(-resultados$positive.likehood.ratio, na.last = T)
    optimo.3 <- i.param.values[which.min(rankings.3)]
  }
  if (!any(!is.na(resultados$negative.likehood.ratio))) {
    rankings.4<-NA
    optimo.4 <- NA
  } else {
    # rankings.4<-rank(resultados$negative.likehood.ratio,na.last=F)
    # optimo.4<-i.param.values[which.max(rankings.4)]
    rankings.4 <- rank(-resultados$negative.likehood.ratio, na.last = T)
    optimo.4 <- i.param.values[which.min(rankings.4)]
  }
  if (!any(!is.na(resultados$percent.agreement))) {
    rankings.6 <- NA
    optimo.6 <- NA
  } else {
    # rankings.6<-rank(resultados$percent.agreement,na.last=F)
    # optimo.6<-i.param.values[which.max(rankings.6)]
    rankings.6 <- rank(-resultados$percent.agreement, na.last = T)
    optimo.6 <- i.param.values[which.min(rankings.6)]
  }

  if (!any(!is.na(resultados$matthews.correlation.coefficient))) {
    rankings.7 <- NA
    optimo.7 <- NA
  } else {
    rankings.7 <- rank(-resultados$matthews.correlation.coefficient, na.last = T)
    optimo.7 <- i.param.values[which.min(rankings.7)]
  }


  optimum <- data.frame(pos.likehood = optimo.3, neg.likehood = optimo.4, aditive = optimo.1, multiplicative = optimo.2,
                        mixed = optimo.5, percent = optimo.6, matthews=optimo.7)

  rankings <- data.frame(pos.likehood = rankings.3, neg.likehood = rankings.4, aditive = rankings.1, multiplicative = rankings.2,
                         mixed = rankings.5, percent = rankings.6, matthews=rankings.7)


  optimum.by.inspection.output <- list(optimum = optimum, rankings = rankings, insp.data = resultados,
                                       param.data = i.data, param.param.values = i.param.values)
  optimum.by.inspection.output$call <- match.call()

  # Graph all data
  if (i.graph){

    if (i.graph.file.name=="") graph.name="inspection analysis" else graph.name<-i.graph.file.name

    graph.title<-nombre.anios[i]
    if (i.graph.subtitle!="") graph.title<-paste(i.graph.subtitle," - ",graph.title,sep="")
    if (i.graph.title!="") graph.title<-paste(i.graph.title,"\n",graph.title,sep="")

    for (i in 1:anios){
      cur<-i.data[i]
      i.timing.1.i<-i.timing.1[i,]
      curva.map<-calcular.map(as.vector(as.matrix(cur)))
      i.param.deteccion<-optimum$matthews
      i.param.deteccion.label<-format(round(i.param.deteccion,1),digits=3,nsmall=1)
      i.timing.2<-calcular.optimo(curva.map,2,i.param.deteccion)[4:5]
      dummmmyyyy<-calcular.indicadores.2.timings(cur, i.timing.1.i, i.timing.2,
                                                 i.timing.labels=c("inspection",i.param.deteccion.label),
                                                 i.graph.title=graph.title,
                                                 i.graph.file=i.graph.file,
                                                 i.graph.file.name=paste(graph.name," - ",i,sep=""))
      if (!i.graph.file) dummmmyyyy<-readline("Press any key to continue\n")
    }
  }

  return(optimum.by.inspection.output)
}
