#' ROC analysis to find optimum parameter value
#'
#' Function \code{roc.analysis} perform a ROC analysis
#'
#' To be written
#'
#' @name roc.analysis
#'
#' @param i.data Data frame of input data.
#' @param i.seasons Maximum number of seasons to use.
#' @param i.type.threshold Type of confidence interval to calculate the threshold.
#' @param i.level.threshold Level of confidence interval to calculate the threshold.
#' @param i.tails.threshold Tails for the confidence interval to calculate the threshold.
#' @param i.type.intensity Type of confidence interval to calculate the intensity thresholds.
#' @param i.level.intensity Levels of confidence interval to calculate the intensity thresholds.
#' @param i.tails.intensity Tails for the confidence interval to calculate the threshold.
#' @param i.type.curve Type of confidence interval to calculate the modelled curve.
#' @param i.level.curve Level of confidence interval to calculate the modelled curve.
#' @param i.type.other Type of confidence interval to calculate length, start and percentages.
#' @param i.level.other Level of confidence interval to calculate length, start and percentages.
#' @param i.method Method to calculate the optimal timing of the epidemic.
#' @param i.param.values range of i.param values to test.
#' @param i.n.max Number of pre-epidemic values used to calculate the threshold.
#' @param i.type.boot Type of bootstrap technique.
#' @param i.iter.boot Number of bootstrap iterations.
#' @param i.calculation.method method of determining true/false positives and true/false negatives.
#' @param i.goodness.method method to calculate goodness.
#' @param i.detection.values values to use in the i.param value of \code{memtiming}.
#' @param i.weeks.above number of weeks over the threshold to give the alert.
#' @param i.output output directory for graphs.
#' @param i.graph whether the graphs must be written or not.
#' @param i.prefix prefix used for naming graphs.
#'
#' @return
#' \code{roc.analysis} returns a list.
#' An object of class \code{mem} is a list containing at least the following components:
#'   \item{optimum}{optimum value.}
#'   \item{results}{Detailed results of each iteration.}
#'
#' @examples
#' # Castilla y Leon Influenza Rates data
#' data(flucyl)
#' # Finds the timing of the first season: 2001/2002
#' repi<-roc.analysis(flucyl,i.param.values=seq(2.5,2.8,0.1),i.detection.values=seq(2.5,2.8,0.1))
#' repi
#'
#' @author Jose E. Lozano \email{lozalojo@@gmail.com}
#'
#' @references
#' Vega T., Lozano J.E. (2004) Modelling influenza epidemic - can we detect the beginning
#' and predict the intensity and duration? International Congress Series 1263 (2004)
#' 281-283.\cr
#' Vega T., Lozano J.E. (2012) Influenza surveillance in Europe: establishing epidemic
#' thresholds by the Moving Epidemic Method. Influenza and Other Respiratory Viruses,
#' DOI:10.1111/j.1750-2659.2012.00422.x.
#'
#' @keywords influenza
#'
#' @export
roc.analysis<-function(i.data,
                       i.seasons=10,
                       i.type.threshold=5,
                       i.level.threshold=0.95,
                       i.tails.threshold=1,
                       i.type.intensity=6,
                       i.level.intensity=c(0.40,0.90,0.975),
                       i.tails.intensity=1,
                       i.type.curve=2,
                       i.level.curve=0.95,
                       i.type.other=2,
                       i.level.other=0.95,
                       i.method=2,
                       i.param.values=seq(2.0,3.0,0.1),
                       i.n.max=-1,
                       i.type.boot="norm",
                       i.iter.boot=10000,
                       i.calculation.method="default",
                       i.goodness.method="cross",
                       i.detection.values=seq(2.0,3.0,0.1),
                       i.weeks.above=1,
                       i.output=".",
                       i.graph=F,
                       i.prefix=""){

#   i.data<-flucyl
#   i.seasons=10
#   i.type.threshold=5
#   i.level.threshold=0.95
#   i.tails.threshold=1
#   i.type.intensity=6
#   i.level.intensity=c(0.40,0.90,0.975)
#   i.tails.intensity=1
#   i.type.curve=2
#   i.level.curve=0.95
#   i.type.other=2
#   i.level.other=0.95
#   i.method=2
#   i.param.values=seq(2.0,3.0,0.1)
#   i.n.max=-1
#   i.type.boot="norm"
#   i.iter.boot=10000
#   i.calculation.method="default"
#   i.goodness.method="cross"
#   i.detection.values=seq(2.0,3.0,0.1)
#   i.weeks.above=1
#   i.output="."
#   i.graph=T
#   i.prefix=""

  n.values<-length(i.param.values)

  resultados<-array(dim=c(n.values,9))
  colnames(resultados)<-c("Parameter","Weeks","Non-missing weeks","True positives","False positives","True negatives","False negatives","Sensitivity","Specificity")

  for (i in 1:n.values){

    cat("[",format(round(100*(i-1)/n.values,1),digits=3,nsmall=1),"%][Parameter: ",format(round(i.param.values[i],1),digits=3,nsmall=1),"] Analysis started (",i," out of ",n.values,")\n",sep="")

    resultados[i,1]<-i.param.values[i]
    resultado.i<-memgoodness(i.data=i.data,
                            i.seasons=i.seasons,
                            i.type.threshold=i.type.threshold,
                            i.level.threshold=i.level.threshold,
                            i.tails.threshold=i.tails.threshold,
                            i.type.intensity=i.type.intensity,
                            i.level.intensity=i.level.intensity,
                            i.tails.intensity=i.tails.intensity,
                            i.type.curve=i.type.curve,
                            i.level.curve=i.level.curve,
                            i.type.other=i.type.other,
                            i.level.other=i.level.other,
                            i.method=i.method,
                            i.param=i.param.values[i],
                            i.n.max=i.n.max,
                            i.type.boot=i.type.boot,
                            i.iter.boot=i.iter.boot,
                            i.calculation.method=i.calculation.method,
                            i.goodness.method=i.goodness.method,
                            i.detection.values=i.detection.values,
                            i.weeks.above=i.weeks.above,
                            i.output=i.output,
                            i.graph=i.graph,
                            i.prefix=paste(i.prefix,"[",format(round(i.param.values[i],1),digits=3,nsmall=1),"] ",sep=""))
    resultados[i,2:9]<-resultado.i$results
  }

  qf<-abs(resultados[,8]-resultados[,9])
  qe<-2-resultados[,8]-resultados[,9]
  qs<-(1-resultados[,8])^2+(1-resultados[,9])^2

  rango1<-rank(qf)
  rango2<-rank(qe)
  rango3<-rank(qs)

  optimo<-resultados[min(rango1+rango2+rango3)==(rango1+rango2+rango3),]

  resultados<-cbind(resultados,qf,qe,qs,rango1,rango2,rango3)

  roc.analysis.output<-list(optimum=optimo,
                            roc.data=resultados,
                            param.data=i.data,
                            param.seasons=i.seasons,
                            param.type.threshold=i.type.threshold,
                            param.level.threshold=i.level.threshold,
                            param.tails.threshold=i.tails.threshold,
                            param.type.intensity=i.type.intensity,
                            param.level.intensity=i.level.intensity,
                            param.tails.intensity=i.tails.intensity,
                            param.type.curve=i.type.curve,
                            param.level.curve=i.level.curve,
                            param.type.other=i.type.other,
                            param.level.other=i.level.other,
                            param.method=i.method,
                            param.param.values=i.param.values,
                            param.n.max=i.n.max,
                            param.type.boot=i.type.boot,
                            param.iter.boot=i.iter.boot,
                            param.calculation.method=i.calculation.method,
                            param.goodness.method=i.goodness.method,
                            param.detection.values=i.detection.values,
                            param.weeks.above=i.weeks.above,
                            param.output=i.output,
                            param.graph=i.graph,
                            param.prefix=i.prefix)
  roc.analysis.output$call<-match.call()
  return(roc.analysis.output)
}
