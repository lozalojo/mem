#' ROC analysis to find optimum parameter value
#'
#' Function \code{roc.analysis} perform a ROC analysis
#'
#' To be written
#'
#' @name roc.analysis
#'
#' @param i.data Data frame of input data.
#' @param i.param.values range of i.param values to test.
#' @param i.prefix.roc prefix used for naming graphs.
#' @param ... other paramaters to be used by memgoodness function.
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
#' # ROC analysis
#' epi.roc<-roc.analysis(flucyl,i.param.values=seq(2.5,2.8,0.1),i.detection.values=seq(2.5,2.8,0.1))
#' epi.roc$results
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
                       i.param.values=seq(2.0,3.0,0.1),
                       i.prefix.roc="",...){
  n.values<-length(i.param.values)
  resultados<-data.frame()
  
  for (i in 1:n.values){
    cat("[",format(round(100*(i-1)/n.values,1),digits=3,nsmall=1),"%][Parameter: ",format(round(i.param.values[i],1),digits=3,nsmall=1),"] Analysis started (",i," out of ",n.values,")\n",sep="")
    resultados.i<-data.frame(value=i.param.values[i],
                             t(memgoodness(i.data=i.data,i.method=2,i.param=i.param.values[i],
i.prefix=paste(i.prefix.roc,"[",format(round(i.param.values[i],1),digits=3,nsmall=1),"] ",sep=""))$results))
    resultados<-rbind(resultados,resultados.i)    
  }

  names(resultados)<-tolower(names(resultados))
  
  if (is.null(resultados$sensitivity) | is.null(resultados$specificity)){
    optimo.1<-NA
    optimo.2<-NA
    optimo.3<-NA
    optimo.4<-NA
    optimo.5<-NA
  }else{
    rankings.1<-order(resultados$sensitivity,decreasing = T)+order(resultados$specificity,decreasing = T)
    rankings.2<-order(resultados$sensitivity*resultados$specificity,decreasing = T)
    rankings.3<-order(resultados$positive.likehood.ratio,decreasing = T)
    rankings.4<-order(resultados$negative.likehood.ratio,decreasing = T)
    optimo.1<-i.param.values[which.min(rankings.1)]
    optimo.2<-i.param.values[which.min(rankings.2)]
    optimo.3<-i.param.values[which.min(rankings.3)]
    optimo.4<-i.param.values[which.min(rankings.4)]
    
    qf<-abs(resultados$sensitivity-resultados$specificity)
    qe<-2-resultados$sensitivity-resultados$specificity
    qs<-(1-resultados$sensitivity)^2+(1-resultados$specificity)^2
    rankings.5<-rank(qf)+rank(qe)+rank(qs)
    optimo.5<-i.param.values[which.min(rankings.5)]
  }
  
  optimum<-data.frame(pos.likehood=optimo.3,neg.likehood=optimo.4,aditive=optimo.1,multiplicative=optimo.2,mixed=optimo.5)
  
  roc.analysis.output<-list(optimum=optimum,
                            roc.data=resultados,
                            param.data=i.data,
                            param.param.values=i.param.values,
                            param.prefix=i.prefix.roc)
  roc.analysis.output$call<-match.call()
  return(roc.analysis.output)
}
