#' @title Transformation of series of data
#'
#' @description
#' Function \code{transformseries} transforms whole datasets.
#'
#' @name transformseries
#'
#' @param i.data Historical data series.
#' @param i.transformation Transformation to apply to the dataset.
#'
#' @return
#' \code{transformseries} The transformed dataset.
#'
#' @details
#' Input data must be a data.frame with each column a surveillance season and each
#' row a week.
#'
#' Transformation options:
#'
#' \tabular{rlll}{
#' \tab [1] \tab No transformation\cr
#' \tab [2] \tab Odd\cr
#' \tab [3] \tab Fill missing data\cr
#' \tab [4] \tab Loess\cr
#' \tab [5] \tab Two waves (observed)\cr
#' \tab [6] \tab Two waves (expected)\cr
#' }
#'
#' Fill missings sustitute missing values with predicted values from a loess regression fit.
#' If does not impute leading or trailing missings, only missings in the middle of the season.
#'
#' Odd calculates the odd: p divided by 1-p.
#'
#' Loess substitute the dataset with predicted values from a loess regression fit.
#'
#' Two waves (observed) is used when there are two waves per season. It divides the original
#' dataset in two using a mixture of two normal distributions. The expected option uses the
#' same procedure but also substitutes all data with predicted values of the mixture fit.
#'
#' @examples
#' # Castilla y Leon Influenza Rates data
#' data(flucyl)
#' # Data of the last season
#' transformseries(flucyl,2)
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
transformseries<-function(i.data, i.transformation=1){
  if (is.null(i.data)){
    i.data.transf<-i.data
  }else if (is.null(i.transformation)){
    i.data.transf<-i.data
  }else if (is.na(i.transformation)){
    i.data.transf<-i.data
  }else{
    if (i.transformation==1){
      i.data.transf<-i.data
    }else if (i.transformation==2){
      # odd transformation requires p between 0 and 1, if I have percentage I have to divide by 100
      # in case other unit is used, first i detect the units x10, x100, x1000, x10000...
      mults<-0:25
      mult<-min(mults[10^mults>=max(i.data,na.rm=T)])
      i.data.transf<-data.frame(apply(i.data,2,transformseries.odd, mult=mult),stringsAsFactors = F)
      names(i.data.transf)<-names(i.data)
      rownames(i.data.transf)<-rownames(i.data)
    }else if (i.transformation==3){
      i.data.transf<-data.frame(apply(i.data,2,fill.missing),stringsAsFactors = F)
      names(i.data.transf)<-names(i.data)
      rownames(i.data.transf)<-rownames(i.data)
    }else if (i.transformation==4){
      i.data.transf<-data.frame(apply(i.data,2,suavizado,hsuav=-1),stringsAsFactors = F)
      names(i.data.transf)<-names(i.data)
      rownames(i.data.transf)<-rownames(i.data)
    }else if (i.transformation==5){
      i.data.transf<-transformseries.twowaves(i.data)$data.observed
      rownames(i.data.transf)<-rownames(i.data)
    }else if (i.transformation==6){
      i.data.transf<-transformseries.twowaves(i.data)$data.expected
      rownames(i.data.transf)<-rownames(i.data)
    }else{
      i.data.transf<-i.data
    }
  }
  return(i.data.transf)
}
