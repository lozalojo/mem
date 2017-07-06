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
#' \itemize{
#' \item{1} {No transformation}
#' \item{2} {Odd}
#' \item{3} {Fill missing data}
#' \item{4} {Loess}
#' \item{5} {Two waves (observed)}
#' \item{6} {Two waves (expected)}
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
#' Vega T, Lozano JE, Ortiz de Lejarazu R, Gutierrez Perez M. Modelling influenza epidemic - can we
#' detect the beginning and predict the intensity and duration? Int Congr Ser. 2004 Jun;1263:281-3.
#'
#' Vega T, Lozano JE, Meerhoff T, Snacken R, Mott J, Ortiz de Lejarazu R, et al. Influenza surveillance
#' in Europe: establishing epidemic thresholds by the moving epidemic method. Influenza Other Respir
#' Viruses. 2013 Jul;7(4):546-58. DOI:10.1111/j.1750-2659.2012.00422.x.
#'
#' Vega T, Lozano JE, Meerhoff T, Snacken R, Beaute J, Jorgensen P, et al. Influenza surveillance in
#' Europe: comparing intensity levels calculated using the moving epidemic method. Influenza Other
#' Respir Viruses. 2015 Sep;9(5):234-46. DOI:10.1111/irv.12330.
#'
#' Lozano JE. lozalojo/mem: Second release of the MEM R library. Zenodo [Internet]. [cited 2017 Feb 1];
#' Available from: \url{https://zenodo.org/record/165983}. DOI:10.5281/zenodo.165983
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
