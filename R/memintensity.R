#' Thresholds for influenza intensity
#'
#' Function \code{memintensity} is used to calculate the thresholds for influenza activity
#' using historical records (surveillance rates).\cr
#' This method is based on the Moving Epidemics Method (MEM) used to monitor influenza
#' activity in a weekly surveillance system.
#'
#' Input data is a data frame containing rates that represent historical influenza surveillance
#' data. It can start and end at any given week (tipically at week 40th), and rates can be
#' expressed as per 100,000 inhabitants (or per consultations, if population is not
#' available) or any other scale.\cr
#' MEM is used to locate the epidemic for each season. Then confidence intervals are
#' calculated at different levels.\cr
#' The parameter \code{i.levels}, define the three levels of the confidence intervals
#' used to calculate thresholds.\cr
#' The \code{i.n.max} parameter indicates how many epidemic values to use to calculate
#' the thresholds. A value of -1 indicates the program to use an appropiate number of
#' points depending on the number of seasons provided as input.\cr
#' The \code{i.seasons} parameter indicates how many seasons are used for calculating
#' thresholds. A value of -1 indicates the program to use as many as possible. If there
#' are less than this parameter, the program used all seasons avalaible.\cr
#' Intensity thresholds and Epidemic threshold defines 5 levels of intensity:\cr
#' \tabular{rlll}{
#' \tab \code{1} \tab Baseline level - Below epidemic threshold.\cr
#' \tab \code{2} \tab Low level - Above epidemic threshold and below low intensity
#'                    threshold.\cr
#' \tab \code{3} \tab Medium level - Above low intensity threshold and below medium
#'                    intensity threshold.\cr
#' \tab \code{4} \tab High level - Above medium intensity threshold and below high
#'                    intensity threshold.\cr
#' \tab \code{5} \tab Very high level - Above high intensity threshold.\cr
#' }
#'
#' @name memintensity
#'
#' @param i.flu An object of class \code{mem}.
#'
#' @return
#' \code{memintensity} returns a list with three objects, two of them are the parameters
#' used (\code{param.levels} and \code{param.seasons}) and the third one
#' (\code{intensity.thresholds}) is a matrix 1x4 with the epidemic and intensity thresholds.
#' \tabular{rlll}{
#' \tab \code{1} \tab Epidemic threshold.\cr
#' \tab \code{2} \tab Low intensity threshold.\cr
#' \tab \code{3} \tab Medium intensity threshold.\cr
#' \tab \code{4} \tab High intensity threshold.\cr
#' }
#'
#' @examples
#' # Castilla y Leon Influenza Rates data
#' data(flucyl)
#' # mem model
#' flucyl.mem<-memmodel(flucyl)
#' # Calculates intensity thresholds
#' intensity<-memintensity(flucyl.mem)
#' intensity
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
memintensity<-function(i.flu){
  intensity.thresholds<-matrix(c(i.flu$pre.post.intervals[1,3],i.flu$epi.intervals[,4]),ncol=4)
  colnames(intensity.thresholds)<-c("Epidemic",paste(c("Medium (","High (","Very high ("),as.character(round(i.flu$epi.intervals[,1]*100,1)),"%)",sep=""))
  rownames(intensity.thresholds)<-"Intensity Thresholds"
  memintensity.output<-list(intensity.thresholds=intensity.thresholds,
                            param.i.flu=i.flu)
  memintensity.output$call<-match.call()
  return(memintensity.output)
}
