#' @title Stability of indicators
#'
#' @description
#' Function \code{memstability} perform an analisys of stability of estimators
#'
#' @name memstability
#'
#' @param i.data Data frame of input data.
#' @param ... other parameters passed to memmodel.
#'
#' @return
#' \code{memstability} returns a list.
#' A list containing at least the following components:
#' \itemize{
#'   \item{stability.data} {data for each value analysed.}
#'   \item{stability.seasons} {seasons used for each iteration.}
#' }
#'
#' @details
#' Shows the stability of the main indicators according the number of seasons used to calculate them.
#' That allows to decide the minimum and maximum number of seasons to be included in the model for this
#' specific country and parameter.
#'
#' These indicators are calculated for the next upcoming season, so for all the iterations are used the
#' last number of seasons determined by the x-axis, so that 2 means 'last two seasons', 10 means
#' 'last ten seasons' and so on.
#'
#' The \code{stability.seasons} output shows data used to calculate each particular point in the graph.
#'
#' @examples
#' # Castilla y Leon Influenza Rates data
#' data(flucyl)
#' # Stability
#' stability<-memstability(flucyl)
#' stability$stability.data
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
memstability<-function(i.data, ...){
  anios<-dim(i.data)[2]
  semanas<-dim(i.data)[1]
  stability.data<-numeric()
  stability.seasons<-logical()

  if (anios<2){
    stability.data<-NULL
    stability.seasons<-NULL
  }else{
    for (i in 2:anios){
      indices.modelo<-(anios-i+1):anios
      datos.modelo<-memmodel(i.data[indices.modelo], i.seasons=NA, ...)
      stability.data.i<-c(datos.modelo$n.seasons,
        datos.modelo$ci.length[1,],
        datos.modelo$ci.start[1,],
        datos.modelo$ci.percent,
        datos.modelo$epidemic.thresholds,
        datos.modelo$intensity.thresholds)
      stability.data<-rbind(stability.data,stability.data.i)
      stability.seasons<-rbind(stability.seasons,1:anios %in% indices.modelo)
    rm("stability.data.i")
    }
    stability.data<-data.frame(stability.data, row.names=NULL, stringsAsFactors = F)
    names(stability.data)<-c("number","durationll","duration","durationul","startll","start","startul","percentagell","percentage","percentageul","epidemic","postepidemic","medium","high","veryhigh")
    rownames(stability.data)<-stability.data$number
    stability.seasons<-data.frame(stability.seasons, row.names=NULL, stringsAsFactors = F)
    names(stability.seasons)<-names(i.data)
    rownames(stability.seasons)<-stability.data$number
    stability.data$number<-NULL
  }
  memstability.output<-list(stability.data=stability.data,
                            stability.seasons=stability.seasons,
                          param.data=i.data)
  memstability.output$call<-match.call()
  return(memstability.output)
}

