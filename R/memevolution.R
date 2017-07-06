#' @title Evolution of estimators
#'
#' @description
#' Function \code{memevolution} analyzes the evolution of mem estimators
#'
#' @name memevolution
#'
#' @param i.data Data frame of input data.
#' @param i.evolution.seasons Maximum number of seasons to use.
#' @param i.evolution.method method to calculate evolution.
#' @param ... other parameters passed to memmodel.
#'
#' @return
#' \code{memevolution} returns a list.
#' A list containing at least the following components:
#' \itemize{
#'   \item{evolution.data} {data for each value analysed.}
#' }
#'
#' @details
#' Shows the evolution of some indicators across time. The indicators are: duration of the epidemic,
#' start of the epidemic, epidemic percentage, pre-epidemic, post-epidemic and intensity thresholds.
#'
#' Values per season are the estimates of the indicator for this specific season calculated according the
#' \link{memmodel} options: cross or sequential (validation option), maximum number of seasons to use in calculations.
#' In \link{memmodel} is it also possible to select the method used to calculate indicators and the level of
#' confidence of the confidence intervals.
#'
#' To clarify how it is calculated, the \code{evolution.seasons} showss for each x-axis point the seasons
#' selected to calculate thresholds. Each row is a different season and in each column it is shown if that
#' specific season has been used to calculate the indicators. If \code{TRUE}, it has been used, if \code{NO}, it hasn't.
#' In the last row, called Next, the indicators are calculated for the upcoming next season.
#'
#' Note that if you select sequential validation, first points in the graphs calculate thresholds
#' using less data (there are less seasons before the current data than the max. seasons value), and
#' this will affect the range of confidence intervals.
#'
#' There are four indicators:
#'
#' \itemize{
#' \item Duration: Average duration of the epidemic and its confidence interval.
#' \item Start: Average start of the epidemic and its confidence interval.
#' \item Epidemic percentage: The sum of the values (cases/rates) in the epidemic period
#' divided by the total sum of values of the whole surveillance period. It's a coverage percentage
#' of the epidemic period. And its confidence interval.
#' \item Thresholds: Pre-epidemic, post-epidemic and intensity thresholds.
#' }
#'
#' The confidence intervals for these indicators are calculated using the other CI and other CI.
#' level parameters of \link{memmodel}.
#'
#' @examples
#' # Castilla y Leon Influenza Rates data
#' data(flucyl)
#' # evolution of estimators
#' evolution<-memevolution(flucyl)
#' evolution$evolution.data
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
memevolution<-function(i.data, i.evolution.seasons=10, i.evolution.method="sequential", ...){

  anios<-dim(i.data)[2]
  semanas<-dim(i.data)[1]
  evolution.data<-numeric()
  evolution.seasons<-logical()

  if (is.na(i.evolution.seasons)) i.evolution.seasons<-anios
  if (is.null(i.evolution.seasons)) i.evolution.seasons<-anios

  if (anios<2){
    evolution.data<-NULL
    evolution.seasons<-NULL
  }else{
    if (!(i.evolution.method=="sequential")){
      # Metodo 2: cruzada
      for (i in 1:anios){
        indices.2<-(1:anios)-i
        indices.1<-abs(indices.2)
        indices.modelo<-order(indices.1,indices.2)[2:(i.evolution.seasons+1)]
        indices.modelo<-sort(indices.modelo[!is.na(indices.modelo)])
        datos.modelo<-memmodel(i.data[indices.modelo], i.seasons=NA, ...)
        evolution.data.i<-c(datos.modelo$n.seasons,
                            datos.modelo$ci.length[1,],
                            datos.modelo$ci.start[1,],
                            datos.modelo$ci.percent,
                            datos.modelo$epidemic.thresholds,
                            datos.modelo$intensity.thresholds)
        evolution.data<-rbind(evolution.data,evolution.data.i)
        evolution.seasons<-rbind(evolution.seasons,1:anios %in% indices.modelo)
        rm("evolution.data.i")
      }
      indices.modelo<-max(1,anios+1-i.evolution.seasons):anios
      datos.modelo<-memmodel(i.data[indices.modelo], i.seasons=NA, ...)
      evolution.data.i<-c(datos.modelo$n.seasons,
                          datos.modelo$ci.length[1,],
                          datos.modelo$ci.start[1,],
                          datos.modelo$ci.percent,
                          datos.modelo$epidemic.thresholds,
                          datos.modelo$intensity.thresholds)
      evolution.data<-rbind(evolution.data,evolution.data.i)
      evolution.seasons<-rbind(evolution.seasons,1:anios %in% indices.modelo)
      rm("evolution.data.i")
      evolution.seasons<-data.frame(evolution.seasons, row.names=NULL, stringsAsFactors = F)
      names(evolution.seasons)<-names(i.data)
      rownames(evolution.seasons)<-c(names(i.data),"next")
      evolution.data<-data.frame(evolution.data, row.names=NULL, stringsAsFactors = F)
      names(evolution.data)<-c("number","durationll","duration","durationul","startll","start","startul","percentagell","percentage","percentageul","epidemic","postepidemic","medium","high","veryhigh")
      rownames(evolution.data)<-c(names(i.data),"next")
    }else{
      for (i in 3:(anios+1)){
        indices.modelo<-max(1,i-i.evolution.seasons):(i-1)
        datos.modelo<-memmodel(i.data[indices.modelo], i.seasons=NA, ...)
        evolution.data.i<-c(datos.modelo$n.seasons,
                            datos.modelo$ci.length[1,],
                            datos.modelo$ci.start[1,],
                            datos.modelo$ci.percent,
                            datos.modelo$epidemic.thresholds,
                            datos.modelo$intensity.thresholds)
        evolution.data<-rbind(evolution.data,evolution.data.i)
        evolution.seasons<-rbind(evolution.seasons,1:anios %in% indices.modelo)
        rm("evolution.data.i")
      }
      evolution.seasons<-data.frame(evolution.seasons, row.names=NULL, stringsAsFactors = F)
      names(evolution.seasons)<-names(i.data)
      rownames(evolution.seasons)<-c(names(i.data)[3:anios],"next")
      evolution.data<-data.frame(evolution.data, row.names=NULL, stringsAsFactors = F)
      names(evolution.data)<-c("number","durationll","duration","durationul","startll","start","startul","percentagell","percentage","percentageul","epidemic","postepidemic","medium","high","veryhigh")
      rownames(evolution.data)<-c(names(i.data)[3:anios],"next")
    }
  }
  memevolution.output<-list(evolution.data=evolution.data,
                            evolution.seasons=evolution.seasons,
                            param.data=i.data,
                            param.evolution.seasons=i.evolution.seasons,
                            param.evolution.method=i.evolution.method)
  memevolution.output$call<-match.call()
  return(memevolution.output)
}

