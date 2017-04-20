#' evolution of estimators
#'
#' Function \code{memevolution} analyzes the evolution of mem estimators
#'
#' To be written
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
#'   \item{evolution.data}{data for each value analysed.}
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
                            datos.modelo$ci.start[2,],
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
                            datos.modelo$ci.start[2,],
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

