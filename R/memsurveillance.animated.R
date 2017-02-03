#' Creates the animated surveillance graph of the current season
#'
#' Function \code{memsurveillance.animated} creates an animated surveillance graph for the current season.
#'
#' Input data must be the current season and an object of class \code{mem}. The output
#' graph contains the weekly rates series along with the epidemic and intensity threshols
#' located at the exact situation where the epidemic started. If there is no epidemic yet,
#' only the epidemic threshold is placed.
#'
#' @name memsurveillance.animated
#'
#' @param i.current Current season weekly rates.
#' @param i.epidemic.thresholds Pre and post epidemic threholds.
#' @param i.intensity.thresholds Intensity thresholds.
#' @param i.output Directory where graph is saved.
#' @param i.graph.file.name Name of the graph.
#' @param i.delay Delay between frames of the animated gif.
#' @param i.loop Number of loops for the animated dif, 0 for Infinite.
#' @param ... Additional parameters parsed to memsurveillance.
#'
#' @return
#' \code{memsurveillance.animated} writes a gif graph of the surveillance of this season.
#'
#' @examples
#' # Castilla y Leon Influenza Rates data
#' data(flucyl)
#' # Data of the last season
#' cur<-flucyl[8]
#' # The model
#' epi<-memmodel(flucyl[1:7])
#' # Epidemic thresholds
#' e.thr<-epi$epidemic.thresholds
#' # Intensity threhsolds
#' i.thr<-epi$intensity.thresholds
#' # Set the working directory to whererever you want to store the graph file
#' setwd(".")
#' m1<-a<-memsurveillance.animated(cur, i.graph.file.name="Animated",i.epidemic.thresholds = e.thr,
#' i.intensity.thresholds = i.thr, i.pos.epidemic = TRUE)
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
#' @importFrom grDevices dev.off rgb tiff
#' @importFrom graphics abline axis legend matplot mtext par points text lines
memsurveillance.animated<-function(i.current,
                                   i.epidemic.thresholds=NA,
                                   i.intensity.thresholds=NA,
                                   i.output=".",
                                   i.graph.file.name="",
                                   i.delay=100,
                                   i.loop=0,...){

  if (is.null(dim(i.current))) stop('Incorrect number of dimensions, input must be a data.frame.') else if (!(ncol(i.current)==1)) stop('Incorrect number of dimensions, only one season required.')

  y.max<-max(i.current,na.rm=T)

  if (!is.numeric(i.epidemic.thresholds) | length(i.epidemic.thresholds)==1) i.epidemic.thresholds<-rep(NA,2) else y.max<-max(max(i.epidemic.thresholds,na.rm=T),y.max)
  if (!is.numeric(i.intensity.thresholds) | length(i.intensity.thresholds)==1) i.intensity.thresholds<-rep(NA,3) else y.max<-max(max(i.intensity.thresholds,na.rm=T),y.max)
  for (i in 1:NROW(i.current)){
    temp1<-data.frame(name=i.current[1:i,])
    names(temp1)<-names(i.current)
    rownames(temp1)<-rownames(i.current)[1:i]
    memsurveillance(temp1,
                    i.epidemic.thresholds=i.epidemic.thresholds,
                    i.intensity.thresholds=i.intensity.thresholds,
                    i.graph.title = paste("Season: ",names(i.current),", Week: ",rownames(i.current)[i],sep=""), i.graph.file = TRUE,
                    i.graph.file.name=i, i.range.y = c(0,y.max),...)
    shell(paste("convert  \"",i,".tiff\" -resize 800x600 \"",i,".png\"",sep=""))
    file.remove(paste(i,".tiff",sep=""))
  }
  command<-paste("convert -delay ",i.delay," -loop ",i.loop,sep="")
  for (i in 1:NROW(i.current)) command<-paste(command," \"",i,".png\"",sep="")
  command<-paste(command, " \"Animated surveillance.gif\"",sep="")
  shell(command)
  for (i in 1:NROW(i.current)) file.remove(paste(i,".png",sep=""))
  if (i.graph.file.name=="") graph.name=paste(i.output,"/animated graph.gif",sep="") else graph.name<-paste(i.output,"/",i.graph.file.name,".gif",sep="")
  file.rename("Animated surveillance.gif",graph.name)
  cat(graph.name,"\n")
  memsurveillance.animated.output<-list(param.current=i.current,
                                        param.output=i.output,
                                        param.graph.file.name=i.graph.file.name,
                                        param.delay=i.delay,
                                        param.loop=i.loop)
  memsurveillance.animated.output$call<-match.call()
  return(memsurveillance.animated.output)
}
