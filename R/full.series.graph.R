#' Creates the historical series graph of the datasets
#'
#' Function \code{full.series.graph} creates a graph with the whole data.
#'
#' Input data must be a data.frame with each column a surveillance season and each
#' row a week.
#'
#' @name full.series.graph
#'
#' @param i.data Historical data series.
#' @param i.range.y Range y of graph.
#' @param i.output Directory where graph is saved.
#' @param i.graph.title Title of the graph.
#' @param i.graph.subtitle Subtitle of the graph.
#' @param i.graph.file Graph to a file.
#' @param i.graph.file.name Name of the graph.
#'
#' @return
#' \code{full.series.graph} writes a tiff graph of the full series of the dataset.
#'
#' @examples
#' # Castilla y Leon Influenza Rates data
#' data(flucyl)
#' # Data of the last season
#' full.series.graph(flucyl)
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
#' @importFrom graphics abline axis legend matplot mtext par points text lines plot
full.series.graph<-function(i.data,
                            i.range.y=NA,
                            i.output=".",
                            i.graph.title="",
                            i.graph.subtitle="",
                            i.graph.file=T,
                            i.graph.file.name=""){
  datos<-transformdata.back(i.data,"Rates",c(30,29))
  datos.x<-1:dim(datos)[1]
  datos.semanas<-as.numeric(datos$week)
  datos.anios<-as.numeric(datos$year)
  datos.y<-as.numeric(datos[,4])
  range.x<-range(datos.x,na.rm=T)

  if (i.graph.file.name=="") graph.name="series graph" else graph.name<-i.graph.file.name

  if (is.numeric(i.range.y)) range.y.bus<-i.range.y else range.y.bus<-c(0,max.fix.na(datos.y))
  otick<-optimal.tickmarks(range.y.bus[1],range.y.bus[2],10)
  range.y<-c(otick$range[1],otick$range[2]+otick$by/2)

  if (i.graph.file) tiff(filename=paste(i.output,"/",graph.name,".tiff",sep=""),width=8,height=6,units="in",pointsize="12",
                         compression="lzw",bg="white",res=300,antialias="none")

  opar<-par(mar=c(5,3,3,3)+0.1,mgp=c(3,0.5,0),xpd=T)

  #Plot the first time series. Notice that you don't have to draw the axis nor the labels
  plot(datos.x,datos.y,axes=F,xlab="",ylab="",
       type="l",
       col="#000000",
       main=i.graph.title,
       xlim=range.x,
       ylim=range.y,
       lty=1,
       col.main="#003366")
  # Puntos de la serie de tasas
  points(1:length(datos.x),datos.y,pch=19,type="p",col="#606060",cex=0.5)
  # Ejes
  axis(1,at=datos.x[datos.semanas %in% c(40,50,10,20,30)],
       labels=datos.semanas[datos.semanas %in% c(40,50,10,20,30)],cex.axis=0.7,
       col.axis="#404040",
       col="#C0C0C0")
  axis(1,at=datos.x[datos.semanas==24],tick=F,
       labels=datos.anios[datos.semanas==24],cex.axis=0.7,line=1,
       col.axis="#404040",
       col="#C0C0C0")
  mtext("Week",side=1,line=2.5,cex=0.8,col="#000040")

  axis(2,at=otick$tickmarks,lwd=1,cex.axis=0.6,col.axis="#404040",col="#C0C0C0")
  mtext(2,text="Weekly rate",line=1.3,cex=0.8,col="#000040")

  mtext(3,text=i.graph.subtitle,cex=0.8,col="#606060")

  mtext(4,text=paste("mem R library - Jos",rawToChar(as.raw(233))," E. Lozano - https://github.com/lozalojo/mem",sep=""),
        line=0.75,cex=0.6,col="#404040")
  par(opar)
  if (i.graph.file) dev.off()
  if (i.graph.file) cat("graph created: ",getwd(),"/",paste(i.output,"/",graph.name,".tiff",sep=""),"\n",sep="")
}
