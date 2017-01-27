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
#' @param i.range.x Range x (surveillance weeks) of graph.
#' @param i.range.y Range y of graph.
#' @param i.output Directory where graph is saved.
#' @param i.graph.title Title of the graph.
#' @param i.graph.subtitle Subtitle of the graph.
#' @param i.graph.file Graph to a file.
#' @param i.graph.file.name Name of the graph.
#' @param i.plot.timing Plot the timing of epidemics.
#' @param i.plot.intensity Plot the intensity levels.
#' @param i.color.pattern colors to use in the graph.
#' @param ... other parameters passed to memmodel.
#'
#' @return
#' \code{full.series.graph} writes a tiff graph of the full series of the dataset.
#'
#' Color codes:
#' 1: Axis.
#' 2: Tickmarks.
#' 3: Axis labels.
#' 4: Series line.
#' 5: Series dots (default).
#' 6: Title and subtitle.
#' 7: Series dots (pre-epidemic).
#' 8: Series dots (epidemic).
#' 9: Series dots (post-epidemic).
#' 10: Epidemic threshold.
#' 11: Medium threshold.
#' 12: High threshold.
#' 13: Very high threshold.
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
                            i.range.x=c(30,29),
                            i.range.y=NA,
                            i.output=".",
                            i.graph.title="",
                            i.graph.subtitle="",
                            i.graph.file=T,
                            i.graph.file.name="",
                            i.plot.timing=F,
                            i.plot.intensity=F,
                            i.color.pattern=c("#C0C0C0","#606060","#000000","#808080","#000000","#001933",
                                              "#00C000","#800080","#FFB401",
                                              "#8c6bb1","#88419d","#810f7c","#4d004b"),...){

  datos<-transformdata.back(i.data,i.name="rates",i.range.x=i.range.x,i.fun=sum)
  datos.x<-1:dim(datos)[1]
  semanas<-length(datos.x)
  datos.semanas<-as.numeric(datos$week)
  datos.temporadas<-datos$season
  datos.y<-as.numeric(datos[,names(datos)=="rates"])
  range.x<-range(datos.x,na.rm=T)

  epi<-memmodel(i.data,...)
  
  datos.fixed<-transformdata.back(epi$data,i.name="rates",i.range.x=i.range.x,i.fun=sum)
  datos.y.fixed<-as.numeric(datos.fixed[,names(datos.fixed)=="rates"])

  datos.missing<-datos.fixed
  datos.missing[!(is.na(datos) & !is.na(datos.fixed))]<-NA
  datos.y.missing<-as.numeric(datos.missing[,names(datos.missing)=="rates"])

  indices<-as.data.frame(epi$season.indexes[,,1])
  indices[is.na(epi$data)]<-NA
  
  rownames(indices)<-rownames(i.data)
  names(indices)<-names(i.data)
  datos.indexes<-transformdata.back(indices,i.name="rates",i.range.x=i.range.x,i.fun=function(x,...) if (all(is.na(x))) return(NA) else if (any(x==2,...)) return(2) else if (any(x==1,...)) return(1) else return(3))
  datos.y.indexes<-as.numeric(datos.indexes[,names(datos.indexes)=="rates"])
  
  intensity<-as.numeric(memintensity(epi)$intensity.thresholds)

  if (i.graph.file.name=="") graph.name="series graph" else graph.name<-i.graph.file.name

  if (is.numeric(i.range.y)){
    range.y.bus<-i.range.y
  }else if (i.plot.intensity){
    range.y.bus<-c(0,max.fix.na(c(datos.y,intensity)))
  }else{
    range.y.bus<-c(0,max.fix.na(datos.y))
  }
  otick<-optimal.tickmarks(range.y.bus[1],range.y.bus[2],10)
  range.y<-c(otick$range[1],otick$range[2]+otick$by/2)

  if (i.graph.file) tiff(filename=paste(i.output,"/",graph.name,".tiff",sep=""),width=8,height=6,units="in",pointsize="12",
                         compression="lzw",bg="white",res=300,antialias="none")

  opar<-par(mar=c(5,3,3,3)+0.1,mgp=c(3,0.5,0),xpd=T)

  #Plot the first time series. Notice that you don't have to draw the axis nor the labels
  matplot(datos.x,datos.y.fixed,axes=F,xlab="",ylab="",
       type="l",
       col=i.color.pattern[4],
       main=i.graph.title,
       xlim=range.x,
       ylim=range.y,
       lty=1,
       col.main=i.color.pattern[6])
  # Puntos de la serie de tasas
  if (i.plot.timing){
    if (i.color.pattern[7]==i.color.pattern[9]){
      etiquetas<-c("Non-epidemic","Epidemic")
      tipos<-c(NA,NA)
      anchos<-c(NA,NA)
      colores<-c(i.color.pattern[7:8])
      puntos<-c(19,19)
      colores.pt<-c(NA,NA)      
    }else{
      etiquetas<-c("Pre-epidemic","Epidemic","Post-epidemic")
      tipos<-c(NA,NA,NA)
      anchos<-c(NA,NA,NA)
      colores<-c(i.color.pattern[7:9])
      puntos<-c(19,19,19)
      colores.pt<-c(NA,NA,NA)      
    }
  # pre
  points(datos.x[datos.y.indexes==1],datos.y[datos.y.indexes==1],pch=19,type="p",col=i.color.pattern[7],cex=0.75)
  points(datos.x[datos.y.indexes==1],datos.y.missing[datos.y.indexes==1],pch=13,type="p",col=i.color.pattern[7],cex=0.75)
  # epi
  points(datos.x[datos.y.indexes==2],datos.y[datos.y.indexes==2],pch=19,type="p",col=i.color.pattern[8],cex=0.75)
  points(datos.x[datos.y.indexes==2],datos.y.missing[datos.y.indexes==2],pch=13,type="p",col=i.color.pattern[8],cex=0.75)
  # post
  points(datos.x[datos.y.indexes==3],datos.y[datos.y.indexes==3],pch=19,type="p",col=i.color.pattern[9],cex=0.75)
  points(datos.x[datos.y.indexes==3],datos.y.missing[datos.y.indexes==3],pch=13,type="p",col=i.color.pattern[9],cex=0.75)
  }else{
    etiquetas<-"Series"
    tipos<-1
    anchos<-1
    colores<-i.color.pattern[4]
    puntos<-21
    colores.pt<-i.color.pattern[5]
    #points(1:length(datos.x),datos.y,pch=21,bg="#FFFFFF",type="p",col=i.color.pattern[5],cex=0.5)
    points(datos.x,datos.y,pch=19,type="p",col=i.color.pattern[5],cex=0.75)
    points(datos.x,datos.y.missing,pch=13,type="p",col=i.color.pattern[5],cex=0.75)
  }
  if (i.plot.intensity){
    etiquetas<-c(etiquetas,"Epidemic thr","Medium thr","High thr","Very high thr")
    tipos<-c(tipos,2,2,2,2)
    anchos<-c(anchos,2,2,2,2)
    colores<-c(colores,i.color.pattern[10:13])
    puntos<-c(puntos,NA,NA,NA,NA)  
    colores.pt<-c(colores.pt,NA,NA,NA,NA)  
    lines(x=datos.x[c(1,semanas)],y=rep(intensity[1],2),lty=2,,lwd=2,col=i.color.pattern[10])
    lines(x=datos.x[c(1,semanas)],y=rep(intensity[2],2),lty=2,,lwd=2,col=i.color.pattern[11])
    lines(x=datos.x[c(1,semanas)],y=rep(intensity[3],2),lty=2,,lwd=2,col=i.color.pattern[12])
    lines(x=datos.x[c(1,semanas)],y=rep(intensity[4],2),lty=2,,lwd=2,col=i.color.pattern[13])
  }
  # Ejes
  posicion.temporadas<-as.numeric(rownames(i.data)[floor(dim(i.data)[1]/2)])
  axis(1,at=datos.x[datos.semanas %in% c(40,50,10,20,30)],tcl=-0.3,
       tick=T,
       labels=F,
       cex.axis=0.7,
       col.axis=i.color.pattern[2],col=i.color.pattern[1])
  axis(1,at=datos.x[datos.semanas %in% c(10,30,50)],las=3,
       tick=F,
       labels=datos.semanas[datos.semanas %in% c(10,30,50)],
       cex.axis=0.5,
       line=0.2,
       col.axis=i.color.pattern[2],col=i.color.pattern[1])
  axis(1,at=datos.x[datos.semanas %in% c(20,40)],las=3,
       tick=F,
       labels=datos.semanas[datos.semanas %in% c(20,40)],
       cex.axis=0.5,
       line=0.2,
       col.axis=i.color.pattern[2],col=i.color.pattern[1])
  
  axis(1,at=datos.x[datos.semanas %in% i.range.x],tcl=-0.3,
       tick=T,
       labels=F,
       cex.axis=0.7,
       line=1.7,
       col.axis=i.color.pattern[2],col=i.color.pattern[1])
  axis(1,at=datos.x[datos.semanas==posicion.temporadas],
       tick=F,
       labels=datos.temporadas[datos.semanas==posicion.temporadas],
       cex.axis=0.5,
       line=1,
       col.axis=i.color.pattern[2],col=i.color.pattern[1])
  axis(2,at=otick$tickmarks,
       lwd=1,
       cex.axis=0.6,
       col.axis=i.color.pattern[2],
       col=i.color.pattern[1])
  mtext(1,text="Week",line=2.5,cex=0.8,col=i.color.pattern[3])
  mtext(2,text="Weekly value",line=1.3,cex=0.8,col=i.color.pattern[3])
  mtext(3,text=i.graph.subtitle,cex=0.8,col=i.color.pattern[6])
  mtext(4,text=paste("mem R library - Jos",rawToChar(as.raw(233))," E. Lozano - https://github.com/lozalojo/mem",sep=""),
        line=0.75,cex=0.6,col="#404040")

  xa<-"topright"
  ya<-NULL
  legend(x=xa,y=ya,inset=c(0,-0.05),xjust=0,
         legend=rev(etiquetas),
         bty="n",
         lty=rev(tipos),
         lwd=rev(anchos),
         col=rev(colores),
         pch=rev(puntos),
         pt.bg=rev(colores.pt),
         cex=0.75,
         x.intersp=0.5,
         y.intersp=0.7,
         text.col="#000000",
         ncol=1)

  par(opar)
  if (i.graph.file) dev.off()
  #if (i.graph.file) cat("graph created: ",getwd(),"/",paste(i.output,"/",graph.name,".tiff",sep=""),"\n",sep="")
}
