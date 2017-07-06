#' @title Full process plots for mem
#'
#' @description
#' Function \code{processPlots} creates graphs of all mem process
#'
#' @name processPlots
#'
#' @param i.flu Object of class flu.
#' @param i.output Output directory.
#' @param i.prefix Prefix for all files to be output.
#'
#' @return
#' \code{processPlots} prints a set of graphs.
#'
#' @details
#' Create plots related to the process of calculating MEM indicators to an
#' output directory, showing the MAP curves and the slope of the MAP curve
#' and how the timing is calculated.
#'
#' @examples
#' # Castilla y Leon Influenza Rates data
#' data(flucyl)
#' # Graphs
#' epi<-memmodel(flucyl)
#' processPlots(epi)
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
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRamp colorRampPalette
#' @importFrom graphics polygon
#' @importFrom utils write.table
processPlots<-function(i.flu,i.output=".",i.prefix=""){

  i.flu$param.data<-i.flu$param.data[names(i.flu$param.data) %in% names(i.flu$data)]

  semanas<-dim(i.flu$data)[1]
  anios<-dim(i.flu$data)[2]
  if (!file.exists(i.output)) dir.create(i.output)
  salidas<-i.output
  if (!file.exists(salidas)) dir.create(salidas)

  # Output 0 - Summary of the results.

  sink(file=paste(salidas,"/",i.prefix,"Summary.txt",sep=""),append=FALSE)
  summary(i.flu)
  sink()

  write.table(round(i.flu$pre.post.intervals,4),file=paste(salidas,"/",i.prefix,"Summary.txt",sep=""),dec=",",append=TRUE,quote=FALSE,sep="\t",row.names=F,col.names=F)
  sink(file=paste(salidas,"/",i.prefix,"Summary.txt",sep=""),append=TRUE)
  cat("\n\nEstimated starting and ending point of the influenza season and its 95% CL\n\n")
  sink()
  write.table(i.flu$ci.start[1:2,],file=paste(salidas,"/",i.prefix,"Summary.txt",sep=""),dec=",",append=TRUE,quote=FALSE,sep="\t",row.names=F,col.names=F)
  sink(file=paste(salidas,"/",i.prefix,"Summary.txt",sep=""),append=TRUE)
  cat("\n\nLimits of the influenza season, relative and absolute position in weeks\n\n")
  sink()

  limites.temporada<-rbind(c(i.flu$mean.start,i.flu$mean.start+i.flu$mean.length-1),
                           c(semana.absoluta(i.flu$mean.start,i.flu$semana.inicio),semana.absoluta(i.flu$mean.start+i.flu$mean.length-1,i.flu$semana.inicio)))
  write.table(limites.temporada,file=paste(salidas,"/",i.prefix,"Summary.txt",sep=""),dec=",",append=TRUE,quote=FALSE,sep="\t",row.names=F,col.names=F)



  ## Epidemics for all flu seasons.

  for (j in 1:anios){
    opar<-par(mfrow=c(1,1))
    tiff(filename=paste(salidas,"/",i.prefix,"Epidemics ",j,".tiff",sep=""),width=8,height=6,units="in",pointsize="12",
         compression="lzw",bg="white",res=300,antialias="none")
    opar<-par(mfrow=c(1,1),mar=c(4,4,3,10) + 0.1,xpd=T)
    tempdatos<-as.data.frame(cbind(1:semanas,i.flu$data[,j]))
    names(tempdatos)<-c("Week","Rate")
    matplot(tempdatos[,1],
            tempdatos[,2],
            type="l",
            main=paste("Flu wave periods\n",names(i.flu$param.data)[j]),
            xlab="Week",
            ylab="Rate",
            col="#808080",
            lty=c(1,1),
            xaxt="n")
    axis(1,at=1:dim(tempdatos)[1],labels=rownames(i.flu$data),cex.axis=1)
    # pre
    puntos<-tempdatos
    puntos[i.flu$seasons.data[1,j,1]:semanas,2]<-NA
    points(puntos,pch=19,type="p",col="#00C000",cex=1.5)
    # epi
    puntos<-tempdatos
    if (i.flu$seasons.data[1,j,1]>1) puntos[1:(i.flu$seasons.data[1,j,1]-1),2]<-NA
    if (i.flu$seasons.data[2,j,1]<semanas) puntos[(i.flu$seasons.data[2,j,1]+1):semanas,2]<-NA
    points(puntos,pch=19,type="p",col="#980043",cex=1.5)
    # post
    puntos<-tempdatos
    puntos[1:i.flu$seasons.data[2,j,1],2]<-NA
    points(puntos,pch=19,type="p",col="#FFB401",cex=1.5)
    x<-semanas-10
    y<-max.fix.na(i.flu$param.data[,j])-1
    legend("topright",
           inset=c(-0.375,0),
           legend=c("Crude rate","Pre-epi period","Epidemic","Post-epi period"),
           lty=c(1,1,1,1),lwd=c(1,1,1,1),
           col=c("#808080","#C0C0C0","#C0C0C0","#C0C0C0"),
           pch=c(NA,21,21,21),
           pt.bg=c(NA,"#00C000","#980043","#FFB401"),
           cex=1)
    dev.off()
    par(opar)

    write.table(round(tempdatos,2),file=paste(salidas,"/",i.prefix,"Epidemics ",j,".txt",sep=""),dec=",",append=FALSE,quote=FALSE,sep="\t",row.names=F,col.names=T)

  }

  # Figure 2: MAP Curve

  for (j in 1:anios){

    tiff(filename=paste(salidas,"/",i.prefix,"MAP Curve ",j,".tiff",sep=""),width=8,height=6,units="in",pointsize="12",
         compression="lzw",bg="white",res=300,antialias="none")
    opar<-par(mfrow=c(1,1),mar=c(4,4,3,2) + 0.1,xpd=T)

    tempdatos<-i.flu$optimum[[j]]$map.curve

    matplot(tempdatos[,1],
            tempdatos[,2],
            type="l",col="#808080",lty=c(1,1),
            main=paste("MAP Curve\n",names(i.flu$param.data)[j]),
            xlab="Number of Weeks",ylab="Percentage")
    points(tempdatos[,1],
           tempdatos[,2],
           pch=19,type="p",col="#C0C0C0",cex=0.75)

    points(x=c(i.flu$optimum[[j]]$optimum.map[1],i.flu$optimum[[j]]$optimum.map[1]),
           y=c(i.flu$optimum[[j]]$optimum.map[2],tempdatos[1,2]),
           type="l",col="#FFB401",lwd=1)
    points(x=c(i.flu$optimum[[j]]$optimum.map[1],tempdatos[1,1]),
           y=c(i.flu$optimum[[j]]$optimum.map[2],i.flu$optimum[[j]]$optimum.map[2]),
           type="l",col="#FFB401",lwd=1)
    points(x=i.flu$optimum[[j]]$optimum.map[1],
           y=i.flu$optimum[[j]]$optimum.map[2],
           col="#980043",lwd=2.5)
    par(opar)
    dev.off()
    write.table(round(tempdatos,2),file=paste(salidas,"/",i.prefix,"MAP Curve ",j,".txt",sep=""),dec=",",append=FALSE,quote=FALSE,sep="\t",row.names=F,col.names=T)
  }

  # Figure 2.b : MAP Curve Slope for criterium 2

  if (i.flu$param.method==2){
    for (j in 1:anios){

      tiff(filename=paste(salidas,"/",i.prefix,"MAP Curve Slope ",j,".tiff",sep=""),width=8,height=6,units="in",pointsize="12",
           compression="lzw",bg="white",res=300,antialias="none")
      opar<-par(mfrow=c(1,1),mar=c(4,4,3,2) + 0.1,xpd=T)

      i.curva.map<-i.flu$optimum[[j]]$map.curve
      y.100<-min((1:length(i.curva.map[,2]))[round(i.curva.map[,2],2)==100])
      curva.map<-i.curva.map[1:y.100,]
      x<-curva.map[,1]
      y<-curva.map[,2]
      y.s<-suavizado(y,1)
      d.y<-diff(y.s)
      optimo<-i.flu$optimum[[j]]$optimum.map[1]

      matplot(x[-length(x)],d.y,
              type="l",col="#808080",lty=c(1,1),
              main=paste("MAP Curve Slope\n",names(i.flu$param.data)[j]),
              xlab="Number of Weeks",ylab="Slope")
      points(x[-length(x)],d.y,
             pch=19,type="p",col="#C0C0C0",cex=0.75)

      # Punto objetivo de pendiente
      points(x=c(0,length(x)-1),
             y=c(i.flu$param.param,i.flu$param.param),
             type="l",col="#980043",lwd=2.5,lty="dotted")
      # Donde se alcanza
      points(x=c(i.flu$optimum[[j]]$optimum.map[1],i.flu$optimum[[j]]$optimum.map[1]),
             y=c(0,max(d.y,na.rm=T)),
             type="l",col="#FFB401",lwd=1)
      points(x=c(0,length(x)-1),
             y=c(d.y[i.flu$optimum[[j]]$optimum.map[1]],d.y[i.flu$optimum[[j]]$optimum.map[1]]),
             type="l",col="#FFB401",lwd=1)
      points(x=i.flu$optimum[[j]]$optimum.map[1],
             y=d.y[i.flu$optimum[[j]]$optimum.map[1]],
             col="#980043",lwd=2)
      par(opar)
      dev.off()

      write.table(round(cbind(x[-length(x)],d.y),2),file=paste(salidas,"/",i.prefix,"MAP Curve Slope ",j,".txt",sep=""),dec=",",append=FALSE,quote=FALSE,sep="\t",row.names=F,col.names=T)
    }
  }


  # Figure 3 - Relative positions of the seasons.

  tempdatos<-as.data.frame(i.flu$season.scheme[,,1])
  names(tempdatos)<-c(names(i.flu$param.data),"Optimal","Period")
  write.table(tempdatos,file=paste(salidas,"/",i.prefix,"Relative Positions.txt",sep=""),dec=",",append=FALSE,quote=FALSE,sep="\t",row.names=F,col.names=T)
  tempdatos<-as.data.frame(i.flu$season.scheme[,,2])
  names(tempdatos)<-c(names(i.flu$param.data),"Optimal","Period")
  write.table(tempdatos,file=paste(salidas,"/",i.prefix,"Absolute Positions.txt",sep=""),dec=",",append=FALSE,quote=FALSE,sep="\t",row.names=F,col.names=T)

  ## Figure 4 - Weekly incidence rates of the seasons matching their relative position in the model.

  tempdatos<-as.data.frame(cbind(1:semanas,i.flu$moving.epidemics))
  names(tempdatos)<-c("Week",names(i.flu$param.data))
  write.table(round(tempdatos,2),file=paste(salidas,"/",i.prefix,"Moving Epidemics.txt",sep=""),dec=",",append=FALSE,quote=FALSE,sep="\t",row.names=F,col.names=T)
  tiff(filename=paste(salidas,"/",i.prefix,"Moving Epidemics.tiff",sep=""),width=8,height=6,units="in",pointsize="12",
       compression="lzw",bg="white",res=300,antialias="none")
  opar<-par(mfrow=c(1,1),mar=c(4,4,3,12) + 0.1,xpd=T)

  tipos<-rep(1,anios)
  anchos<-rep(2,anios)
  #colores<-c(rgb(runif(anios-1),runif(anios-1),runif(anios-1)),"#FF0000")
  pal <- colorRampPalette(brewer.pal(4,"Spectral"))
  colores<-pal(anios)
  #colores<-brewer.pal(anios,"Blues")
  #limite.superior<-c(0,50+max.fix.na(i.flu$moving.epidemics))
  limite.superior<-c(0,1.05*max.fix.na(i.flu$moving.epidemics))
  rango.superior<-limite.superior[2]-limite.superior[1]
  matplot(tempdatos[,1],tempdatos[,2:(anios+1)],
          type="l",lty=tipos,lwd=anchos,col=colores,
          main=paste("Weekly rates matching their relative position\n",substring(names(i.flu$param.data)[1],1,nchar(names(i.flu$param.data)[1])-12)),
          xlim=c(1,dim(i.flu$moving.epidemics)[1]),
          ylim=limite.superior,
          xlab="Week",ylab="Rate",
          font.axis=1,font.lab=1,font.main=2,font.sub=1,xaxt="n")
  axis(1,at=1:dim(tempdatos)[1],
       labels=rownames(i.flu$data),cex.axis=1)
  abline(v=c(limites.temporada[1,1]-0.5,limites.temporada[1,2]+0.5),
         col=c("#00C000","#FFB401"),lty=2,lwd=1.5)
  abline(h=i.flu$pre.post.intervals[1,3],
         col=c("#8c6bb1"),lty=2,lwd=1.5)

  y<-limite.superior[2]-rango.superior*0.025
  legend("topright",inset=c(-0.475,0),legend=names(i.flu$param.data),lty=tipos,lwd=anchos,col=colores,cex=0.75)

  par(opar)
  dev.off()

  # Figure 5 - Typical influenza epidemic period and its 95%CL.

  lineas.basicas<-array(dim=c(semanas,3))

  for (i in 1:(i.flu$mean.start-1)){
    lineas.basicas[i,]<-i.flu$pre.post.intervals[1,1:3]
  }

  for (i in ((i.flu$mean.start+i.flu$mean.length):semanas)){
    lineas.basicas[i,]<-i.flu$pre.post.intervals[2,1:3]
  }

  tempdatos<-as.data.frame(cbind(1:semanas,i.flu$typ.curve,lineas.basicas))
  names(tempdatos)<-c("Week","LowerCurve","MeanCurve","UpperCurve","LowerThreshold","MeanThreshold","UpperThreshold")
  write.table(round(tempdatos,2),file=paste(salidas,"/",i.prefix,"MEM Model v1.txt",sep=""),dec=",",append=FALSE,quote=FALSE,sep="\t",row.names=F,col.names=T)
  tiff(filename=paste(salidas,"/",i.prefix,"MEM Model v1.tiff",sep=""),width=8,height=6,units="in",pointsize="12",
       compression="lzw",bg="white",res=300,antialias="none")
  opar<-par(mfrow=c(1,1),mar=c(4,4,3,10) + 0.1,xpd=T)

  tipos<-c(1,1,1,0,0,2)
  anchos<-c(2,2,2,0,0,2)
  colores<-c("#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#8c6bb1")
  # Limites normales
  dgraf<-cbind(i.flu$typ.curve,lineas.basicas)
  nulos<-is.na(dgraf[,1]) | is.na(dgraf[,3])
  #limite.superior<-c(0,50+max.fix.na(dgraf))
  limite.superior<-c(0,1.05*max.fix.na(dgraf))
  matplot(1:semanas,dgraf,
          type="l",lty=tipos,lwd=anchos,col=colores,
          main=paste("Typical influenza epidemic\n",substring(names(i.flu$param.data)[1],1,nchar(names(i.flu$param.data)[1])-12)),
          xlim=c(1,dim(dgraf)[1]),xlab="Week",ylab="Rate",
          font.axis=1,font.lab=1,font.main=2,font.sub=1, ylim=limite.superior,xaxt="n")
  polygon(c((1:semanas)[!nulos],rev((1:semanas)[!nulos])),c(dgraf[!nulos,3],rev(dgraf[!nulos,1])),
          col="#e0e0e0", border = NA)
  points(1:semanas,dgraf[,2],col="#808080",type="l",lwd=2)

  axis(1,at=1:dim(tempdatos)[1],labels=rownames(i.flu$data),cex.axis=1)
  x<-4
  y<-dgraf[1,6]*1.2
  texto<-as.character(round(dgraf[1,6],digits=2))
  text(x,y,texto,font=2)
  x<-semanas-3
  y<-dgraf[limites.temporada[1,2]+1,6]*1.2
  texto<-as.character(round(dgraf[limites.temporada[1,2]+1,6],digits=2))
  text(x,y,texto,font=2)
  abline(v=c(limites.temporada[1,1]-0.5,limites.temporada[1,2]+0.5),col=c("#00C000","#FFB401"),lty=2)
  x<-semanas-10
  y<-max.fix.na(tempdatos)-1
  legend("topright",inset=c(-0.375,0),
         legend=c("Typical curve","Limits of the curve","Threshold","Epidemic start","Epidemic end"),
         lty=c(1,1,2,2,2),lwd=c(2,10,2,1,1),
         col=c("#808080","#e0e0e0","#8c6bb1","#00C000","#FFB401"),cex=0.75)
  par(opar)
  dev.off()

  # Fig 6, alternative version

  n.niveles<-dim(i.flu$epi.intervals)[1]
  limites.niveles<-as.vector(i.flu$epi.intervals[1:n.niveles,4])
  nombres.niveles<-as.character(i.flu$epi.intervals[1:n.niveles,1])
  limites.niveles[limites.niveles<0]<-0
  limites.niveles.mas<-array(dim=c(semanas,n.niveles))
  for (i in limites.temporada[1,1]:limites.temporada[1,2]) limites.niveles.mas[i,]<-limites.niveles
  tempdatos<-as.data.frame(cbind(1:semanas,i.flu$typ.curve[,2],lineas.basicas[,3],limites.niveles.mas))
  etiquetas<-paste(round(limites.niveles,2)," (",as.numeric(nombres.niveles)*100,"%) ",sep="")
  names(tempdatos)<-c("Week","Threshold",etiquetas)
  write.table(round(tempdatos,2),file=paste(salidas,"/",i.prefix,"MEM Model v2.txt",sep=""),dec=",",append=FALSE,quote=FALSE,sep="\t",row.names=F,col.names=T)
  tiff(filename=paste(salidas,"/",i.prefix,"MEM Model v2.tiff",sep=""),width=8,height=6,units="in",pointsize="12",
       compression="lzw",bg="white",res=300,antialias="none")
  opar<-par(mfrow=c(1,1),mar=c(4,4,3,10) + 0.1,xpd=T)

  tipos<-c(1,2,rep(2,times=n.niveles))
  anchos<-c(3,2,rep(2,times=n.niveles))
  if (n.niveles==3){
    colores<-c("#808080","#8c6bb1","#88419d","#810f7c","#4d004b")
  }else{
    colores<-c("#808080",brewer.pal(9,"BuPu")[(9-n.niveles):9])
  }

  # Limites normales
  par(mfrow=c(1,1))
  dgraf<-tempdatos[,-1]
  limite.superior<-c(0,1.05*max.fix.na(dgraf))
  #cat("Lineas:",dim(dgraf)[2],"\n")
  matplot(1:semanas,dgraf,type="l",
          main=paste("MEM Thresholds\n",substring(names(i.flu$param.data)[1],1,nchar(names(i.flu$param.data)[1])-12)),
          lty=tipos,lwd=anchos,col=colores,
          xlim=c(1,dim(dgraf)[1]),xlab="Week",ylab="Rate",
          font.axis=1,font.lab=1,font.main=2,font.sub=1,
          ylim=limite.superior,xaxt="n")
  axis(1,at=1:dim(tempdatos)[1],labels=rownames(i.flu$data),cex.axis=1)
  x<-c(limites.temporada[1,1]-5,1+rep(limites.temporada[1,2],n.niveles))
  y<-c(lineas.basicas[1,3],limites.niveles)
  #texto<-c(paste("Threshold: ",round(lineas.basicas[1,3],2),sep=""),etiquetas)
  texto<-c(paste(round(lineas.basicas[1,3],2),sep=""),etiquetas)
  posiciones<-c(3,rep(4,n.niveles))
  tamanios<-c(1.25,rep(1,n.niveles))
  text(x,y,texto,pos=posiciones,
       col=colores[-1],cex=tamanios)
  legend("topright",inset=c(-0.275,0),
         legend=rev(c("Typical curve","Epidemic thr","Medium thr","High thr","Very high thr")),
         lty=rev(tipos),lwd=rev(anchos),col=rev(colores),cex=0.75)

  par(opar)
  dev.off()

}
