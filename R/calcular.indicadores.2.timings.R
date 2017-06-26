#' Calculates specificity and sensitivity
#'
#' @keywords internal
#' @importFrom graphics rect
calcular.indicadores.2.timings<-function(i.current,
                               i.timing.1,
                               i.timing.2,
                               i.timing.labels,
                               i.output=".",
                               i.graph.title="",
                               i.graph.file=F,
                               i.graph.file.name=""){

  if (!is.numeric(i.timing.1) | length(i.timing.1)!=2) stop('Incorrect use of this function. Timing 1 not of length 2.')
  if (!is.numeric(i.timing.2) | length(i.timing.2)!=2) stop('Incorrect use of this function. Timing 1 not of length 2.')

  semanas<-dim(i.current)[1]
  nombre.semana<-rownames(i.current)
  numero.semana<-1:semanas

  # M?todo 1: Mediante optimo mem, datos definitivos
  resultado.1<-rep(NA,semanas)
  if (i.timing.1[1]>1) resultado.1[1:(i.timing.1[1]-1)]<-1
  resultado.1[i.timing.1[1]:i.timing.1[2]]<-2
  if (i.timing.1[2]<semanas) resultado.1[(i.timing.1[2]+1):semanas]<-3
  resultado.1[is.na(i.current[,1])]<-0

  # M?todo 2: Mediante el criterio de los expertos
  resultado.2<-rep(NA,semanas)
  if (i.timing.2[1]>1) resultado.2[1:(i.timing.2[1]-1)]<-1
  resultado.2[i.timing.2[1]:i.timing.2[2]]<-2
  if (i.timing.2[2]<semanas) resultado.2[(i.timing.2[2]+1):semanas]<-3
  resultado.2[is.na(i.current[,1])]<-0

  resultado.3<-apply(rbind(resultado.1,resultado.2),2,comparar.metodos)

  true.pos<-sum(resultado.3=="TP",na.rm=T)
  false.neg<-sum(resultado.3=="FN",na.rm=T)
  false.pos<-sum(resultado.3=="FP",na.rm=T)
  true.neg<-sum(resultado.3=="TN",na.rm=T)

  if (true.pos+false.neg>0) sensibilidad<-true.pos/(true.pos+false.neg) else sensibilidad<-NA
  if (true.neg+false.pos>0) especificidad<-true.neg/(true.neg+false.pos) else especificidad<-NA
  if (true.pos+false.pos>0) ppv<-true.pos/(true.pos+false.pos) else ppv<-NA
  if (true.neg+false.neg>0) npv<-true.neg/(true.neg+false.neg) else npv<-NA
  pos.likehood.ratio<-NA
  if (!is.na(especificidad)) if (1-especificidad>0) pos.likehood.ratio<-sensibilidad/(1-especificidad) else pos.likehood.ratio<-NA
  neg.likehood.ratio<-NA
  if (!is.na(especificidad)) if (especificidad>0) neg.likehood.ratio<-(1-sensibilidad)/especificidad else neg.likehood.ratio<-NA
  if (true.pos+true.neg+false.pos+false.neg>0) percent.agreement<-(true.pos+true.neg)/(true.pos+true.neg+false.pos+false.neg) else percent.agreement<-NA
  if ((true.pos+false.pos)>0 & (true.pos+false.neg)>0 & (true.neg+false.pos)>0 & (true.neg+false.neg)>0) mcc<-(true.pos*true.neg-false.pos*false.neg)/(sqrt(true.pos+false.pos)*sqrt(true.pos+false.neg)*sqrt(true.neg+false.pos)*sqrt(true.neg+false.neg)) else mcc<-NA

  semanas.not.na<-sum(!is.na(i.current))

  indicadores<-data.frame(semanas=semanas,semanas.not.na=semanas.not.na,true.pos=true.pos,false.pos=false.pos,
                          true.neg=true.neg,false.neg=false.neg,sensibilidad=sensibilidad,especificidad=especificidad,
                          ppv=ppv,npv=npv,pos.likehood.ratio=pos.likehood.ratio,neg.likehood.ratio=neg.likehood.ratio,
                          percent.agreement=percent.agreement,mcc=mcc)

    dgraf<-as.data.frame(i.current)
    names(dgraf)<-c("Rate")

    colores.epi<-c("#00C000","#980043","#FFB401")

    range.x<-1:semanas

    # calculo el rango y para que tenga 10 marcas o este cerca

    maximo.y<-max.fix.na(dgraf)
    posicion.ticks<-optimal.tickmarks(0,maximo.y,10)$by
    range.y<-c(-1.5*posicion.ticks,ceiling(maximo.y/posicion.ticks)*posicion.ticks)
    range.y.seq<-seq(0,ceiling(maximo.y/posicion.ticks)*posicion.ticks,posicion.ticks)

    if (i.graph.file.name=="") graph.name<-"surveillance graph" else graph.name<-i.graph.file.name

    if (i.graph.file==T) tiff(filename=paste(i.output,"/",graph.name,".tiff",sep=""),width=8,height=6,units="in",pointsize="12",
           compression="lzw",bg="white",res=300,antialias="none")

    etiquetas<-c("Weekly rates")
    tipos<-c(1)
    anchos<-c(3)
    colores<-c("#808080")

      opar<-par(mar=c(4,4,3,8)+0.1,xpd=TRUE)
      #,mgp=c(3,0.5,0),xpd=T)
      # Grafico principal
      matplot(range.x,
              dgraf,
              type="l",
              lty=tipos,lwd=anchos,col=colores,
              xlab="",ylab="",axes=F,
              ylim=range.y,main=i.graph.title)
      # Puntos de la serie de tasas
      #points(1:semanas,dgraf[,1],pch=19,type="p",col="#000000",cex=1)
      # pre
      puntos.1<-i.current[,1]
      puntos.1[!(resultado.1==1)]<-NA
      points(range.x,puntos.1,pch=19,type="p",col=colores.epi[1],cex=1.5)
      # epi
      puntos.2<-i.current[,1]
      puntos.2[!(resultado.1==2)]<-NA
      points(range.x,puntos.2,pch=19,type="p",col=colores.epi[2],cex=1.5)
      # post
      puntos.3<-i.current[,1]
      puntos.3[!(resultado.1==3)]<-NA
      points(range.x,puntos.3,pch=19,type="p",col=colores.epi[3],cex=1.5)

      # Ejes
      axis(2,at=range.y.seq,lwd=1,cex.axis=0.6,col.axis="#404040",col="#C0C0C0",mgp=c(3, 0.5, 0))
      mtext(2,text="Weekly rate",line=2,cex=0.8,col="#000040")
      axis(1,pos=0,at=seq(1,semanas,1),labels=F,cex.axis=0.7,col.axis="#404040",col="#C0C0C0")
      axis(1,at=seq(0.5,semanas+0.5,1),labels=F,cex.axis=0.7,col.axis="#404040",col="#C0C0C0")
      axis(1,at=seq(1,semanas,2),tick=F,mgp=c(3, 0.5, 0),
           labels=nombre.semana[seq(1,semanas,2)],cex.axis=0.6,col.axis="#404040",col="#C0C0C0")
      axis(1,at=seq(2,semanas,2),tick=F,mgp=c(3, 0.5, 0),
           labels=nombre.semana[seq(2,semanas,2)],cex.axis=0.6,line=0.60,col.axis="#404040",col="#C0C0C0")
      mtext(1,text="Week",line=2.5,cex=0.8,col="#000040")
      mtext(4,text=paste("mem R library - Jose E. Lozano - https://github.com/lozalojo/mem",sep=""),
            line=7,cex=0.6,col="#404040")

      # Etiquetas de la leyenda

      etiquetas.leyenda<-c("Pre","Epidemic","Post",etiquetas)
      tipos.leyenda<-c(1,1,1,tipos)
      anchos.leyenda<-c(1,1,1,anchos)
      colores.leyenda<-c("#C0C0C0","#C0C0C0","#C0C0C0",colores)
      puntos.leyenda<-c(21,21,21,rep(NA,1))
      bg.leyenda<-c("#FFFFFF","#FFFFFF","#FFFFFF",rep(NA,1))
      pt.bg.leyenda<-c(colores.epi,rep(NA,1))

      legend("topright",inset=c(-0.25,0),
             legend=rev(etiquetas.leyenda),
             bty="n",
             lty=rev(tipos.leyenda),
             lwd=rev(anchos.leyenda),
             col=rev(colores.leyenda),
             pch=rev(puntos.leyenda),
             bg=rev(bg.leyenda),
             pt.bg=rev(pt.bg.leyenda),
             cex=0.9,
             text.col="#000000",
             ncol=1
      )

      colores.cuadros<-c("#C0C0C0",colores.epi)
      rect(seq(0.5,semanas-0.5,1),
           -3*posicion.ticks/2,
           seq(1.5,(semanas+0.5),1),
           -2*posicion.ticks/2,
           density=NULL,
           col=colores.cuadros[1+resultado.2],
           border="white")
      rect(seq(0.5,semanas-0.5,1),
           -2*posicion.ticks/2,
           seq(1.5,(semanas+0.5),1),
           -1*posicion.ticks/2,
           density=NULL,
           col=colores.cuadros[1+resultado.1],
           border="white")

      text.xy<-expand.grid(x=seq(1,semanas,1),y=seq(-2.5,-1.5,1)*posicion.ticks/2)
      text.l<-c(resultado.3,rep(NA,semanas))
      text.p<-rep(1,semanas*2)
      text.s<-rep(0.25,semanas*2)
      text.c<-rep("#FFFFFF",semanas*2)
      text(text.xy$x,text.xy$y,text.l,col=text.c,cex=text.s)

      text(semanas,-3.5*posicion.ticks/2,pos=2,label=paste("Sensitivity: ",format(round(sensibilidad,2),nsmall=2,align="right"),", Specificity: ",format(round(especificidad,2),nsmall=2,align="right"),sep=""),cex=0.5)

      axis(2,at=seq(-2.5,-1.5,1)*posicion.ticks/2,
           labels=rev(i.timing.labels),
           tick=F,
           las=1,
           lwd=1,
           cex.axis=0.6,col.axis="#404040",col="#C0C0C0",mgp=c(3, 0, 0))

      # Etiquetas de la leyenda
      etiquetas.leyenda<-c("Pre","Epidemic","Post")
      tipos.leyenda<-c(0,0,0)
      anchos.leyenda<-c(0,0,0)

      colores.leyenda<-c("#C0C0C0","#C0C0C0","#C0C0C0")
      puntos.leyenda<-c(21,21,21)
      bg.leyenda<-c("#FFFFFF","#FFFFFF","#FFFFFF")
      pt.bg.leyenda<-c("#00C000","#980043","#FFB401")
      legend("bottomright",inset=c(-0.18,0.025),
             x.intersp=-1,
             legend=rev(etiquetas.leyenda),
             bty="n",
             lty=rev(tipos.leyenda),
             lwd=rev(anchos.leyenda),
             fill=pt.bg.leyenda,
             cex=0.9,
             text.col="#000000",
             ncol=1
      )

      par(opar)
      if (i.graph.file==T){
        dev.off()
        cat("graph created: ",getwd(),"/",i.output,"/",graph.name,".tiff","\n",sep="")
      }

  names(indicadores)<- c("weeks","non.missing.weeks","true.positives","false.positives",
                                                 "true.negatives","false.negatives","sensitivity","specificity",
                                                 "positive.predictive.value","negative.predictive.value",
                                                 "positive.likehood.ratio","negative.likehood.ratio",
                                                 "percent.agreement","matthews.correlation.coefficient")

  return(list(resultado.1=resultado.1,resultado.2=resultado.2,resultado.3=resultado.3,indicadores=indicadores))
}
