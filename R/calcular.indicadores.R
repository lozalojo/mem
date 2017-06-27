#' Calculates specificity and sensitivity
#'
#' @keywords internal
#' @importFrom graphics rect
calcular.indicadores<-function(i.current,
                               i.umbral.pre,
                               i.umbral.pos=NA,
                               i.intensidades,
                               i.duracion.intensidad=10,
                               i.equal=F,
                               i.metodo.calculo="default",
                               i.semanas.por.encima=1,
                               i.valores.parametro.deteccion=seq(0.1,5.0,0.1),
                               i.output=".",
                               i.graph=F,
                               i.graph.name=""){

  #if (is.na(i.umbral.pre)) if (!is.na(i.umbral.pos)) i.umbral.pre<-i.umbral.pos else i.umbral.pre<-Inf
  if (is.na(i.umbral.pre)) i.umbral.pre<-Inf

  semanas<-dim(i.current)[1]
  nombre.semana<-rownames(i.current)
  numero.semana<-1:semanas

  # Preparacion de datos necesarios
  umbral.pre<-i.umbral.pre
  if (i.equal) umbral.pos<-i.umbral.pre else umbral.pos<-i.umbral.pos
  if (is.na(umbral.pos)) umbral.pos<-i.umbral.pre
  duracion.media<-i.duracion.intensidad
  punto.maximo<-max.fix.na(i.current)
  semana.punto.maximo<-min(numero.semana[i.current==punto.maximo],na.rm=T)
  # Semana inicio umbral
  if (any(i.current[,1]>umbral.pre,na.rm=T)){
    semana.inicio.umbral.1<-i.current[,1]>umbral.pre
    semana.inicio.umbral.2<-c(semana.inicio.umbral.1[2:semanas],NA)
    semana.inicio.umbral.3<-rbind(semana.inicio.umbral.1,semana.inicio.umbral.2)
    semana.inicio.umbral<-1+min((1:semanas)[apply(semana.inicio.umbral.3,2,sum,na.rm=T)==i.semanas.por.encima],na.rm=T)
  }else semana.inicio.umbral<-NA
  if (!is.na(semana.inicio.umbral)){
    resultado.2.1<-(1:semanas)[i.current[,1]<umbral.pos & semana.inicio.umbral<(1:semanas)]
    if (any(resultado.2.1,na.rm=T)) semana.fin.umbral<-min(resultado.2.1,na.rm=T) else semana.fin.umbral<-NA
  }else semana.fin.umbral<-NA

  # M?todo 0: Mediante optimo mem, datos definitivos
  curva.map<-calcular.map(as.vector(as.matrix(i.current)))
  resultado.1<-numeric()
  n.parametros<-length(i.valores.parametro.deteccion)
  optimos<-numeric()
  for (i in 1:n.parametros){
    optimo.map<-calcular.optimo(curva.map,2,i.valores.parametro.deteccion[i])
    semana.i.optimo<-optimo.map[4]
    if (optimo.map[5]<semanas) semana.f.optimo<-optimo.map[5]+1 else semana.f.optimo<-NA
    resultado.1.1<-rep(NA,semanas)
    if (!is.na(semana.i.optimo)){
      if (semana.i.optimo>1) resultado.1.1[1:(semana.i.optimo-1)]<-1
      if (!is.na(semana.f.optimo)){
        if (semana.i.optimo<semana.f.optimo) resultado.1.1[semana.i.optimo:(semana.f.optimo-1)]<-2
        resultado.1.1[semana.f.optimo:semanas]<-3
      }else resultado.1.1[semana.i.optimo:semanas]<-2
    }else resultado.1.1[1:semanas]<-1
    resultado.1.1[is.na(i.current[,1])]<-0
    resultado.1<-rbind(resultado.1,resultado.1.1)
    optimos<-rbind(optimos,c(semana.i.optimo,semana.f.optimo))
  }

  if (!(i.metodo.calculo=="alternative")){
    # M?todo 1: Tasas por encima y por debajo del umbral.
    resultado.2<-rep(NA,semanas)
    if (semana.punto.maximo<semanas){
      resultado.2.1<-i.current[(1:semana.punto.maximo),]>umbral.pre
      resultado.2[1:semana.punto.maximo][resultado.2.1]<-2
      resultado.2[1:semana.punto.maximo][!resultado.2.1]<-1
      resultado.2.2<-i.current[((semana.punto.maximo+1):semanas),]>=umbral.pos
      resultado.2[(semana.punto.maximo+1):semanas][resultado.2.2]<-2
      resultado.2[(semana.punto.maximo+1):semanas][!resultado.2.2]<-3
    }else{
      resultado.2.1<-i.current[(1:semana.punto.maximo),]>umbral.pre
      resultado.2[1:semana.punto.maximo][resultado.2.1]<-2
      resultado.2[1:semana.punto.maximo][!resultado.2.1]<-1
    }
    resultado.2[is.na(i.current[,1])]<-0
  }else{
    # M?todo 2: Hasta iniciar temporada (x tasas por encima), pre, hasta finalizar, epi, despues pos, independientemente de que las tasas est?n por encima o por debajo
    resultado.2<-rep(NA,semanas)
    if (!is.na(semana.inicio.umbral)){
      if (semana.inicio.umbral>1) resultado.2[1:(semana.inicio.umbral-1)]<-1
      if (!is.na(semana.fin.umbral)){
        if (semana.inicio.umbral<semana.fin.umbral) resultado.2[semana.inicio.umbral:(semana.fin.umbral-1)]<-2
        resultado.2[semana.fin.umbral:semanas]<-3
      }else resultado.2[semana.inicio.umbral:semanas]<-2
    }else resultado.2[1:semanas]<-1
    resultado.2[is.na(i.current[,1])]<-0
  }

  resultado.2<-matrix(rep(resultado.2,n.parametros),nrow=n.parametros,byrow=T)

  resultado.3<-numeric()
  for (i in 1:n.parametros){
    resultado.3.1<-apply(rbind(resultado.1[i,],resultado.2[i,]),2,comparar.metodos)
    resultado.3<-rbind(resultado.3,resultado.3.1)
  }

  true.pos.t<-sum(resultado.3=="TP",na.rm=T)
  true.pos<-apply(resultado.3=="TP",1,sum,na.rm=T)
  false.neg.t<-sum(resultado.3=="FN",na.rm=T)
  false.neg<-apply(resultado.3=="FN",1,sum,na.rm=T)
  false.pos.t<-sum(resultado.3=="FP",na.rm=T)
  false.pos<-apply(resultado.3=="FP",1,sum,na.rm=T)
  true.neg.t<-sum(resultado.3=="TN",na.rm=T)
  true.neg<-apply(resultado.3=="TN",1,sum,na.rm=T)

  sensibilidad<-true.pos/(true.pos+false.neg)
  sensibilidad[is.nan(sensibilidad)]<-NA
  especificidad<-true.neg/(true.neg+false.pos)
  especificidad[is.nan(especificidad)]<-NA
  ppv<-true.pos/(true.pos+false.pos)
  ppv[is.nan(ppv)]<-NA
  npv<-true.neg/(true.neg+false.neg)
  npv[is.nan(npv)]<-NA
  pos.likehood.ratio<-sensibilidad/(1-especificidad)
  pos.likehood.ratio[is.nan(pos.likehood.ratio)]<-NA
  neg.likehood.ratio<-(1-sensibilidad)/especificidad
  neg.likehood.ratio[is.nan(neg.likehood.ratio)]<-NA

  percent.agreement<-(true.pos+true.neg)/(true.pos+true.neg+false.pos+false.neg)
  percent.agreement[is.nan(percent.agreement)]<-NA

  #Matthews correlation coefficient
  mcc<-(true.pos*true.neg-false.pos*false.neg)/sqrt((true.pos+false.pos)*(true.pos+false.neg)*(true.neg+false.pos)*(true.neg+false.neg))
  mcc[is.nan(mcc)]<-NA

  if (true.pos.t+false.neg.t>0) sensibilidad.t<-true.pos.t/(true.pos.t+false.neg.t) else sensibilidad.t<-NA
  if (true.neg.t+false.pos.t>0) especificidad.t<-true.neg.t/(true.neg.t+false.pos.t) else especificidad.t<-NA
  if (true.pos.t+false.pos.t>0) ppv.t<-true.pos.t/(true.pos.t+false.pos.t) else ppv.t<-NA
  if (true.neg.t+false.neg.t>0) npv.t<-true.neg.t/(true.neg.t+false.neg.t) else npv.t<-NA
  pos.likehood.ratio.t<-NA
  if (!is.na(especificidad.t)) if (1-especificidad.t>0) pos.likehood.ratio.t<-sensibilidad.t/(1-especificidad.t) else pos.likehood.ratio.t<-NA
  neg.likehood.ratio.t<-NA
  if (!is.na(especificidad.t)) if (especificidad.t>0) neg.likehood.ratio.t<-(1-sensibilidad.t)/especificidad.t else neg.likehood.ratio.t<-NA
  if (true.pos.t+true.neg.t+false.pos.t+false.neg.t>0) percent.agreement.t<-(true.pos.t+true.neg.t)/(true.pos.t+true.neg.t+false.pos.t+false.neg.t) else percent.agreement.t<-NA

  if ((true.pos.t+false.pos.t)>0 & (true.pos.t+false.neg.t)>0 & (true.neg.t+false.pos.t)>0 & (true.neg.t+false.neg.t)>0) mcc.t<-(true.pos.t*true.neg.t-false.pos.t*false.neg.t)/(sqrt(true.pos.t+false.pos.t)*sqrt(true.pos.t+false.neg.t)*sqrt(true.neg.t+false.pos.t)*sqrt(true.neg.t+false.neg.t)) else mcc.t<-NA

  semanas.not.na<-sum(!is.na(i.current))

  indicadores.t<-as.matrix(c(semanas,semanas.not.na,true.pos.t,false.pos.t,true.neg.t,
                             false.neg.t,sensibilidad.t,especificidad.t,ppv.t,npv.t,
                             pos.likehood.ratio.t,neg.likehood.ratio.t,percent.agreement.t,
                             mcc.t))
  rownames(indicadores.t)<-c("Weeks","Non-missing weeks","True positives","False positives",
                             "True negatives","False negatives","Sensitivity","Specificity",
                             "Positive predictive value","Negative predictive value",
                             "Positive likehood ratio","Negative likehood ratio",
                             "Percent agreement","Matthews correlation coefficient")
  colnames(indicadores.t)<-"values"

  indicadores<-data.frame(parametro=i.valores.parametro.deteccion,semanas=semanas,semanas.not.na=semanas.not.na,
                          true.pos=true.pos,false.pos=false.pos,true.neg=true.neg,false.neg=false.neg,
                          sensibilidad=sensibilidad,especificidad=especificidad,ppv=ppv,npv=npv,
                          pos.likehood.ratio=pos.likehood.ratio,neg.likehood.ratio=neg.likehood.ratio,
                          percent.agreement=percent.agreement,mcc=mcc)

  if (i.graph){

    ######## Gr?fico

    limites.niveles<-as.vector(i.intensidades)
    #nombres.niveles<-as.character(i.flu$epi.intervalos[,1])
    limites.niveles[limites.niveles<0]<-0

    # Datos para el grafico
    if (is.na(semana.inicio.umbral)){
      # No iniciada
      umbrales.1<-rep(umbral.pre,semanas)
      umbrales.2<-rep(NA,semanas)
      intensidades.1<-array(dim=c(semanas,3))
      intensidades.2<-array(dim=c(semanas,3))
    }else{
      if (is.na(semana.fin.umbral)){
        # Iniciada y no finalizada
        umbrales.1<-rep(umbral.pre,semana.inicio.umbral-1)
        umbrales.2<-rep(NA,max(duracion.media,semanas-semana.inicio.umbral+2))
        intensidades.1<-array(dim=c(max(semana.inicio.umbral-2,0),3))
        intensidades.2<-matrix(rep(limites.niveles,max(duracion.media,semanas-semana.inicio.umbral+2)),ncol=3,byrow=T)
      }else{
        # Iniciada y finalizada
        umbrales.1<-rep(umbral.pre,semana.inicio.umbral-1)
        umbrales.2<-rep(NA,semana.fin.umbral-semana.inicio.umbral)
        intensidades.1<-array(dim=c(max(semana.inicio.umbral-2,0),3))
        intensidades.2<-matrix(rep(limites.niveles,semana.fin.umbral-semana.inicio.umbral+2),ncol=3,byrow=T)
      }
    }
    umbrales.3<-rep(umbral.pos,semanas)

    umbrales<-c(umbrales.1,umbrales.2,umbrales.3)[1:semanas]
    intensidades.3<-array(dim=c(semanas,3))
    intensidades<-rbind(intensidades.1,intensidades.2,intensidades.3)[1:semanas,]
    dgraf<-as.data.frame(cbind(i.current,umbrales,intensidades))
    names(dgraf)<-c("Rate","Epidemic threshold",paste("Intensidad",1:3))

    etiquetas<-c("Weekly rates","Epidemic thr","Medium thr","High thr","Very high thr")
    tipos<-c(1,2,2,2,2)
    anchos<-c(3,2,2,2,2)
    colores<-c("#808080","#8c6bb1","#88419d","#810f7c","#4d004b")
    colores.epi<-c("#00C000","#980043","#FFB401")

    range.x<-1:semanas

    # calculo el rango y para que tenga 10 marcas o este cerca

    maximo.y<-max.fix.na(dgraf)
    posicion.ticks<-optimal.tickmarks(0,maximo.y,10)$by
    range.y<-c(-1.5*posicion.ticks,ceiling(maximo.y/posicion.ticks)*posicion.ticks)
    range.y.seq<-seq(0,ceiling(maximo.y/posicion.ticks)*posicion.ticks,posicion.ticks)

    for (i in 1:n.parametros){
      if (i.graph.name=="") graph.name<-paste("surveillance graph (",format(round(i.valores.parametro.deteccion[i],1),digits=3,nsmall=1),")",sep="") else graph.name<-paste(i.graph.name," (",format(round(i.valores.parametro.deteccion[i],1),digits=3,nsmall=1),")",sep="")

      tiff(filename=paste(i.output,"/",graph.name,".tiff",sep=""),width=8,height=6,units="in",pointsize="12",
           compression="lzw",bg="white",res=300,antialias="none")
      opar<-par(mar=c(4,4,1,8)+0.1,xpd=TRUE)
      #,mgp=c(3,0.5,0),xpd=T)
      # Grafico principal
      matplot(range.x,
              dgraf,
              type="l",
              lty=tipos,lwd=anchos,col=colores,
              xlab="",ylab="",axes=F,
              ylim=range.y)
      # Puntos de la serie de tasas
      #points(1:semanas,dgraf[,1],pch=19,type="p",col="#000000",cex=1)
      # pre
      puntos.1<-i.current[,1]
      puntos.1[!(resultado.1[i,]==1)]<-NA
      # if (!is.na(optimos[i,1])) puntos.1[optimos[i,1]:semanas]<-NA
      points(puntos.1,pch=19,type="p",col=colores.epi[1],cex=1.5)
      # epi
      puntos.2<-i.current[,1]
      puntos.2[!(resultado.1[i,]==2)]<-NA
      # if (is.na(optimos[i,1])){
      #   puntos.2[1:semanas]<-NA
      # }else{
      #   if (optimos[i,1]>1) puntos.2[1:(optimos[i,1]-1)]<-NA
      #   if (!is.na(optimos[i,2])) puntos.2[optimos[i,2]:semanas]<-NA
      # }
      points(puntos.2,pch=19,type="p",col=colores.epi[2],cex=1.5)
      # post
      puntos.3<-i.current[,1]
      puntos.3[!(resultado.1[i,]==3)]<-NA
      # if (is.na(optimos[i,1])){
      #   puntos.3[1:semanas]<-NA
      # }else{
      #   if (is.na(optimos[i,2])) puntos.3[1:semanas]<-NA else puntos.3[1:(optimos[i,2]-1)]<-NA
      # }
      points(puntos.3,pch=19,type="p",col=colores.epi[3],cex=1.5)
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
      # Etiquetas de los 4 umbrales

      if (is.na(semana.inicio.umbral)){
        # No iniciada
        text.x<-semanas-semanas/25
        text.y<-umbral.pre
        text.l<-round(umbral.pre,2)
        text.p<-3
        text.s<-1
        text.c<-colores[2]
      }else{
        if (is.na(semana.fin.umbral)){
          # Iniciada y no finalizada
          lugar.intensidad<-min(semanas,semana.inicio.umbral+max(duracion.media,semanas-semana.inicio.umbral+1))
        }else{
          # Iniciada y finalizada
          lugar.intensidad<-semana.fin.umbral
        }
        text.x<-c(semana.inicio.umbral,rep(lugar.intensidad,3),semanas)-semanas/25
        text.y<-c(umbral.pre,limites.niveles,umbral.pos)
        text.l<-round(c(umbral.pre,limites.niveles,umbral.pos),2)
        text.p<-rep(3,5)
        text.s<-rep(1,5)
        text.c<-colores[c(2:5,2)]
      }
      text(text.x,text.y,text.l,pos=text.p,col=text.c,cex=text.s)

      # Etiquetas de la leyenda
      etiquetas.leyenda<-c("Pre","Epidemic","Post",etiquetas)
      tipos.leyenda<-c(1,1,1,tipos)
      anchos.leyenda<-c(1,1,1,anchos)
      colores.leyenda<-c("#C0C0C0","#C0C0C0","#C0C0C0",colores)
      puntos.leyenda<-c(21,21,21,rep(NA,5))
      bg.leyenda<-c("#FFFFFF","#FFFFFF","#FFFFFF",rep(NA,5))
      pt.bg.leyenda<-c("#00C000","#980043","#FFB401",rep(NA,5))

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
           col=colores.cuadros[1+resultado.2[i,]],
           border="white")
      rect(seq(0.5,semanas-0.5,1),
           -2*posicion.ticks/2,
           seq(1.5,(semanas+0.5),1),
           -1*posicion.ticks/2,
           density=NULL,
           col=colores.cuadros[1+resultado.1[i,]],
           border="white")

      text.xy<-expand.grid(x=seq(1,semanas,1),y=seq(-2.5,-1.5,1)*posicion.ticks/2)
      text.l<-c(resultado.3,rep(NA,semanas))
      text.p<-rep(1,semanas*2)
      text.s<-rep(0.25,semanas*2)
      text.c<-rep("#FFFFFF",semanas*2)
      text(text.xy$x,text.xy$y,text.l,col=text.c,cex=text.s)

      text(semanas,-3.5*posicion.ticks/2,pos=2,label=paste("Sensitivity: ",format(round(sensibilidad[i],2),nsmall=2,align="right"),", Specificity: ",format(round(especificidad[i],2),nsmall=2,align="right"),sep=""),cex=0.5)

      if (!(i.metodo.calculo=="alternative")){
        text.method<-paste(i.metodo.calculo,sep="")
      }else{
        text.method<-paste(i.metodo.calculo," (",i.semanas.por.encima,"w)",sep="")
      }

      axis(2,at=seq(-2.5,-1.5,1)*posicion.ticks/2,
           labels=c(text.method,paste("real (",format(round(i.valores.parametro.deteccion[i],1),digits=3,nsmall=1),")",sep="")),
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
      dev.off()

    }
  }

  return(list(resultado.1=resultado.1,resultado.2=resultado.2,resultado.3=resultado.3,indicadores=indicadores,indicadores.t=indicadores.t))
}
