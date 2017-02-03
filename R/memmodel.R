#' Methods for influenza modelization
#'
#' Function \code{memmodel} is used to calculate the threshold for influenza epidemic using historical
#' records (surveillance rates).\cr
#' The method to calculate the threshold is described in the Moving Epidemics Method (MEM) used to
#' monitor influenza activity in a weekly surveillance system.
#'
#' Input data is a data frame containing rates that represent historical influenza surveillance
#' data. It can start and end at any given week (tipically at week 40th), and rates can be
#' expressed as per 100,000 inhabitants (or per consultations, if population is not
#' available) or any other scale.\cr
#' Parameters \code{i.type}, \code{i.type.threshold} and \code{i.type.curve} defines how to
#' calculate confidence intervals along the process.\cr
#' \code{i.type.curve} is used for calculating the typical influenza curve,
#' \code{i.type.threshold} is used to calculate the pre and post epidemic threshold and
#' \code{i.type} is used for any other confidende interval used in the method.\cr
#' All three parameters must be a number between \code{1} and \code{6}:\cr
#' \tabular{rlll}{
#' \tab \code{1} \tab Arithmetic mean and mean confidence interval.\cr
#' \tab \code{2} \tab Geometric mean and mean confidence interval.\cr
#' \tab \code{3} \tab Median and the KC Method to calculate its confidence interval.\cr
#' \tab \code{4} \tab Median and bootstrap confidence interval.\cr
#' \tab \code{5} \tab Arithmetic mean and point confidence interval (standard deviations).\cr
#' \tab \code{6} \tab Geometric mean and point confidence interval (standard deviations).\cr
#' }
#' Option \code{4} uses two more parameters: \code{i.type.boot} indicates which bootstrap
#' method to use. The values are the same of those of the \code{\link{boot.ci}} function.
#' Parameter \code{i.iter.boot} indicates the number of bootstrap samples to use. See
#' \code{\link{boot}} for more information about this topic.\cr
#' Parameters \code{i.level}, \code{i.level.threshold} and \code{i.level.curve} indicates,
#' respectively, the level of the confidence intervals described above.\cr
#' The \code{i.n.max} parameter indicates how many pre epidemic values to use to calculate
#' the threshold. A value of -1 indicates the program to use an appropiate number of points
#' depending on the number of seasons provided as input. \code{i.tails} tells the program
#' to use {1} or {2} tailed confidence intervals when calculating the threshold (1 is
#' recommended).\cr
#' Parameters \code{i.method} and \code{i.param} indicates how to find the optimal timing
#' of the epidemics. See \code{\link{memtiming}} for details on the values this parameters
#' can have.
#'
#' @name memmodel
#'
#' @aliases summary.flu,plot.flu,print.flu
#'
#' @param i.data Data frame of input data.
#' @param i.seasons Maximum number of seasons to use.
#' @param i.type.threshold Type of confidence interval to calculate the threshold.
#' @param i.level.threshold Level of confidence interval to calculate the threshold.
#' @param i.tails.threshold Tails for the confidence interval to calculate the threshold.
#' @param i.type.intensity Type of confidence interval to calculate the intensity thresholds.
#' @param i.level.intensity Levels of confidence interval to calculate the intensity thresholds.
#' @param i.tails.intensity Tails for the confidence interval to calculate the threshold.
#' @param i.type.curve Type of confidence interval to calculate the modelled curve.
#' @param i.level.curve Level of confidence interval to calculate the modelled curve.
#' @param i.type.other Type of confidence interval to calculate length, start and percentages.
#' @param i.level.other Level of confidence interval to calculate length, start and percentages.
#' @param i.method Method to calculate the optimal timing of the epidemic.
#' @param i.param Parameter to calculate the optimal timing of the epidemic.
#' @param i.n.max Number of pre-epidemic values used to calculate the threshold.
#' @param i.type.boot Type of bootstrap technique.
#' @param i.iter.boot Number of bootstrap iterations.
#'
#' @return
#' \code{memmodel} returns an object of class \code{mem}.
#' An object of class \code{mem} is a list containing at least the following components:
#'   \item{i.data }{input data}
#'   \item{pre.post.intervals }{Pre/post confidence intervals (Threhold is the upper limit
#'   of the confidence interval).}
#'   \item{ci.length }{Mean epidemic length confidence interval.}
#'   \item{ci.percent }{Mean covered percentage confidence interval.}
#'   \item{mean.length }{Mean length.}
#'   \item{moving.epidemics }{Moving epidemic rates.}
#'   \item{mean.start }{Mean epidemic start.}
#'   \item{epi.intervals }{Epidemic levels of intensity.}
#'   \item{typ.curve }{Typical epidemic curve.}
#'   \item{n.max }{Effective number of pre epidemic values.}
#'
#' @examples
#' # Castilla y Leon Influenza Rates data
#' data(flucyl)
#' # Finds the timing of the first season: 2001/2002
#' epi<-memmodel(flucyl)
#' print(epi)
#' summary(epi)
#' plot(epi)
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
#' @importFrom stats loess qnorm quantile runif var
#' @importFrom grDevices colorRampPalette colorRamp dev.off rgb tiff
#' @importFrom RColorBrewer brewer.pal
#' @importFrom graphics abline axis legend matplot mtext par points text lines
memmodel<-function(i.data,
                    i.seasons=10,
                    i.type.threshold=5,
                    i.level.threshold=0.95,
                    i.tails.threshold=1,
                    i.type.intensity=6,
                    i.level.intensity=c(0.40,0.90,0.975),
                    i.tails.intensity=1,
                    i.type.curve=2,
                    i.level.curve=0.95,
                    i.type.other=2,
                    i.level.other=0.95,
                    i.method=2,
                    i.param=2.8,
                    i.n.max=-1,
                    i.type.boot="norm",
                    i.iter.boot=10000){

  if (is.null(dim(i.data))) stop('Incorrect number of dimensions, input must be a data.frame.') else if (!(ncol(i.data)>1)) stop('Incorrect number of dimensions, at least two seasons of data required.')

  datos<-i.data[apply(i.data,2,function(x) sum(x,na.rm=T)>0)]

  if (is.matrix(datos)) datos<-as.data.frame(datos)

  if (!is.na(i.seasons)) if (i.seasons>0) datos<-datos[(max((dim(datos)[2])-i.seasons+1,1)):(dim(datos)[2])]

  datos<-as.data.frame(apply(datos,2,fill.missing))

  semanas<-dim(datos)[1]
  anios<-dim(datos)[2]

  # Calcular el optimo

  if (i.n.max==-1){
    n.max<-max(1,round(30/anios,0))
    #if (anios>=10) n.max=3 else n.max=5
  }else if (i.n.max==0){
    n.max=semanas
  }else{
    n.max=i.n.max
  }

  optimo<-apply(datos,2,memtiming,i.n.values=n.max,i.method=i.method,i.param=i.param)

  datos.duracion.real<-extraer.datos.optimo.map(optimo)

  # por defecto la aritmetica en ambos
  ic.duracion<-iconfianza(as.numeric(datos.duracion.real[1,]),i.level.other,i.type.other,T,i.type.boot,i.iter.boot,2)
  ic.duracion<-rbind(ic.duracion,c(floor(ic.duracion[1]),round(ic.duracion[2]),ceiling(ic.duracion[3])))
  ic.porcentaje<-iconfianza(as.numeric(datos.duracion.real[2,]),i.level.other,i.type.other,T,i.type.boot,i.iter.boot,2)

  duracion.media<-ic.duracion[2,2]

  ########################################################################################
  # Calcula todos los parametros asociados a la temporada gripal en base a la estimacion #
  # del numero de semanas de duracion total.                                             #
  ########################################################################################

  # Datos de la gripe en la temporada REAL y en la temporada MEDIA, esta ultima sirve para unir las temporadas.

  semana.inicio<-as.integer(rownames(datos)[1])
  gripe<-array(dim=c(7,anios,2))

  datos.duracion.media<-extraer.datos.curva.map(optimo,duracion.media)

  for (j in 1:anios){
    ## Semana en la q comienza la temporada.real
    gripe[1,j,1]<-datos.duracion.real[4,j]
    ## Semana en la q acaba
    gripe[2,j,1]<-datos.duracion.real[5,j]
    ## numero de casos en la temporada.real de gripe
    gripe[3,j,1]<-datos.duracion.real[3,j]
    ## Numero de casos totales de la temporada global
    gripe[4,j,1]<-sum(datos[,j],na.rm=TRUE)
    ## Porcentaje cubierto de casos de la temporada gripal
    gripe[5,j,1]<-datos.duracion.real[2,j]
    ## Casos Acumulados justo antes de comenzar la temporada gripal
    if (gripe[1,j,1]>1) gripe[6,j,1]<-sum(datos[1:(gripe[1,j,1]-1),j],na.rm=TRUE) else gripe[6,j,1]<-0
    ## Casos Despues de la temporada gripal
    if (gripe[2,j,1]<semanas) gripe[7,j,1]<-sum(datos[(gripe[2,j,1]+1):semanas,j],na.rm=TRUE) else gripe[7,j,1]<-0

    ## Semana en la q comienza la temporada.media
    gripe[1,j,2]<-datos.duracion.media[4,j]
    ## Semana en la q acaba
    gripe[2,j,2]<-datos.duracion.media[5,j]
    ## numero de casos en la temporada.media de gripe
    gripe[3,j,2]<-datos.duracion.media[3,j]
    ## Numero de casos totales de la temporada global
    gripe[4,j,2]<-sum(datos[,j],na.rm=TRUE)
    ## Porcentaje cubierto de casos de la temporada gripal
    gripe[5,j,2]<-datos.duracion.media[2,j]
    ## Casos Acumulados justo antes de comenzar la temporada gripal
    if (gripe[1,j,2]>1) gripe[6,j,2]<-sum(datos[1:(gripe[1,j,2]-1),j],na.rm=TRUE) else gripe[6,j,2]<-0
    ## Casos Despues de la temporada gripal
    if (gripe[2,j,2]<semanas) gripe[7,j,2]<-sum(datos[(gripe[2,j,2]+1):semanas,j],na.rm=TRUE) else gripe[7,j,2]<-0
  }

  # Indice de estado temporal. Si es un 1 me dice que estamos en epoca pretemporada,
  # si pone un 2 indica que estamos en temporada gripal. Si pone un 3 indica que
  # estamos en posttemporada.

  indices.temporada<-array(dim=c(semanas,anios,2))

  for (j in 1:anios){
    indices.temporada[-((datos.duracion.real[4,j]):semanas),j,1]<-1
    indices.temporada[(datos.duracion.real[4,j]):(datos.duracion.real[5,j]),j,1]<-2
    indices.temporada[-(1:(datos.duracion.real[5,j])),j,1]<-3
    indices.temporada[-((datos.duracion.media[4,j]):semanas),j,2]<-1
    indices.temporada[(datos.duracion.media[4,j]):(datos.duracion.media[5,j]),j,2]<-2
    indices.temporada[-(1:(datos.duracion.media[5,j])),j,2]<-3
  }

  ## Estimacion del numero de semana en el  comienza la gripe

  ic.inicio<-iconfianza(as.numeric(datos.duracion.real[4,]),i.level.other,i.type.other,T,i.type.boot,i.iter.boot,2)
  ic.inicio<-rbind(ic.inicio,c(floor(ic.inicio[1]),round(ic.inicio[2]),ceiling(ic.inicio[3])))
  ic.inicio<-rbind(ic.inicio,c(semana.absoluta(ic.inicio[2,1],semana.inicio),semana.absoluta(ic.inicio[2,2],semana.inicio),semana.absoluta(ic.inicio[2,3],semana.inicio)))
  ic.inicio<-ic.inicio[c(2,3,1),]
  inicio.medio<-ic.inicio[1,2]

  # Grafico del esquema de las temporadas gripales para su union

  longitud.esquema<-semanas+max.fix.na(gripe[1,,2])-min.fix.na(gripe[1,,2])
  inicio.epidemia.esquema<-max.fix.na(gripe[1,,2])
  fin.epidemia.esquema<-max.fix.na(gripe[2,,2])

  esquema.temporadas<-array(dim=c(longitud.esquema,anios+2,3))

  for (j in 1:anios){
    for (i in 1:semanas){
      diferencia<-inicio.epidemia.esquema-gripe[1,j,2]
      esquema.temporadas[i+diferencia,j,1]<-i
      esquema.temporadas[i+diferencia,j,2]<-semana.absoluta(i,semana.inicio)
      esquema.temporadas[i+diferencia,j,3]<-datos[i,j]
    }
  }

  diferencia<-inicio.epidemia.esquema-inicio.medio
  for (i in 1:semanas){
    if ((i+diferencia)>=1 & (i+diferencia)<=longitud.esquema){
      esquema.temporadas[i+diferencia,anios+1,1]<-i
      esquema.temporadas[i+diferencia,anios+1,2]<-semana.absoluta(i,semana.inicio)
    }
  }

  esquema.temporadas[,anios+2,c(1,2)]<-3
  esquema.temporadas[1:(inicio.epidemia.esquema-1),anios+2,1]<-1
  esquema.temporadas[(inicio.epidemia.esquema):(inicio.epidemia.esquema+duracion.media-1),anios+2,1]<-2
  esquema.temporadas[1:(inicio.epidemia.esquema-1),anios+2,2]<-1
  esquema.temporadas[(inicio.epidemia.esquema):(inicio.epidemia.esquema+duracion.media-1),anios+2,2]<-2

  ## Temporadas moviles

  temporadas.moviles<-esquema.temporadas[,c(1:anios),3]

  ## Limites de la temporada

  limites.temporada<-c(inicio.medio,inicio.medio+duracion.media-1)
  limites.temporada<-rbind(limites.temporada,c(semana.absoluta(inicio.medio,semana.inicio),semana.absoluta(inicio.medio+duracion.media-1,semana.inicio)))

  # Limites relativos

  limites.esquema<-c(inicio.epidemia.esquema,inicio.epidemia.esquema+duracion.media-1)

  ## En ttotal reordenamos la gripe para q todas las temporadas comiencen la semana "estinicio" (es decir, quitamos informacion
  ## por delante y por detras de esquema.temporada[,,3] para que queden 35 semanas (las que hay).

  temporadas.moviles.recortada<-array(dim=c(semanas,anios))

  ## Temporada gripal comienza "estinicio" y termina en "estinicio+temporada-1"

  diferencia<-inicio.medio-limites.esquema[1]

  for (j in 1:anios){
    for (i in 1:semanas){
      if ((i-diferencia)>=1 & (i-diferencia)<=longitud.esquema){
        temporadas.moviles.recortada[i,j]<-temporadas.moviles[i-diferencia,j]
      }else{
        temporadas.moviles.recortada[i,j]<-NA
      }
    }
  }

  ## Dibujamos la curva epidemica tipo

  # Importante: ?Quitamos los ceros?
  # iy<-(is.na(temporadas.moviles.recortada) | temporadas.moviles.recortada==0)
  iy<-is.na(temporadas.moviles.recortada)
  temporadas.moviles.recortada.no.ceros<-temporadas.moviles.recortada
  temporadas.moviles.recortada.no.ceros[iy]<-NA
  curva.tipo<-t(apply(temporadas.moviles.recortada.no.ceros,1,iconfianza,nivel=i.level.curve,tipo=i.type.curve,ic=T,tipo.boot=i.type.boot,iteraciones.boot=i.iter.boot,colas=2))

  ## Seleccionamos los periodos pre, epidemia, y post

  ## PRE y POST-TEMPORADA GRIPAL

  ## Como no se registra mas q de la semana 40 a la 20, tenemos q muchas de las semanas tienen tasa 0, eliminamos esas semanas,
  ## ya q podrian llevar a una infraestimacion de la tasa base fuera de temporada

  pre.datos<-as.vector(as.matrix(extraer.datos.pre.epi(optimo)))
  post.datos<-as.vector(as.matrix(extraer.datos.post.epi(optimo)))
  epi.datos<-as.vector(as.matrix(extraer.datos.epi(optimo)))
  epi.datos.2<-as.vector(as.matrix(apply(datos,2,max.n.valores,n.max=n.max)))

  pre.post.datos<-rbind(pre.datos,post.datos)

  # IC de la linea basica de pre y post temporada

  # por defecto estaba la geometrica
  # pre.d<-pre.datos[!(is.na(pre.datos) | pre.datos==0)]
  # post.d<-post.datos[!(is.na(post.datos) | post.datos==0)]
  # epi.d<-epi.datos[!(is.na(epi.datos) | epi.datos==0)]
  # epi.d.2<-epi.datos.2[!(is.na(epi.datos.2) | epi.datos.2==0)]
  pre.d<-pre.datos[!is.na(pre.datos)]
  post.d<-post.datos[!is.na(post.datos)]
  epi.d<-epi.datos[!is.na(epi.datos)]
  epi.d.2<-epi.datos.2[!is.na(epi.datos.2)]

  pre.i<-iconfianza(pre.d,nivel=i.level.threshold,tipo=i.type.threshold,ic=T,tipo.boot=i.type.boot,iteraciones.boot=i.iter.boot,colas=i.tails.threshold)
  post.i<-iconfianza(post.d,nivel=i.level.threshold,tipo=i.type.threshold,ic=T,tipo.boot=i.type.boot,iteraciones.boot=i.iter.boot,colas=i.tails.threshold)
  epi.intervalos<-numeric()
  for (niv in i.level.intensity) epi.intervalos<-rbind(epi.intervalos,c(niv,iconfianza(epi.d,nivel=niv,tipo=i.type.intensity,ic=T,colas=i.tails.intensity)))
  epi.intervalos.2<-numeric()
  for (niv in i.level.intensity) epi.intervalos.2<-rbind(epi.intervalos.2,c(niv,iconfianza(epi.d.2,nivel=niv,tipo=i.type.intensity,ic=T,colas=i.tails.intensity)))

  pre.post.intervalos<-rbind(pre.i,post.i)

  ## Ademas, añadimos las estimaciones de las lineas basicas antes y despues

  #lineas.basicas<-array(dim=c(semanas,3))
  #
  #for (i in 1:(inicio.medio-1)){
  #	lineas.basicas[i,]<-pre.post.intervalos[1,1:3]
  #}
  #
  #for (i in ((inicio.medio+duracion.media):semanas)){
  #	lineas.basicas[i,]<-pre.post.intervalos[2,1:3]
  #}

  memmodel.output<-list(pre.post.intervals=pre.post.intervalos,
    ci.length=ic.duracion,
    ci.percent=ic.porcentaje,
    ci.start=ic.inicio,
    mean.length=duracion.media,
    mean.start=inicio.medio,
    moving.epidemics=temporadas.moviles.recortada,
    epi.intervals=epi.intervalos,
    epi.intervals.full=epi.intervalos.2,
    typ.curve=curva.tipo,
    n.max=n.max,
    n.seasons=anios,
    n.weeks=semanas,
    data=datos,
    start.week=semana.inicio,
    epi.data=epi.datos,
    epi.data.nona=epi.d,
    epi.data.full=epi.datos.2,
    epi.data.nona.full=epi.d.2,
    optimum=optimo,
    real.length.data=datos.duracion.real,
    seasons.data=gripe,
    season.indexes=indices.temporada,
    season.scheme=esquema.temporadas,
    seasons.moving=temporadas.moviles,
    pre.post.data=pre.post.datos,
    pre.data.nona=pre.d,
    post.data.nona=post.d,
    epidemic.thresholds=as.numeric(pre.post.intervalos[,3]),
    intensity.thresholds=as.numeric(epi.intervalos[,4]),
    param.data=i.data,
    param.seasons=i.seasons,
    param.type.threshold=i.type.threshold,
    param.level.threshold=i.level.threshold,
    param.tails.threshold=i.tails.threshold,
    param.type.intensity=i.type.intensity,
    param.level.intensity=i.level.intensity,
    param.tails.intensity=i.tails.intensity,
    param.type.curve=i.type.curve,
    param.level.curve=i.level.curve,
    param.type.other=i.type.other,
    param.level.other=i.level.other,
    param.method=i.method,
    param.param=i.param,
    param.n.max=i.n.max,
    param.type.boot=i.type.boot,
    param.iter.boot=i.iter.boot)

  memmodel.output$call<-match.call()
  class(memmodel.output)<-"mem"
  return(memmodel.output)
}

#' @export
print.mem<- function(x, ...){
  threshold<-as.data.frame(round(t(x$pre.post.intervals[1:2,3]),2))
  colnames(threshold)<-c("Pre","Post")
  rownames(threshold)<-"Threshold"
  values<-as.data.frame(round(x$epi.intervals[,4],2))
  colnames(values)<-"Threshold"
  rownames(values)<-paste(c("Medium","High","Very high")," (",round(x$epi.intervals[,1]*100,1),"%)",sep="")
  cat("Call:\n")
  print(x$call)
  cat("\nEpidemic threshold:\n")
  print(threshold)
  cat("\nIntensity thresholds:\n")
  print(values)
}

#' @export
summary.mem<-function(object, ...){
  threshold<-as.data.frame(round(t(object$pre.post.intervals[1:2,3]),2))
  colnames(threshold)<-c("Pre","Post")
  rownames(threshold)<-"Threshold"
  values<-as.data.frame(round(object$epi.intervals[,4],2))
  colnames(values)<-"Threshold"
  rownames(values)<-paste(c("Medium","High","Very high")," (",round(object$epi.intervals[,1]*100,1),"%)",sep="")
  cat("Call:\n")
  print(object$call)
  cat("\nParameters:\n")
  cat("\t- General:\n")
  cat("\t\t+ Number of seasons restriction: ", if (object$param.seasons==-1) "Unrestricted (maximum)" else paste("Restricted to ",object$param.seasons,sep=""),"\n")
  cat("\t\t+ Number of seasons used: ", object$n.seasons,"\n")
  cat("\t\t+ Number of weeks: ", object$n.weeks,"\n")
  cat("\t- Confidence intervals:\n")
  cat("\t\t+ Epidemic threshold: ", output.ci(object$param.type.threshold,object$param.level.threshold,object$param.tails.threshold),"\n")
  cat("\t\t+ Intensity: ", output.ci(object$param.type.intensity,object$param.level.intensity,object$param.tails.intensity),"\n")
  cat("\t\t+ Curve: ", output.ci(object$param.type.curve,object$param.level.curve,2),"\n")
  cat("\t\t+ Others: ", output.ci(object$param.type.other, object$param.level.other,2),"\n")
  cat("\t- Epidemic timing calculation:\n")
  cat("\t\t+ Method: ", object$param.method,"\n")
  cat("\t\t+ Parameter: ", object$param.param,"\n")
  cat("\t- Epidemic threshold calculation:\n")
  cat("\t\t+ Pre-epidemic values: ", if (object$param.n.max==-1) paste("Optimized: ",object$n.max,sep="") else object$n.max,"\n")
  cat("\t\t+ Tails of CI: ", object$param.tails.threshold,"\n")
  cat("\t- Intensity thresholds calculation:\n")
  cat("\t\t+ Number of values: ", if (object$param.n.max==-1) paste("Optimized: ",object$n.max,sep="") else object$n.max,"\n")
  cat("\t\t+ Tails of CI: ", object$param.tails.intensity,"\n")
  cat("\t\t+ Levels of CI: ", paste(c("Medium","High","Very high"),": ",round(object$epi.intervals[,1]*100,1),"%",sep=""),"\n")
  cat("\t- Bootstrap (if used):\n")
  cat("\t\t+ Technique: ", if (is.na(object$param.type.boot)) "-" else object$param.type.boot,"\n")
  cat("\t\t+ Bootstrap samples: ", if (is.na(object$param.iter.boot)) "-" else object$param.iter.boot,"\n")
  cat("\nEpidemic description:\n")
  cat("\t- Typical influenza season lasts ",round(object$ci.length[1,2],2)," weeks. CL ",100*object$param.level,"% of\t[",round(object$ci.length[1,1],2),",",round(object$ci.length[1,3],2),"]\n")
  cat("\t- This optimal ",object$mean.length," weeks influenza season includes the",round(object$ci.percent[2],2),"% of the total sum of rates\n\n")
  cat("\nEpidemic threshold:\n")
  print(threshold)
  cat("\n\nIntensity thresholds:\n")
  print(values)
}

#' @export
plot.mem<-function(x,...){
  #opar<-par(mfrow=c(1,2))
  opar<-par(mfrow=c(1,2),mar=c(4,3,1,2)+0.1,mgp=c(3,0.5,0),xpd=T)

  # Graph 1
  semanas<-dim(x$data)[1]
  anios<-dim(x$data)[2]
  names.seasons<-sub("^.*\\(([^\\)]*)\\)$","\\1",names(x$data),perl=T)
  datos.graf<-x$moving.epidemics
  colnames(datos.graf)<-names(x$data)
  #lab.graf<-(1:semanas)+x$mean.start[2]-x$mean.start[1]
  lab.graf<-(1:semanas)+x$ci.start[2,2]-x$ci.start[1,2]
  lab.graf[lab.graf>52]<-(lab.graf-52)[lab.graf>52]
  lab.graf[lab.graf<1]<-(lab.graf+52)[lab.graf<1]
  tipos<-rep(1,anios)
  anchos<-rep(2,anios)
  pal <- colorRampPalette(brewer.pal(4,"Spectral"))
  colores<-pal(anios)
  #colores<-c(rgb(runif(anios-1),runif(anios-1),runif(anios-1)),"#FF0000")
  # limite.superior<-c(1.05*max.fix.na(datos.graf))
  otick<-optimal.tickmarks(0,max.fix.na(datos.graf),10)
  range.y<-c(otick$range[1],otick$range[2]+otick$by/2)
  matplot(1:semanas,
          datos.graf,
          type="l",
          sub=paste("Weekly incidence rates of the seasons matching their relative position in the model."),
          lty=tipos,
          lwd=anchos,
          col=colores,
          xlim=c(1,semanas),
          #xlab="Week",
          #ylab="Rate",
          xlab="",
          ylab="",
          axes=F,
          #font.axis=1,
          #font.lab=1,
          font.main=2,
          font.sub=1,
          ylim=range.y)
  # Ejes
  axis(1,at=seq(1,semanas,1),labels=F,cex.axis=0.7,col.axis="#404040",col="#C0C0C0")
  axis(1,at=seq(1,semanas,2),tick=F,
       labels=as.character(lab.graf)[seq(1,semanas,2)],cex.axis=0.7,col.axis="#404040",col="#C0C0C0")
  axis(1,at=seq(2,semanas,2),tick=F,
       labels=as.character(lab.graf)[seq(2,semanas,2)],cex.axis=0.7,line=0.60,col.axis="#404040",col="#C0C0C0")
  mtext(1,text="Week",line=2,cex=0.8,col="#000040")
  axis(2,at=otick$tickmarks,lwd=1,cex.axis=0.6,col.axis="#404040",col="#C0C0C0")
  mtext(2,text="Weekly rate",line=1.3,cex=0.8,col="#000040")
  mtext(4,text=paste("mem R library - Jos",rawToChar(as.raw(233))," E. Lozano - https://github.com/lozalojo/mem",sep=""),
        line=0.75,cex=0.6,col="#404040")
  i.temporada<-x$ci.start[1,2]
  f.temporada<-x$ci.start[1,2]+x$mean.length-1
  lines(x=rep(i.temporada-0.5,2),y=range.y,col="#FF0000",lty=2,lwd=2)
  lines(x=rep(f.temporada+0.5,2),y=range.y,col="#40FF40",lty=2,lwd=2)
  lines(x=c(1,semanas),y=rep(x$epidemic.thresholds[1],2),col="#8c6bb1",lty=2,lwd=2)
  # abline(v=c(i.temporada-0.5,f.temporada+0.5),col=c("#00C000","#FFB401"),lty=c(2,2))
  #ya<-range.y[2]*0.975
  ya<-otick$range[2]
  if ((i.temporada-1)<=(semanas-f.temporada)) xa<-f.temporada+1 else xa<-1
  legend(x=xa,y=ya,inset=c(0,0),xjust=0,seg.len=1,
           legend=names.seasons,
           bty="n",
           lty=tipos,
           lwd=anchos,
           col=colores,
           cex=0.75,
         x.intersp=0.5,
         y.intersp=0.6,
           text.col="#000000",
           ncol=1)

  # Graph 2
  # lineas.basicas<-rbind(matrix(rep(x$pre.post.intervals[1,1:3],i.temporada-1),ncol=3,byrow=T),
  #                       matrix(rep(NA,3*(f.temporada-i.temporada+1)),ncol=3),
  #                       matrix(rep(x$pre.post.intervals[2,1:3],semanas-f.temporada),ncol=3,byrow=T))
  # n.niveles<-dim(x$epi.intervals)[1]
  # limites.niveles<-as.vector(x$epi.intervals[1:n.niveles,4])
  # nombres.niveles<-as.character(x$epi.intervals[1:n.niveles,1])
  # limites.niveles[limites.niveles<0]<-0
  # limites.niveles.mas<-array(dim=c(semanas,n.niveles))
  # for (i in i.temporada:f.temporada) limites.niveles.mas[i,]<-limites.niveles
  # datos.graf<-as.data.frame(cbind(lineas.basicas[,3],x$typ.curve[,2],limites.niveles.mas))
  # etiquetas<-paste(round(limites.niveles,2)," (",as.numeric(nombres.niveles)*100,"%) ",sep="")
  # names(datos.graf)<-c("Threshold",etiquetas)
  # tipos<-c(1,2,rep(2,times=n.niveles))
  # anchos<-c(3,2,rep(2,times=n.niveles))
  # colores<-c("#FF0000","#800080","#C0C0FF","#8080FF","#4040FF","#0000C0","#000080")
  # matplot(1:semanas,
  #         datos.graf,
  #         type="l",
  #         sub=paste("MEM Threshold"),
  #         lty=tipos,
  #         lwd=anchos,
  #         col=colores,
  #         xlim=c(1,semanas),
  #         xlab="",
  #         ylab="",
  #         axes=F,
  #         font.main=2,
  #         font.sub=1,
  #         ylim=range.y)
  # axis(1,at=seq(1,semanas,1),labels=F,cex.axis=0.7,col.axis="#404040",col="#C0C0C0")
  # axis(1,at=seq(1,semanas,2),tick=F,
  #      labels=as.character(lab.graf)[seq(1,semanas,2)],cex.axis=0.7,col.axis="#404040",col="#C0C0C0")
  # axis(1,at=seq(2,semanas,2),tick=F,
  #      labels=as.character(lab.graf)[seq(2,semanas,2)],cex.axis=0.7,line=0.60,col.axis="#404040",col="#C0C0C0")
  # mtext(1,text="Week",line=2,cex=0.8,col="#000040")
  # axis(2,at=otick$tickmarks,lwd=1,cex.axis=0.6,col.axis="#404040",col="#C0C0C0")
  # mtext(2,text="Weekly rate",line=1.3,cex=0.8,col="#000040")
  # mtext(4,text=paste("mem R library - Jos",rawToChar(as.raw(233))," E. Lozano - https://github.com/lozalojo/mem",sep=""),
  #       line=0.75,cex=0.6,col="#404040")
  # xa<-c(i.temporada/2,1+rep(f.temporada,n.niveles))
  # ya<-c(lineas.basicas[1,3],limites.niveles)
  # texto<-c(paste("Threshold: ",round(lineas.basicas[1,3],2),sep=""),etiquetas)
  # posiciones<-c(3,rep(4,n.niveles))
  # tamanios<-c(0.75,rep(0.75,n.niveles))
  # colores<-c("#FF0000",colores[3:(3+n.niveles-1)])
  # text(xa,ya,texto,pos=posiciones,col=colores,cex=tamanios)


  temp1<-memsurveillance(i.current=data.frame(rates=x$typ.curve[,2], row.names = rownames(x$data)),
                         i.epidemic.thresholds = x$epidemic.thresholds,
                         i.intensity.thresholds = x$intensity.thresholds,
                         i.mean.length = x$mean.length,
                         i.force.length = T,
                         i.graph.file = F,
                         i.week.report = NA,
                         i.equal = F,
                         i.pos.epidemic = T,
                         i.no.epidemic = F,
                         i.no.intensity = F,
                         i.epidemic.start = x$ci.start[2,2],
                         i.range.x = as.numeric(c(rownames(x$data)[1],rownames(x$data)[semanas])),
                         i.range.x.53 = F,
                         i.range.y = NA,
                         i.no.labels = F,
                         i.start.end.marks = T)

  par(opar)
}

