#' Goodness of fit of the mem
#'
#' Function \code{memgoodness} perform the goodness of fit of the mem
#'
#' To be written
#'
#' @name memgoodness
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
#' @param i.calculation.method method of determining true/false positives and true/false negatives.
#' @param i.goodness.method method to calculate goodness.
#' @param i.detection.values values to use in the i.param value of \code{memtiming}.
#' @param i.weeks.above number of weeks over the threshold to give the alert.
#' @param i.output output directory for graphs.
#' @param i.graph whether the graphs must be written or not.
#' @param i.prefix prefix used for naming graphs.
#' @param i.min.seasons minimum number of seasons to perform goodness, default=6.
#'
#' @return
#' \code{memgoodness} returns a list.
#' A list containing at least the following components:
#'   \item{validity.data}{data for each value analysed.}
#'   \item{results}{Total weeks, non-missing weeks, true positives, false positives
#' true negatives, false negatives, sensitivity, specificity .}
#'   \item{peaks}{distribution of the levels of intensity of the peaks.}
#'   \item{peaks.data}{Peak value, week of the peak value, epidemic and intensity thresholds and intensity level of each season analysed.}
#'
#' @examples
#' # Castilla y Leon Influenza Rates data
#' data(flucyl)
#' # Goodness of fit
#' epi.good<-memgoodness(flucyl,i.detection.values=seq(2.5,2.8,0.1))
#' epi.good$results
#' epi.good$peaks
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
memgoodness<-function(i.data,
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
                      i.iter.boot=10000,
                      i.calculation.method="default",
                      i.goodness.method="cross",
                      i.detection.values=seq(2.0,3.0,0.1),
                      i.weeks.above=1,
                      i.output=".",
                      i.graph=F,
                      i.prefix="",
                      i.min.seasons=6){

  anios<-dim(i.data)[2]
  semanas<-dim(i.data)[1]
  validacion<-array(NA,dim=c(14,anios))
  colnames(validacion)<-names(i.data)

  maximos<-numeric()
  maximos.seasons<-character()

  if (is.na(i.seasons)) i.seasons<-anios
  if (is.null(i.seasons)) i.seasons<-anios

  if (!(i.goodness.method=="sequential")){
    # Metodo 2: cruzada
    if (anios>=i.min.seasons){
      for (i in 1:anios){
        indices.2<-(1:anios)-i
        indices.1<-abs(indices.2)
        indices.modelo<-order(indices.1,indices.2)[2:(i.seasons+1)]
        indices.modelo<-sort(indices.modelo[!is.na(indices.modelo)])
        indices.actual<-i
        #cat(indices.actual,"\n")
        datos.actual<-i.data[indices.actual]
        datos.modelo<-memmodel(i.data[indices.modelo],
                               i.seasons=i.seasons,
                               i.type.threshold=i.type.threshold,
                               i.level.threshold=i.level.threshold,
                               i.tails.threshold=i.tails.threshold,
                               i.type.intensity=i.type.intensity,
                               i.level.intensity=i.level.intensity,
                               i.tails.intensity=i.tails.intensity,
                               i.type.curve=i.type.curve,
                               i.level.curve=i.level.curve,
                               i.type.other=i.type.other,
                               i.level.other=i.level.other,
                               i.method=i.method,
                               i.param=i.param,
                               i.n.max=i.n.max,
                               i.type.boot=i.type.boot,
                               i.iter.boot=i.iter.boot)
        validacion.i<-calcular.indicadores(i.current=datos.actual,
                                           i.umbral.pre=datos.modelo$pre.post.intervals[1,3],
                                           i.umbral.pos=datos.modelo$pre.post.intervals[2,3],
                                           i.intensidades=datos.modelo$epi.intervals[,4],
                                           i.duracion.intensidad=datos.modelo$mean.length,
                                           i.metodo.calculo=i.calculation.method,
                                           i.semanas.por.encima=i.weeks.above,
                                           i.valores.parametro.deteccion=i.detection.values,
                                           i.output=i.output,
                                           i.graph=i.graph,
                                           i.graph.name=paste(i.prefix,"[mem] Cross ",i,sep=""))
        validacion[,i]<-validacion.i$indicadores.t
        rownames(validacion)<-rownames(validacion.i$indicadores.t)
        # peak
        peak.i<-max.fix.na(datos.actual)
        peak.week.i<-as.numeric(row.names(datos.actual)[min((1:semanas)[peak.i==datos.actual], na.rm=T)])
        umbrales.i<-memintensity(datos.modelo)$intensity.thresholds
        if (is.na(umbrales.i[1])) umbrales.i[1]<-0
        if (umbrales.i[1]>umbrales.i[2]) umbrales.i[2]<-umbrales.i[1]*1.0000001
        level.i<-as.numeric(cut(peak.i,c(-Inf,umbrales.i,Inf)))
        maximos<-cbind(maximos,c(peak.i,peak.week.i,umbrales.i,level.i))
        maximos.seasons<-c(maximos.seasons,names(i.data)[indices.actual])
      }
    }
  }else{
    # Metodo 1: secuencial
    if (anios>=i.min.seasons){
      for (i in i.min.seasons:anios){
        indices.modelo<-max(1,i-i.seasons):(i-1)
        indices.actual<-i
        datos.actual<-i.data[indices.actual]
        datos.modelo<-memmodel(i.data[indices.modelo],
                               i.seasons=i.seasons,
                               i.type.threshold=i.type.threshold,
                               i.level.threshold=i.level.threshold,
                               i.tails.threshold=i.tails.threshold,
                               i.type.intensity=i.type.intensity,
                               i.level.intensity=i.level.intensity,
                               i.tails.intensity=i.tails.intensity,
                               i.type.curve=i.type.curve,
                               i.level.curve=i.level.curve,
                               i.type.other=i.type.other,
                               i.level.other=i.level.other,
                               i.method=i.method,
                               i.param=i.param,
                               i.n.max=i.n.max,
                               i.type.boot=i.type.boot,
                               i.iter.boot=i.iter.boot)
        validacion.i<-calcular.indicadores(i.current=datos.actual,
                                           i.umbral.pre=datos.modelo$pre.post.intervals[1,3],
                                           i.umbral.pos=datos.modelo$pre.post.intervals[2,3],
                                           i.intensidades=datos.modelo$epi.intervals[,4],
                                           i.duracion.intensidad=datos.modelo$mean.length,
                                           i.metodo.calculo=i.calculation.method,
                                           i.semanas.por.encima=i.weeks.above,
                                           i.valores.parametro.deteccion=i.detection.values,
                                           i.output=i.output,
                                           i.graph=i.graph,
                                           i.graph.name=paste(i.prefix,"[mem] Seque ",i,sep=""))
        validacion[,i]<-validacion.i$indicadores.t
        rownames(validacion)<-rownames(validacion.i$indicadores.t)
        # peak
        peak.i<-max.fix.na(datos.actual)
        peak.week.i<-as.numeric(row.names(datos.actual)[min((1:semanas)[peak.i==datos.actual])])
        umbrales.i<-memintensity(datos.modelo)$intensity.thresholds
        if (is.na(umbrales.i[1])) umbrales.i[1]<-0
        if (umbrales.i[1]>umbrales.i[2]) umbrales.i[2]<-umbrales.i[1]*1.0000001
        level.i<-as.numeric(cut(peak.i,c(-Inf,umbrales.i,Inf)))
        maximos<-cbind(maximos,c(peak.i,peak.week.i,umbrales.i,level.i))
        maximos.seasons<-c(maximos.seasons,names(i.data)[indices.actual])
      }
    }
  }

  resultado<-apply(validacion,1,sum,na.rm=T)
  # sensibilidad
  resultado[7]<-resultado[3]/(resultado[3]+resultado[6])
  # especificidad
  resultado[8]<-resultado[5]/(resultado[5]+resultado[4])
  # vpp
  resultado[9]<-resultado[3]/(resultado[3]+resultado[4])
  # vpn
  resultado[10]<-resultado[5]/(resultado[5]+resultado[6])
  # positive likehood ratio
  resultado[11]<-resultado[7]/(1-resultado[8])
  # negative likehood ratio
  resultado[12]<-(1-resultado[7])/resultado[8]
  # percentage agreement/accuracy
  resultado[13]<-(resultado[3]+resultado[5])/(resultado[3]+resultado[4]+resultado[5]+resultado[6])
  # Matthews correlation coefficient
  resultado[14]<-(resultado[3]*resultado[5]-resultado[4]*resultado[6])/sqrt((resultado[3]+resultado[4])*(resultado[3]+resultado[6])*(resultado[5]+resultado[4])*(resultado[5]+resultado[6]))

  resultado[is.nan(resultado)]<-NA

  temp1<-data.frame(Description=c("Baseline","Low","Medium","High","Very high"),Level=1:5,stringsAsFactors = F)
  maximos<-data.frame(t(maximos),stringsAsFactors = F)
  names(maximos)<-c("Peak","Peak week","Epidemic threshold","Medium threshold","High threshold","Very high threshold","Level")
  maximos$id  <- 1:NROW(maximos)
  maximos<-merge(maximos,temp1,by="Level",all.x=T)
  maximos<-maximos[order(maximos$id), ]
  rownames(maximos)<-maximos.seasons
  maximos$id<-NULL
  maximos<-maximos[c(2:7,1,8)]

  temp2<-data.frame(table(as.numeric(maximos[,7]), exclude=c(NA, NaN)),stringsAsFactors = F)
  names(temp2)<-c("Level","Count")
  temp3<-merge(temp1,temp2,all.x=T)
  temp3$Count[is.na(temp3$Count)]<-0
  temp3$Percentage<-temp3$Count/sum(temp3$Count)
  temp4<-data.frame(Level=c(0,-1),Description=c("No data seasons","Total seasons"),Count=c(NROW(maximos)-sum(temp3$Count),NROW(maximos)), Percentage=c((NROW(maximos)-sum(temp3$Count))/NROW(maximos),1),stringsAsFactors = F)
  maximos.resultados<-rbind(temp3,temp4)

  memgoodness.output<-list(validity.data=validacion,
                          results=resultado,
                          peaks=maximos.resultados,
                          peaks.data=maximos,
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
                          param.iter.boot=i.iter.boot,
                          param.calculation.method=i.calculation.method,
                          param.goodness.method=i.goodness.method,
                          param.detection.values=i.detection.values,
                          param.weeks.above=i.weeks.above,
                          param.output=i.output,
                          param.graph=i.graph,
                          param.prefix=i.prefix)
  memgoodness.output$call<-match.call()
  return(memgoodness.output)
}

