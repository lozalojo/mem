#' @title Goodness of fit of the mem
#'
#' @description
#' Function \code{memgoodness} calculates different indicators related to the goodness of the MEM
#' for detecting the epidemics, using data from the model and using all data in the original dataset.
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
#' \itemize{
#'   \item{validity.data} {data for each value analysed.}
#'   \item{results} {Total weeks, non-missing weeks, true positives, false positives
#' true negatives, false negatives, sensitivity, specificity .}
#'   \item{peaks} {distribution of the levels of intensity of the peaks.}
#'   \item{peaks.data} {Peak value, week of the peak value, epidemic and intensity thresholds and intensity level of each season analysed.}
#' }
#'
#' @details
#' The indicators calculated are sensitivity, specificity, positive predictive value, negative
#' predictive value, percent agreement and the Matthews correlation coefficient.
#'
#' How goodness is calculated:
#'
#' MEM calculates goodness indicators in an iterative process. In each iteration:
#'
#' \enumerate{
#' \item For one particular season the timing is calculated to determine which weeks are inside
#' the pre, post and epidemic periods. This is used as the real data: a real positive outcome
#' (epidemic weeks) and a real negative outcome (pre and post-epidemic weeks).
#' \item With a set of seasons, pre-epidemic threshold is calculated. This threshold is compared
#' with values from the season selected in the first step and see if values are above or below the
#' threshold. This is used as the observed data: an observed positive outcome (week value above
#' the threshold), and observed negative outcome (week value below the threshold).
#' \item Each week has a real and an observed outcome, so it can be classified in:
#' \itemize{
#' \item True positives (TP): real positive, observed positive: values of the epidemic period above the threshold.
#' \item True negatives (TN): real negative, observed negative: values of the non-epidemic period below the threshold.
#' \item False positives (FP): real negative, observed positive: values of the non-epidemic period above the threshold.
#' \item False negatives (FN): real positive, observed negative: values of the epidemic period below the threshold.
#' }
#' \item The process is repeated for each season in the dataset (each iteration a different value
#' until all seasons have been processed).
#' \item All TP, TN, FP and FN are pooled together and sensitivity, specificity, positive predictive value,
#' negative predictive value, percent agreement and the Matthews correlation coefficient are calculated.
#' }
#'
#' There are two ways of deciding the set of seasons used to calculate the pre-epidemic threshold in each
#' iteration and it is determined by the \code{i.goodness.method}.
#'
#' \itemize{
#' \item Cross: For each value, the surrounding seasons (after or before the current value) are selected up
#' to the number of Max. seasons (parameter of the Model box). To calculate the thresholds for season 2010/2011,
#' data from 2005/2006 to 2009/2010 and from 2011/20012 to 2015/2016 will be taken.
#' \item Sequential: Only preceding seasons are used (before the current value) up to the number of Max. seasons.
#' To calculate the thresholds for season 2010/2011, data from 2000/2001 to 2009/2010 are taken.
#' }
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
                                           i.graph.name=paste(i.prefix," Goodness ",i,sep=""))
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
                                           i.graph.name=paste(i.prefix," Goodness ",i,sep=""))
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
                          param.prefix=i.prefix,
                          param.min.seasons=i.min.seasons)
  memgoodness.output$call<-match.call()
  return(memgoodness.output)
}

