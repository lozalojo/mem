#' Data transformation
#'
#' Function \code{transformdata.back} transforms data from week,rate1,...,rateN to year,week,rate
#' format.
#'
#' Yet to be written
#'
#' @name transformdata.back
#'
#' @param i.data Data frame of input data.
#' @param i.name Name of the column that contains the values.
#' @param i.cutoff.original Cutoff point between seasons when they have two years
#' @param i.range.x.final Range of the surveillance period in the output dataset
#' @param i.fun sumarize function
#'
#' @return
#' \code{transformdata.back} returns a data.frame with three columns, year, week and rate.
#'
#' @examples
#' # Castilla y Leon Influenza Rates data
#' data(flucyl)
#' # Transform data
#' newdata<-transformdata.back(flucyl)$data
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
#' @importFrom stats aggregate
#' @importFrom reshape2 melt
transformdata.back<-function(i.data, i.name="rates", i.cutoff.original=NA, i.range.x.final=NA, i.fun=sum){
  if (is.na(i.cutoff.original)) i.cutoff.original<-min(as.numeric(rownames(i.data)[1:3]))
  if (i.cutoff.original < 1) i.cutoff.original <- 1
  if (i.cutoff.original > 53) i.cutoff.original <- 53
  if (any(is.na(i.range.x.final)) | !is.numeric(i.range.x.final) | length(i.range.x.final)!=2) i.range.x.final<-c(min(as.numeric(rownames(i.data)[1:3])),max(as.numeric(rownames(i.data)[(NROW(i.data)-2):NROW(i.data)])))
  if (i.range.x.final[1] < 1) i.range.x.final[1] <- 1
  if (i.range.x.final[1] > 53) i.range.x.final[1] <- 53
  if (i.range.x.final[2] < 1) i.range.x.final[2] <- 1
  if (i.range.x.final[2] > 53) i.range.x.final[2] <- 53
  if (i.range.x.final[1] == i.range.x.final[2]) i.range.x.final[2] <- i.range.x.final[2] - 1
  if (i.range.x.final[2]==0) i.range.x.final[2]<-53
  n.seasons<-NCOL(i.data)
  # First: analize names of seasons and seasons with week 53
  if (n.seasons>1){
    seasons<-data.frame(names(i.data),matrix(str_match(names(i.data),"(\\d{4})(?:.*(\\d{4}))?(?:.*\\(.*(\\d{1,}).*\\))?"),nrow=n.seasons,byrow=F)[,-1],stringsAsFactors = F)
  }else{
    seasons<-data.frame(t(c(names(i.data),str_match(names(i.data),"(\\d{4})(?:.*(\\d{4}))?(?:.*\\(.*(\\d{1,}).*\\))?")[-1])),stringsAsFactors = F)
  }
  names(seasons)<-c("column","anioi","aniof","aniow")
  seasons[is.na(seasons)]<-""
  seasons$aniof[seasons$aniof==""]<-seasons$anioi[seasons$aniof==""]
  seasonsname<-seasons$anioi
  seasonsname[seasons$aniof!=""]<-paste(seasonsname[seasons$aniof!=""],seasons$aniof[seasons$aniof!=""],sep="/")
  seasonsname[seasons$aniow!=""]<-paste(seasonsname[seasons$aniow!=""],"(",seasons$aniow[seasons$aniow!=""],")",sep="")
  seasons$season<-seasonsname
  rm("seasonsname")
  names(i.data)<-seasons$season
  i.data$week<-as.numeric(row.names(i.data))
  # Second: Transform the data, summarize (to avoid duplicates) and remove na's
  data.out<-melt(i.data, "week", variable="season", value.name = "data", na.rm = T)
  # adds year, based in the i.cutoff.original value
  data.out$year<-NA
  data.out$year[data.out$week<i.cutoff.original]<-as.numeric(substr(data.out$season,6,9))[data.out$week<i.cutoff.original]
  data.out$year[data.out$week>=i.cutoff.original]<-as.numeric(substr(data.out$season,1,4))[data.out$week>=i.cutoff.original]
  data.out$season<-NULL
  # we aggregate in case data comes from two sources, for example when there are two parts of the same epidemic, notated as (1) and (2)
  data.out<-aggregate(data ~ year + week, data=data.out, FUN=i.fun, na.rm=T)
  # Third: create the structure of the final dataset, considering the i.range.x.final
  week.f<-i.range.x.final[1]
  week.l<-i.range.x.final[2]
  if (week.f>week.l){
    i.range.x.values.52<-data.frame(week=c(week.f:52,1:week.l),week.no=1:(52-week.f+1+week.l))
    i.range.x.values.53<-data.frame(week=c(week.f:53,1:week.l),week.no=1:(53-week.f+1+week.l))
    data.out$season<-""
    data.out$season[data.out$week<week.f]<-paste(data.out$year-1,data.out$year,sep="/")[data.out$week<week.f]
    data.out$season[data.out$week>=week.f]<-paste(data.out$year,data.out$year+1,sep="/")[data.out$week>=week.f]
    seasons.all<-unique(data.out$season)
    seasons.53<-unique(subset(data.out,data.out$week==53 & !is.na(data.out$data))$season)
    seasons.52<-seasons.all[!(seasons.all %in% seasons.53)]
    data.scheme<-rbind(merge(data.frame(season=seasons.52, stringsAsFactors = F),i.range.x.values.52, stringsAsFactors = F),
                       merge(data.frame(season=seasons.53, stringsAsFactors = F),i.range.x.values.53, stringsAsFactors = F))
    data.scheme$year<-NA
    data.scheme$year[data.scheme$week<week.f]<-as.numeric(substr(data.scheme$season,6,9))[data.scheme$week<week.f]
    data.scheme$year[data.scheme$week>=week.f]<-as.numeric(substr(data.scheme$season,1,4))[data.scheme$week>=week.f]
  }else{
    i.range.x.values.52<-data.frame(week=week.f:min(52,week.l),week.no=1:(min(52,week.l)-week.f+1))
    i.range.x.values.53<-data.frame(week=week.f:week.l,week.no=1:(week.l-week.f+1))
    data.out$season<-""
    data.out$season<-paste(data.out$year,data.out$year,sep="/")
    seasons.all<-unique(data.out$season)
    seasons.53<-unique(subset(data.out,data.out$week==53 & !is.na(data.out$data))$season)
    seasons.52<-seasons.all[!(seasons.all %in% seasons.53)]
    data.scheme<-rbind(merge(data.frame(season=seasons.52, stringsAsFactors = F),i.range.x.values.52, stringsAsFactors = F),
                       merge(data.frame(season=seasons.53, stringsAsFactors = F),i.range.x.values.53, stringsAsFactors = F))
    data.scheme$year<-NA
    data.scheme$year<-as.numeric(substr(data.scheme$season,1,4))
  }
  data.final<-merge(data.scheme,data.out,by=c("season","year","week"), all.x=T)
  data.final$yrweek<-data.final$year*100+data.final$week
  data.final$week.no<-NULL
  data.final<-data.final[order(data.final$yrweek),]
  names(data.final)[names(data.final)=="data"]<-i.name
  transformdata.back.output <- list(data = data.final)
  transformdata.back.output$call <- match.call()
  return(transformdata.back.output)
}
