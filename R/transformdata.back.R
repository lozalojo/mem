#' Data transformation
#'
#' Function \code{transformdata} transforms data from week,rate1,...,rateN to year,week,rate
#' format.
#'
#' Yet to be written
#'
#' @name transformdata.back
#'
#' @param i.data Data frame of input data.
#' @param i.name Name of the column that contains the values.
#' @param i.range.x surveillance week range
#' @param i.cutoff week where a new season start (when two years in a season are involved)
#' @param i.fun sumarize function
#'
#' @return
#' \code{transformdata.back} returns a data.frame with three columns, year, week and rate.
#'
#' @examples
#' # Castilla y Leon Influenza Rates data
#' data(flucyl)
#' # Transform data
#' newdata<-transformdata.back(flucyl)
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
transformdata.back<-function(i.data, i.name="rates", i.range.x=NA, i.cutoff=NA, i.fun=mean){
  i.range.x.default<-c(max(1,min(as.numeric(rownames(i.data)[1:3]))),min(52,max(as.numeric(rownames(i.data)[(NROW(i.data)-2):NROW(i.data)]))))
  if (any(is.na(i.range.x)) | !is.numeric(i.range.x) | length(i.range.x)!=2) i.range.x<-i.range.x.default
  if (is.na(i.cutoff)) i.cutoff<-i.range.x[1]
  # Input scheme numbering
  week.f<-i.range.x[1]
  week.l<-i.range.x[2]
  last.week<-53
  if (week.f>week.l){
    i.range.x.values<-data.frame(week.lab=c(week.f:last.week,1:week.l),week.no=1:(last.week-week.f+1+week.l))
  }else{
    i.range.x.values<-data.frame(week.lab=week.f:week.l,week.no=1:(week.l-week.f+1))
  }
  i.data$week.lab<-as.numeric(rownames(i.data))
  i.data<-merge(i.data,i.range.x.values, all.y=T, all.x=F, by="week.lab")
  i.data<-i.data[order(i.data$week.no),]
  rownames(i.data)<-i.data$week.lab
  i.data$week.lab<-NULL
  i.data$week.no<-NULL
  n.seasons<-dim(i.data)[2]
  n.weeks<-dim(i.data)[1]
  # dealing with season start and end, extracts information from rownames and gets season start/end
  seasons<-data.frame(names(i.data),str_match(names(i.data),"(\\d{4})(?:.*(\\d{4}))?(?:.*\\(.*(\\d{1,}).*\\))?")[,-1],stringsAsFactors = F)
  names(seasons)<-c("column","anioi","aniof","aniow")
  seasons[is.na(seasons)]<-""
  seasons$aniof[seasons$aniof==""]<-seasons$anioi[seasons$aniof==""]
  seasonsname<-seasons$anioi
  seasonsname[seasons$aniof!=""]<-paste(seasonsname[seasons$aniof!=""],seasons$aniof[seasons$aniof!=""],sep="/")
  seasonsname[seasons$aniow!=""]<-paste(seasonsname[seasons$aniow!=""],"(",seasons$aniow[seasons$aniow!=""],")",sep="")
  seasons$season<-seasonsname
  rm("seasonsname")
  names(i.data)<-seasons$season
  data.out<-data.frame()
  for (i in 1:n.seasons){
    week.i<-as.numeric(rownames(i.data))
    data.i<-i.data[,i]
    if (seasons$anioi[i]==seasons$aniof[i]){
      year.i<-rep(as.numeric(seasons$anioi[i]),n.weeks)
    }else{
      year.i<-rep(NA,n.weeks)
      year.i[week.i<i.cutoff]<-as.numeric(seasons$aniof[i])
      year.i[week.i>=i.cutoff]<-as.numeric(seasons$anioi[i])
    }
    yrweek.i<-year.i*100+week.i
    data.out.i<-data.frame(year=year.i,
                           week=week.i,
                           yrweek=yrweek.i,
                           data=data.i,stringsAsFactors = F)
    data.out<-rbind(data.out,data.out.i)
    rm("week.i","year.i","yrweek.i","data.i","data.out.i")
  }
  data.out<-aggregate(data ~ year + week + yrweek,data=data.out,FUN=i.fun,na.rm=T)
  data.out$season<-""
  # To determine if the resulting dataset has seasons with one (2011) or two years (2011/2012) we have
  # to check if there exists values prior to the cutoff point.
  if (any(data.out$week<i.cutoff)){
    # two years per season
    week.f<-max(i.cutoff,min(data.out$week[data.out$week>=i.cutoff]))
    week.l<-min(i.cutoff-1,max(data.out$week[data.out$week<i.cutoff]))
    i.range.x.values<-data.frame(week.lab=c(week.f:last.week,1:week.l),week.no=1:(last.week-week.f+1+week.l))
    data.out$season[data.out$week<i.cutoff]<-paste(data.out$year[data.out$week<i.cutoff]-1,data.out$year[data.out$week<i.cutoff],sep="/")
    data.out$season[data.out$week>=i.cutoff]<-paste(data.out$year[data.out$week>=i.cutoff],data.out$year[data.out$week>=i.cutoff]+1,sep="/")
    data.out<-merge(expand.grid(season=unique(data.out$season),week=i.range.x.values$week.lab, stringsAsFactors = F),data.out, all.x=T, by=c("season","week"))
    data.out$year[data.out$week>=i.cutoff]<-as.numeric(substr(data.out$season[data.out$week>=i.cutoff],1,4))
    data.out$year[data.out$week<i.cutoff]<-as.numeric(substr(data.out$season[data.out$week<i.cutoff],6,9))
  }else{
    # one year per season
    week.f<-min(data.out$week)
    week.l<-max(data.out$week)
    last.week<-53
    i.range.x.values<-data.frame(week.lab=week.f:week.l,week.no=1:(week.l-week.f+1))
    data.out$season<-paste(data.out$year,data.out$year,sep="/")
    data.out<-merge(expand.grid(season=unique(data.out$season),week=i.range.x.values$week.lab, stringsAsFactors = F),data.out, all.x=T, by=c("season","week"))
    data.out$year<-as.numeric(substr(data.out$season,1,4))
  }
  data.out$yrweek<-data.out$year*100+data.out$week
  data.out<-data.out[!(data.out$week==53 & is.na(data.out$data)),]
  data.out<-data.out[order(data.out$yrweek),]
  names(data.out)[names(data.out)=="data"]<-i.name
  return(data.out)
}
