#' Tranform the historial series from the mem default input format (weeksxseasons)
#' to a single serie data.frame, with season, week and rate column
#'
#' @keywords internal
transformdata.back<-function(i.data,i.name="rates",i.range.x=c(1,53)){
  n.seasons<-dim(i.data)[2]
  n.weeks<-dim(i.data)[1]
  # Lets look at the format of the i.data
  # I need to determine if seasons are in one year-season data (f.i. "2010" - southern
  # hemisphere) or two years-season data (f.i. "2010/2011" - northern hemisphere)
  years<-regmatches(names(i.data),gregexpr("\\d{4}", names(i.data)))
  years<-matrix(unlist(years), byrow=T, nrow=n.seasons)
  class(years)<-"numeric"
  years<-data.frame(years,stringsAsFactors = F)
  data.out<-data.frame()
  for (i in 1:n.seasons){
    data.out.i<-data.frame(year=years[i,1],
                           week=as.numeric(rownames(i.data)),
                           yrweek=NA,
                           season="",
                           data=i.data[,i],stringsAsFactors = F)
    if (dim(years)[2]>1) if (!is.na(years[i,2]) & years[i,1]!=years[i,2]) data.out.i$year[data.out.i$week<as.numeric(rownames(i.data)[1])]<-years[i,2]
    data.out.i$yrweek<-data.out.i$year*100+data.out.i$week
    data.out<-rbind(data.out,data.out.i)
  }
  data.out<-data.out[order(data.out$yrweek),]
  data.out$dummy<-1
  data.out.1<-aggregate(dummy ~ year + week + yrweek + season,data=data.out,FUN=NROW)
  data.out.2<-aggregate(data ~ year + week + yrweek + season,data=data.out,FUN=mean,na.rm=T)
  data.out<-merge(data.out.1,data.out.2,by=c("year","week","yrweek","season"),all.x=T)
  data.out$dummy<-NULL
  data.out<-data.out[order(data.out$yrweek),]
  
  data.out<-data.out[!(data.out$week==53 & is.na(data.out$data)),]
  names(data.out)[names(data.out)=="data"]<-i.name
  # Now limit to the period
  if (i.range.x[1]>i.range.x[2]){
    season.break<-i.range.x[1]
    if (i.range.x[1]>(i.range.x[2]+1)) data.out<-data.out[!(data.out$week %in% (i.range.x[2]+1):(i.range.x[1]-1)),]
    data.out$season<-paste(data.out$year,data.out$year+1,sep="/")
    data.out$season[data.out$week<season.break]<-paste(data.out$year[data.out$week<season.break]-1,data.out$year[data.out$week<season.break],sep="/")
  }else{
    data.out<-data.out[data.out$week %in% (i.range.x[1]:i.range.x[2]),]
    data.out$season<-as.character(data.out$year)
  }
  return(data.out)
}
