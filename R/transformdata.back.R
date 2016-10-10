#' Tranform the historial series from the mem default input format (weeksxseasons)
#' to a single serie data.frame, with season, week and rate column
#'
#' @keywords internal
transformdata.back<-function(i.data,i.name,i.range.x=c(40,20)){
  if (i.range.x[1]>i.range.x[2]){
    full.range<-data.frame(week=c(i.range.x[1]:52,1:i.range.x[2]))
    full.range$no.week<-1:dim(full.range)[1]
  }else{
    full.range<-data.frame(week=i.range.x[1]:i.range.x[2])
    full.range$no.week<-1:dim(full.range)[1]
  }
  i.data$week<-rownames(i.data)
  datos<-merge(full.range,i.data,"week",all.x=T)
  datos<-datos[order(datos$no.week),]
  rownames(datos)<-datos$week
  datos$week<-NULL
  datos$no.week<-NULL
  seasons<-dim(datos)[2]
  weeks<-dim(datos)[1]
  # I need to determine if seasons are in one year-season data (f.i. "2010" - southern
  # hemisphere) or two years-season data (f.i. "2010/2011" - northern hemisphere)
  years<-regmatches(names(datos),gregexpr("\\d{4}", names(datos)))
  data.out<-data.frame()
  for (i in 1:seasons){
    data.out.i<-data.frame(season=names(datos)[i],
                        year=as.numeric(years[[i]][1]),
                        week=as.numeric(rownames(datos)),
                        data=datos[,i],stringsAsFactors = F)
    if (length(years[[i]])>1) if (years[[i]][2]!="" & !is.na(years[[i]][2])) data.out.i$year[data.out.i$week<30]<-as.numeric(years[[i]][2])
    data.out.i$yrweek<-data.out.i$year*100+data.out.i$week
    data.out<-rbind(data.out,data.out.i)
  }
  names(data.out)[4]<-i.name
  return(data.out)
}
