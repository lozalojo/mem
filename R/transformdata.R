#' Data transformation
#'
#' Function \code{transformdata} transforms data from year,week,rate to week,rate1,...,rateN
#' suitable to use with mem.
#'
#' Yet to be written
#'
#' @name transformdata
#'
#' @param i.data Data frame of input data.
#' @param i.range.x First and last surveillance week.
#' @param i.name Name of the column to transform.
#' @param i.max.na.per maximum percentage of na's in a season allowable, otherwise, the season is removed
#'
#' @return
#' \code{transformdata} returns a data.frame where each column has a different season and
#' rownames are the name of the epidemiological week.
#'
#' @examples
#' # Castilla y Leon Influenza Rates data
#' data(flucylraw)
#' # Transform data
#' newdata<-transformdata(flucylraw, i.range.x=c(40,20))$data
#' epi<-memmodel(newdata)
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
#' @importFrom reshape2 dcast
#' @importFrom stringr str_match
transformdata <- function(i.data, i.range.x = NA, i.name = "rates", i.max.na.per = 100) {
  if (any(is.na(i.range.x)) | !is.numeric(i.range.x) | length(i.range.x)!=2) i.range.x<-c(min(as.numeric(rownames(i.data)[1:(min(3,NROW(i.data)))])),max(as.numeric(rownames(i.data)[(min(1,NROW(i.data)-2)):NROW(i.data)])))
  if (i.range.x[1] < 1) i.range.x[1] <- 1
  if (i.range.x[1] > 53) i.range.x[1] <- 53
  if (i.range.x[2] < 1) i.range.x[2] <- 1
  if (i.range.x[2] > 53) i.range.x[2] <- 53
  if (i.range.x[1] == i.range.x[2]) i.range.x[2] <- i.range.x[2] - 1
  if (i.range.x[2]==0) i.range.x[2]<-53
  # Input scheme numbering
  week.f<-i.range.x[1]
  week.l<-i.range.x[2]
  data <- subset(i.data, select = c("year", "week", i.name))
  names(data)[names(data) == i.name] <- "rates"
  data$season<-""
  if (week.f>week.l){
    i.range.x.length<-52-week.f+1+week.l
    # Scheme numbering for seasons without season 53
    i.range.x.values.52<-data.frame(week=c(week.f:52,1:week.l),week.no=1:i.range.x.length)
    # Scheme numbering for seasons with season 53
    i.range.x.values.53<-data.frame(week=c(week.f:53,1:(week.l-1)),week.no=1:i.range.x.length)
    data$season[data$week<week.f]<-paste(data$year[data$week<week.f]-1,data$year[data$week<week.f],sep="/")
    data$season[data$week>=week.f]<-paste(data$year[data$week>=week.f],data$year[data$week>=week.f]+1,sep="/")
    seasons.all<-unique(data$season)
    seasons.53<-unique(subset(data,data$week==53)$season)
    seasons.52<-seasons.all[!(seasons.all %in% seasons.53)]
    data.out<-rbind(merge(data.frame(season=seasons.52, stringsAsFactors = F),i.range.x.values.52, stringsAsFactors = F),
                    merge(data.frame(season=seasons.53, stringsAsFactors = F),i.range.x.values.53, stringsAsFactors = F))
    data.out<-merge(data.out,data,by=c("season","week"),all.x=T)
    data.out$year[data.out$week>=week.f]<-as.numeric(substr(data.out$season[data.out$week>=week.f],1,4))
    data.out$year[data.out$week<week.f]<-as.numeric(substr(data.out$season[data.out$week<week.f],6,9))
  }else{
    i.range.x.length<-week.l-week.f+1
    if (week.l==53){
      i.range.x.values.52<-data.frame(week=week.f:52,week.no=1:(i.range.x.length-1))
      i.range.x.values.53<-data.frame(week=(week.f+1):53,week.no=1:(i.range.x.length-1))
    }else{
      i.range.x.values.52<-data.frame(week=week.f:week.l,week.no=1:i.range.x.length)
      i.range.x.values.53<-data.frame(week=week.f:week.l,week.no=1:i.range.x.length)
    }
    data$season<-paste(data$year,data$year,sep="/")
    seasons.all<-unique(data$season)
    seasons.53<-unique(subset(data,data$week==53)$season)
    seasons.52<-seasons.all[!(seasons.all %in% seasons.53)]
    data.out<-rbind(merge(data.frame(season=seasons.52, stringsAsFactors = F),i.range.x.values.52, stringsAsFactors = F),
                    merge(data.frame(season=seasons.53, stringsAsFactors = F),i.range.x.values.53, stringsAsFactors = F))
    data.out<-merge(data.out,data,by=c("season","week"),all.x=T)
    data.out$year<-as.numeric(substr(data.out$season,1,4))
  }
  data.out$yrweek<-data.out$year*100+data.out$week
  data.out<-subset(data.out,!is.na(data.out$week.no))
  data.out$week<-NULL
  data.out <- dcast(data.out, formula = week.no ~ season, fun.aggregate = NULL,
                    value.var = "rates")
  data.out<-merge(i.range.x.values.52,data.out,by="week.no",all.x=T)
  data.out <- data.out[apply(data.out, 2, function(x) sum(is.na(x))/length(x) < i.max.na.per/100)]
  data.out <- data.out[order(data.out$week.no), ]
  rownames(data.out) <- data.out$week
  data.out$week <- NULL
  data.out$week.no <- NULL

  transformdata.output <- list(data = data.out)
  transformdata.output$call <- match.call()
  return(transformdata.output)
}
