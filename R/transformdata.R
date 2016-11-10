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
#' newdata<-transformdata(flucylraw)$tdata
#' epi<-memmodel(newdata)
#' print(epi)
#' summary(epi)
#' plot(epi)
#'
#' @author Jose E. Lozano \email{lozalojo@@gmail.com}
#'
#' @references
#' Vega T., Lozano J.E. (2004) Modelling influenza epidemic - can we detect the beginning
#' and predict the intensity and duration? International Congress Series 1263 (2004)
#' 281-283.\cr
#' Vega T., Lozano J.E. (2012) Influenza surveillance in Europe: establishing epidemic
#' thresholds by the Moving Epidemic Method. Influenza and Other Respiratory Viruses,
#' DOI:10.1111/j.1750-2659.2012.00422.x.
#'
#' @keywords influenza
#'
#' @export
#' @importFrom sqldf sqldf
#' @importFrom reshape2 dcast
transformdata <- function(i.data, i.range.x = c(40, 20), i.name = "rate", 
  i.max.na.per = 100) {
  
  i.week.first <- i.range.x[1]
  i.week.last <- i.range.x[2]
  
  if (is.na(i.week.first) | is.na(i.week.last)) 
    stop("Error in starting/ending week")
  
  if (i.week.first < 1) 
    i.week.first <- 1
  if (i.week.first > 53) 
    i.week.first <- 53
  if (i.week.last < 1) 
    i.week.last <- 1
  if (i.week.last > 53) 
    i.week.last <- 53
  if (i.week.first == i.week.last) 
    i.week.last <- i.week.last - 1
  
  data <- subset(i.data, select = c("year", "week", i.name))
  names(data)[names(data) == i.name] <- "rate"
  
  if (i.week.first < i.week.last) {
    # Formato de temporada único año (ej: 2010)
    data$season <- as.character(data$year)
    seasons <- data.frame(season = unique(data$season))
    weeks <- data.frame(week = i.week.first:i.week.last, 
      week.no = 1:(i.week.last - i.week.first + 1))
    esquema <- merge(weeks, seasons)
  } else {
    # Formato de temporada de dos años (ej: 2010/2011)
    data$season <- ifelse(data$week < i.week.first, paste(as.character(data$year - 
      1), as.character(data$year), sep = "/"), paste(as.character(data$year), 
      as.character(data$year + 1), sep = "/"))
    seasons.53 <- sqldf("select distinct season from data where week=53")
    weeks.53 <- data.frame(week = c(i.week.first:53, 1:(i.week.last - 
      1)), week.no = 1:(53 + i.week.last - i.week.first))
    seasons <- sqldf("select distinct season from data where season not in (select season from [seasons.53])")
    weeks <- data.frame(week = c(i.week.first:52, 1:i.week.last), 
      week.no = 1:(53 + i.week.last - i.week.first))
    esquema <- rbind(merge(weeks, seasons), merge(weeks.53, 
      seasons.53))
  }
  temp1 <- sqldf("select esquema.season, esquema.week, esquema.[week.no], data.rate from esquema left join data on (esquema.week=data.week and esquema.season=data.season)")
  temp2 <- temp1[order(temp1$season, temp1$week.no), ]
  temp3 <- dcast(temp2, formula = week.no ~ season, fun.aggregate = NULL, 
    value.var = "rate")
  temp4 <- temp3[apply(temp3, 2, function(x) any(!is.na(x)))]
  temp5 <- sqldf("select weeks.week, temp4.* from temp4 inner join weeks on (temp4.[week.no]=weeks.[week.no]) order by [week.no]")
  
  temp6 <- subset(temp5, !is.na(temp5$week))
  row.names(temp6) <- temp6$week
  temp6$week <- NULL
  temp6$week.no <- NULL
  
  # remove those seasons whose percentaje of NA is greater than
  # a parameter
  
  temp7 <- apply(temp6, 2, function(x) sum(is.na(x))/length(x))
  temp8 <- temp6[temp7 < i.max.na.per/100]
  
  transformdata.output <- list(tdata = temp8)
  transformdata.output$call <- match.call()
  return(transformdata.output)
}
