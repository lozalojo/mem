#' @title Data transformation
#'
#' @description
#' Function \code{transformdata} transforms data from year,week,rate to week,rate1,...,rateN
#' suitable to use with mem.
#'
#' @name transformdata
#'
#' @param i.data Data frame of input data.
#' @param i.range.x First and last surveillance week.
#' @param i.name Name of the column to transform.
#' @param i.max.na.per maximum percentage of na's in a season allowable, otherwise, the season is removed
#' @param i.function function used to aggregate data when duplicate values are found for the same season and week, defaults to NULL (no aggregate function)
#'
#' @return
#' \code{transformdata} returns a data.frame where each column has a different season and
#' rownames are the name of the epidemiological week.
#'
#' @details
#'
#' Transform data from format year, week, rate to a format suitable to be used
#' with \link{mem}, that is, one column with each season in the dataset and the
#' week names as rownames of the data.frame.
#'
#' @examples
#' # Castilla y Leon Influenza Rates data
#' data(flucylraw)
#' # Transform data
#' newdata <- transformdata(flucylraw, i.range.x = c(40, 20))$data
#' epi <- memmodel(newdata)
#' print(epi)
#' summary(epi)
#' plot(epi)
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
#' @importFrom stats aggregate
#' @importFrom tidyr spread
#' @importFrom dplyr select %>% filter
transformdata <- function(i.data, i.range.x = NA, i.name = "rates", i.max.na.per = 100, i.function = NULL) {
  if (is.null(i.range.x)) i.range.x <- NA
  if (any(is.na(i.range.x)) || !is.numeric(i.range.x) || length(i.range.x) != 2) i.range.x <- c(min(as.numeric(i.data$week)), max(as.numeric(i.data$week)))
  if (i.range.x[1] < 1) i.range.x[1] <- 1
  if (i.range.x[1] >= 52) i.range.x[1] <- 52
  if (i.range.x[2] < 1) i.range.x[2] <- 1
  if (i.range.x[2] >= 52) i.range.x[2] <- 52
  if (i.range.x[1] == i.range.x[2]) i.range.x[2] <- i.range.x[2] - 1
  if (i.range.x[2] == 0) i.range.x[2] <- 52
  # Input scheme numbering
  if (!all(c("year", "week") %in% tolower(names(i.data)))) stop("Input data must have a year, week, rate format\n")
  if (!(i.name %in% names(i.data))) stop(paste0(i.name, " variable not found in input data\n"))
  data <- i.data[tolower(names(i.data)) %in% c("year", "week") | names(i.data) %in% i.name]
  names(data)[names(data) == i.name] <- "rates"
  names(data) <- tolower(names(data))
  year <- week <- NULL
  data <- data %>%
    filter(!is.na(year) & !is.na(week))

  week.f <- i.range.x[1]
  week.l <- i.range.x[2]
  data$season <- ""
  if (week.f > week.l) {
    i.range.x.length <- 52 - week.f + 1 + week.l
    # Scheme numbering for seasons without season 53
    i.range.x.values.52 <- data.frame(week = c(week.f:52, 1:week.l), week.no = 1:i.range.x.length)
    # Scheme numbering for seasons with season 53
    i.range.x.values.53 <- data.frame(week = c(week.f:53, 1:(week.l - 1)), week.no = 1:i.range.x.length)
    data$season[data$week < week.f] <- paste(data$year[data$week < week.f] - 1, data$year[data$week < week.f], sep = "/")
    data$season[data$week >= week.f] <- paste(data$year[data$week >= week.f], data$year[data$week >= week.f] + 1, sep = "/")
    seasons.all <- unique(data$season)
    seasons.53 <- unique(subset(data, data$week == 53)$season)
    seasons.52 <- seasons.all[!(seasons.all %in% seasons.53)]
    data.out <- rbind(
      merge(data.frame(season = seasons.52, stringsAsFactors = FALSE), i.range.x.values.52, stringsAsFactors = FALSE),
      merge(data.frame(season = seasons.53, stringsAsFactors = FALSE), i.range.x.values.53, stringsAsFactors = FALSE)
    )
    data.out <- merge(data.out, data, by = c("season", "week"), all.x = TRUE)
    data.out$year[data.out$week >= week.f] <- as.numeric(substr(data.out$season[data.out$week >= week.f], 1, 4))
    data.out$year[data.out$week < week.f] <- as.numeric(substr(data.out$season[data.out$week < week.f], 6, 9))
  } else {
    i.range.x.length <- week.l - week.f + 1
    if (week.l == 53) {
      i.range.x.values.52 <- data.frame(week = week.f:52, week.no = 1:(i.range.x.length - 1))
      i.range.x.values.53 <- data.frame(week = (week.f + 1):53, week.no = 1:(i.range.x.length - 1))
    } else {
      i.range.x.values.52 <- data.frame(week = week.f:week.l, week.no = 1:i.range.x.length)
      i.range.x.values.53 <- data.frame(week = week.f:week.l, week.no = 1:i.range.x.length)
    }
    data$season <- paste(data$year, data$year, sep = "/")
    seasons.all <- unique(data$season)
    seasons.53 <- unique(subset(data, data$week == 53)$season)
    seasons.52 <- seasons.all[!(seasons.all %in% seasons.53)]
    data.out <- rbind(
      merge(data.frame(season = seasons.52, stringsAsFactors = FALSE), i.range.x.values.52, stringsAsFactors = FALSE),
      merge(data.frame(season = seasons.53, stringsAsFactors = FALSE), i.range.x.values.53, stringsAsFactors = FALSE)
    )
    data.out <- merge(data.out, data, by = c("season", "week"), all.x = TRUE)
    data.out$year <- as.numeric(substr(data.out$season, 1, 4))
  }
  data.out$yrweek <- data.out$year * 100 + data.out$week
  data.out <- subset(data.out, !is.na(data.out$week.no))
  data.out$week <- NULL
  # data.out <- dcast(data.out, formula = week.no ~ season, fun.aggregate = i.function,
  #                   value.var = "rates")
  # replace dcast for spread
  week.no <- season <- rates <- NULL
  if (!is.null(i.function)) data.out <- aggregate(rates ~ season + week.no, data = data.out, FUN = i.function)
  data.out <- data.out %>%
    select(week.no, season, rates) %>%
    spread(season, rates)
  data.out <- merge(i.range.x.values.52, data.out, by = "week.no", all.x = TRUE)
  data.out <- data.out[apply(data.out, 2, function(x) sum(is.na(x)) / length(x) < i.max.na.per / 100)]
  data.out <- data.out[order(data.out$week.no), ]
  rownames(data.out) <- data.out$week
  data.out$week <- NULL
  data.out$week.no <- NULL

  transformdata.output <- list(data = data.out)
  transformdata.output$call <- match.call()
  return(transformdata.output)
}
