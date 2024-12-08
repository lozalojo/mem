#' create epidemiological calendar
#'
#' @keywords internal
create.calendar <- function(i.ini = as.POSIXct("1980-01-01"), i.fin = as.POSIXct(paste(format(Sys.Date(), "%Y"), "12", "31", sep = "-"))) {
  year <- weekday <- isini <- weeknum <- NULL
  temp1 <- data.frame(date = seq.POSIXt(from = i.ini - 60 * 60 * 24 * 400, to = i.fin + 60 * 60 * 24 * 400, by = "DSTday")) %>%
    mutate(
      year = as.numeric(strftime(date, "%Y")),
      weekday = as.numeric(format(date, format = "%u")),
      isini = (((weekday == 7 & date < as.POSIXct("2012-01-05")) | (weekday == 1 & date >= as.POSIXct("2012-01-05")))),
      weeknum = cumsum(isini)
    ) %>%
    select(-isini)
  n <- yearepi <- weekepi <- NULL
  temp2 <- temp1 %>%
    group_by(weeknum, year) %>%
    summarise(n = n()) %>%
    arrange(weeknum, desc(n), year) %>%
    group_by(weeknum) %>%
    slice(1) %>%
    select(weeknum, yearepi = year) %>%
    ungroup()
  temp3 <- temp1 %>%
    inner_join(temp2, by = "weeknum") %>%
    group_by(yearepi) %>%
    mutate(weekepi = weeknum - min(weeknum) + 1) %>%
    select(date, yearepi, weekepi) %>%
    ungroup()
  temp4 <- temp3 %>%
    filter(date %in% c(i.ini, i.fin)) %>%
    select(yearepi, weekepi) %>%
    inner_join(temp3, by = c("yearepi", "weekepi")) %>%
    summarise(mindate = min(date), maxdate = max(date))

  temp3 <- temp3 %>%
    filter(date >= temp4$mindate[1] & date <= temp4$maxdate[1])

  temp4 <- temp3 %>%
    group_by(yearepi, weekepi) %>%
    summarise(fini = min(date), ffin = max(date))
  list(calendar = temp3, epiweeks = temp4)
}
