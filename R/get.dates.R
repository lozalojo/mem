#' create epidemiological calendar
#'
#' @keywords internal
get.dates <- function(i.yrweek) {
  ye <- floor(i.yrweek / 100)
  we <- i.yrweek - ye * 100
  yearepi <- weekepi <- NULL
  create.calendar(
    as.POSIXct(paste(ye, "01", "01", sep = "-")),
    as.POSIXct(paste(ye, "12", "31", sep = "-"))
  ) %>%
    purrr::pluck("epiweeks") %>%
    dplyr::filter(yearepi == ye & weekepi == we)
}
