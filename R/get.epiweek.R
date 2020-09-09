#' create epidemiological calendar
#'
#' @keywords internal
get.epiweek <- function(i.date = as.POSIXct(as.POSIXlt(Sys.Date()), tz = "CET")) {
  create.calendar(i.date - 60 * 60 * 24 * 400, i.date + 60 * 60 * 24 * 400) %>%
    purrr::pluck("calendar") %>%
    dplyr::filter(date == i.date)
}
