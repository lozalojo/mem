#' spline transformation
#'
#' @keywords internal
transformseries.spline <- function(i.x, i.positive = FALSE, ...) {
  i.x[is.nan(i.x)] <- NA
  i.x[is.infinite(i.x)] <- NA
  xy.data <- data.frame(dx = seq_len(length(i.x)), dy = i.x)
  if (all(is.na(xy.data$dy))) {
    xt <- xy.data$dy
  } else {
    dy <- NULL
    temp1 <- xy.data %>%
      filter(!is.na(dy))
    splineMod <- try(smooth.spline(temp1$dx, temp1$dy, ...), silent = TRUE)
    res <- try(predict(splineMod, x = xy.data$dx), silent = TRUE)
    if ("try-error" %in% class(splineMod) || "try-error" %in% class(res)) {
      xt <- xy.data$dy
    } else {
      if (i.positive) res$y[res$y < 0] <- 0
      xt <- res$y
    }
  }
  return(xt)
}
