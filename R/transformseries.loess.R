#' loess transformation
#'
#' @keywords internal
transformseries.loess <- function(i.x, i.positive = FALSE, ...) {
  i.x[is.nan(i.x)] <- NA
  i.x[is.infinite(i.x)] <- NA
  xy.data <- data.frame(dx = seq_len(length(i.x)), dy = i.x)
  if (all(is.na(xy.data$dy))) {
    xt <- xy.data$dy
  } else {
    loessMod <- try(loess(dy ~ dx, data = xy.data, ...), silent = TRUE)
    res <- try(predict(loessMod, newdata = xy.data$dx), silent = TRUE)
    if ("try-error" %in% class(loessMod) || "try-error" %in% class(res)) {
      xt <- xy.data$dy
    } else {
      if (i.positive) res[res < 0] <- 0
      xt <- res
    }
  }
  return(xt)
}
