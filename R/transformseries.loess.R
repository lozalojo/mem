#' loess transformation
#'
#' @keywords internal
transformseries.loess <- function(i.x, i.positive = F, ...) {
  i.x[is.nan(i.x)] <- NA
  i.x[is.infinite(i.x)] <- NA
  xy.data <- data.frame(dx = 1:length(i.x), dy = i.x)
  if (all(is.na(xy.data$dy))) {
    xt <- xy.data$dy
  } else {
    loessMod <- try(loess(dy ~ dx, data = xy.data, ...), silent = T)
    res <- try(predict(loessMod, newdata = xy.data$dx), silent = T)
    if ("try-error" %in% class(loessMod) | "try-error" %in% class(res)) {
      xt <- xy.data$dy
    } else {
      if (i.positive) res[res < 0] <- 0
      xt <- res
    }
  }
  return(xt)
}
