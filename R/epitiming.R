#' Deprecated function(s) in the mem package
#'
#' These functions are provided for compatibility with older version of
#' the mem package.  They may eventually be completely
#' removed.
#'
#' @name epitiming
#'
#' @param i.data Original parameter deprecated in the new version
#' @param ... Parameters (that have not changed) to be passed to the modern version of the function
#'
#' @export
#'
#' @section Details:
#' \tabular{rl}{
#'   \code{epitiming} \tab now a synonym for \code{\link{memtiming}}
#' }
#'
epitiming <- function(i.data, ...) {
  .Deprecated("memtiming")
  memtiming(i.data = i.data, ...)
}
