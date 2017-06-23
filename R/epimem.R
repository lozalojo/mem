#' Deprecated function(s) in the mem package
#'
#' These functions are provided for compatibility with older version of
#' the mem package.  They may eventually be completely
#' removed.
#'
#' @name epimem
#'
#' @param i.data Original parameter deprecated in the new version
#' @param i.tails Original parameter deprecated in the new version
#' @param i.levels Original parameter deprecated in the new version
#' @param i.type Original parameter deprecated in the new version
#' @param i.level Original parameter deprecated in the new version
#' @param ... Parameters (that have not changed) to be passed to the modern version of the function
#'
#' @export
#'
#' @section Details:
#' \tabular{rl}{
#'   \code{epimem} \tab now a synonym for \code{\link{memmodel}}\cr
#' }
#'
epimem <- function(i.data, i.tails=1, i.levels=c(0.40,0.90,0.975), i.type=2, i.level=0.95, ...) {
  .Deprecated("memmodel")
  memmodel(i.data = i.data,
           i.tails.threshold=i.tails,
           i.type.intensity=6,
           i.level.intensity=i.levels,
           i.tails.intensity=i.tails,
           i.type.other=i.type,
           i.level.other=i.level,
           ...)
}
