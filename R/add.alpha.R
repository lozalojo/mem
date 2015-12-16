#' Add an alpha value to a colour
#' http://www.magesblog.com/2013/04/how-to-change-alpha-value-of-colours-in.html
#'
#' @keywords internal
#' @importFrom grDevices rgb col2rgb
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2,
        function(x)
          rgb(x[1], x[2], x[3], alpha=alpha))
}
