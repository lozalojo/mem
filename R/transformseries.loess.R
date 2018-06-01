#' loess transformation
#'
#' @keywords internal
transformseries.loess<-function(i.x, i.span=NA){
  if (is.na(i.span)) i.span <- 0.15
  i.x[is.nan(i.x)] <- NA
  i.x[is.infinite(i.x)] <- NA
  xy.data<-data.frame(dx=1:length(i.x), dy=i.x)
  if (all(is.na(xy.data$dy))){
    xt<-xy.data$dy
  }else{
    loessMod <- try(loess(dy ~ dx, data=xy.data, span=i.span), silent=T)
    res <- try(predict(loessMod, newdata=xy.data$dx), silent=T)
    if("try-error" %in% class(res)){
      xt<-xy.data$dy
    }else{
      xt<-res
    }
  }
  return(xt)
}