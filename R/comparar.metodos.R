#' Compares outputs from two methods of locating the epidemic
#'
#' @keywords internal
comparar.metodos<-function(i.data){
  if (any(is.na(i.data))){
    resultado<-NA
  }else{  
    if (i.data[1]==3) i.data[1]<-1
    if (i.data[2]==3) i.data[2]<-1
    if (i.data[1]==0 | i.data[2]==0) resultado<-NA
    else if(i.data[1]==1 & i.data[2]==1) resultado<-"TN"
    else if(i.data[1]==1 & i.data[2]==2) resultado<-"FP"
    else if(i.data[1]==2 & i.data[2]==1) resultado<-"FN"
    else if(i.data[1]==2 & i.data[2]==2) resultado<-"TP"
    else resultado<-NA
  }
  return(resultado)
}
