#' function to format output
#'
#' @keywords internal
output.ci<-function(i.type,i.level,i.tails){
  paste(switch(i.type,"Arithmetic mean","Geometric mean","Median","Median","Arithmetic mean","Geometric mean"),
        " and its ",
        switch(i.tails,"one sided","two sided"),
        " ",
        paste(i.level*100, collapse=","),
        "% CI using ",
        switch(i.type,"the normal approximation","the (log) normal approximation","the KC Method","bootstrap","2*SD","(log) 2*SD"),
        sep="")
}
