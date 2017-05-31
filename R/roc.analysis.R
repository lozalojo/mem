#' ROC analysis to find optimum parameter value
#'
#' Function \code{roc.analysis} perform a ROC analysis
#'
#' To be written
#'
#' @name roc.analysis
#'
#' @param i.data Data frame of input data.
#' @param i.param.values range of i.param values to test.
#' @param i.graph create a graph with the outputs (T/F).
#' @param i.graph.file write the graph to a file.
#' @param i.graph.file.name name of the output file.
#' @param i.graph.title title of the graph.
#' @param i.graph.subtitle subtitle of the graph.
#' @param i.output output directory.
#' @param ... other paramaters to be used by memgoodness function.
#'
#' @return
#' \code{roc.analysis} returns a list.
#' An object of class \code{mem} is a list containing at least the following components:
#'   \item{optimum}{optimum value.}
#'   \item{results}{Detailed results of each iteration.}
#'
#' @examples
#' # Castilla y Leon Influenza Rates data
#' data(flucyl)
#' # ROC analysis
#' epi.roc<-roc.analysis(flucyl,i.param.values=seq(2.6,2.8,0.1),i.detection.values=seq(2.6,2.8,0.1))
#' epi.roc$results
#'
#' @author Jose E. Lozano \email{lozalojo@@gmail.com}
#'
#' @references
#' Vega Alonso, Tomas, Jose E Lozano Alonso, Raul Ortiz de Lejarazu, and Marisol Gutierrez Perez. 2004.
#' Modelling Influenza Epidemic: Can We Detect the Beginning and Predict the Intensity and Duration?
#' International Congress Series, Options for the Control of Influenza V. Proceedings of the International
#' Conference on Options for the Control of Influenza V, 1263 (June): 281-83. doi:10.1016/j.ics.2004.02.121.\cr
#' Vega, Tomas, Jose Eugenio Lozano, Tamara Meerhoff, Rene Snacken, Joshua Mott, Raul Ortiz de Lejarazu, and
#' Baltazar Nunes. 2013. Influenza Surveillance in Europe: Establishing Epidemic Thresholds by the Moving
#' Epidemic Method. Influenza and Other Respiratory Viruses 7 (4): 546-58. doi:10.1111/j.1750-2659.2012.00422.x.\cr
#' Vega, Tomas, Jose E. Lozano, Tamara Meerhoff, Rene Snacken, Julien Beaute, Pernille Jorgensen, Raul Ortiz
#' de Lejarazu, et al. 2015. Influenza Surveillance in Europe: Comparing Intensity Levels Calculated Using
#' the Moving Epidemic Method. Influenza and Other Respiratory Viruses 9 (5): 234-46. doi:10.1111/irv.12330.
#'
#' @keywords influenza
#'
#' @export
roc.analysis <- function(i.data,
                         i.param.values = seq(1.5, 4.5, 0.1),
                         i.graph=F,
                         i.graph.file=F,
                         i.graph.file.name="",
                         i.graph.title="",
                         i.graph.subtitle="",
                         i.output=".", ...) {
    n.values <- length(i.param.values)
    resultados <- data.frame()

    for (i in 1:n.values) {
        cat("[", format(round(100 * (i - 1)/n.values, 1), digits = 3, nsmall = 1), "%][Parameter: ", format(round(i.param.values[i],
            1), digits = 3, nsmall = 1), "] Analysis started (", i, " out of ", n.values, ")\n", sep = "")
        # good.i<-memgoodness(i.data = i.data, i.method = 2,
        #     i.param = i.param.values[i], i.prefix = paste(i.prefix.roc, "[", format(round(i.param.values[i],
        #         1), digits = 3, nsmall = 1), "] ", sep = ""),...)
        good.i<-memgoodness(i.data = i.data, i.method = 2, i.param = i.param.values[i], i.graph=F, ...)

        resultados.i <- data.frame(value = i.param.values[i], t(good.i$results))
        resultados <- rbind(resultados, resultados.i)
    }

    names(resultados) <- tolower(names(resultados))

    if (!any(!is.na(resultados$sensitivity)) | !any(!is.na(resultados$specificity))) {
      rankings.1<-NA
      rankings.2<-NA
      rankings.5<-NA
        optimo.1 <- NA
        optimo.2 <- NA
        optimo.5 <- NA
    } else {
        # rankings.1<-rank(resultados$sensitivity,na.last=F)+rank(resultados$specificity,na.last=F)
        # optimo.1<-i.param.values[which.max(rankings.1)]
        rankings.1 <- rank(-resultados$sensitivity, na.last = T) + rank(-resultados$specificity, na.last = T)
        optimo.1 <- i.param.values[which.min(rankings.1)]

        # rankings.2<-rank(resultados$sensitivity*resultados$specificity,na.last=F)
        # optimo.2<-i.param.values[which.max(rankings.2)]
        rankings.2 <- rank(-resultados$sensitivity * resultados$specificity, na.last = T)
        optimo.2 <- i.param.values[which.min(rankings.2)]

        qf <- abs(resultados$sensitivity - resultados$specificity)
        qe <- 2 - resultados$sensitivity - resultados$specificity
        qs <- (1 - resultados$sensitivity)^2 + (1 - resultados$specificity)^2

        rankings.5 <- rank(qf) + rank(qe) + rank(qs)
        optimo.5 <- i.param.values[which.min(rankings.5)]
    }
    if (!any(!is.na(resultados$positive.likehood.ratio))) {
      rankings.3<-NA
        optimo.3 <- NA
    } else {
        # rankings.3<-rank(resultados$positive.likehood.ratio,na.last=F)
        # optimo.3<-i.param.values[which.max(rankings.3)]
        rankings.3 <- rank(-resultados$positive.likehood.ratio, na.last = T)
        optimo.3 <- i.param.values[which.min(rankings.3)]
    }
    if (!any(!is.na(resultados$negative.likehood.ratio))) {
      rankings.4<-NA
        optimo.4 <- NA
    } else {
        # rankings.4<-rank(resultados$negative.likehood.ratio,na.last=F)
        # optimo.4<-i.param.values[which.max(rankings.4)]
        rankings.4 <- rank(-resultados$negative.likehood.ratio, na.last = T)
        optimo.4 <- i.param.values[which.min(rankings.4)]
    }
    if (!any(!is.na(resultados$percent.agreement))) {
      rankings.6 <- NA
        optimo.6 <- NA
    } else {
        # rankings.6<-rank(resultados$percent.agreement,na.last=F)
        # optimo.6<-i.param.values[which.max(rankings.6)]
        rankings.6 <- rank(-resultados$percent.agreement, na.last = T)
        optimo.6 <- i.param.values[which.min(rankings.6)]
    }

    if (!any(!is.na(resultados$matthews.correlation.coefficient))) {
      rankings.7 <- NA
      optimo.7 <- NA
    } else {
      rankings.7 <- rank(-resultados$matthews.correlation.coefficient, na.last = T)
      optimo.7 <- i.param.values[which.min(rankings.7)]
    }


    optimum <- data.frame(pos.likehood = optimo.3, neg.likehood = optimo.4, aditive = optimo.1, multiplicative = optimo.2,
        mixed = optimo.5, percent = optimo.6, matthews=optimo.7)

    rankings <- data.frame(pos.likehood = rankings.3, neg.likehood = rankings.4, aditive = rankings.1, multiplicative = rankings.2,
                           mixed = rankings.5, percent = rankings.6, matthews=rankings.7)


    roc.analysis.output <- list(optimum = optimum, rankings = rankings, roc.data = resultados, param.data = i.data, param.param.values = i.param.values
                                #,param.prefix = i.prefix.roc
                                )
    roc.analysis.output$call <- match.call()


    if (i.graph){

      colores<-c("#EBEAEA","#5B9BD5","#ED7D31")

      if (i.graph.file.name=="") graph.name="roc analysis" else graph.name<-i.graph.file.name
      if (i.graph.file) tiff(filename=paste(i.output,"/",graph.name,".tiff",sep=""),width=8,height=6,units="in",pointsize="12",
                             compression="lzw",bg="white",res=300,antialias="none")

      opar<-par(mar=c(5,3,3,3)+0.1,mgp=c(3,0.5,0),xpd=T,mfrow=c(2,2))

      if (any(!is.na(resultados$sensitivity)) & any(!is.na(resultados$specificity))){
        d.x<-resultados$value
        d.y<-cbind(resultados$sensitivity,resultados$specificity)
        etiquetas<-c("Sensitivity","Specificity")
        otick<-optimal.tickmarks(0,1,10)
        range.y<-c(otick$range[1],otick$range[2]+otick$by/2)
        matplot(d.x,d.y,type="l",lty=rep(1,2),lwd=rep(1,2),col=colores[c(1,1)],xlab="",ylab="",axes=F,ylim=range.y,main=i.graph.title)
        points(d.x,d.y[,1],pch=19,type="p",col=colores[2],cex=0.5)
        points(d.x,d.y[,2],pch=19,type="p",col=colores[3],cex=0.5)
        axis(1,at=d.x,labels=d.x,cex.axis=0.7,col.axis="#404040",col="#C0C0C0")
        axis(2,at=otick$tickmarks,lwd=1,cex.axis=0.6,col.axis="#404040",col="#C0C0C0")
        mtext(1,text="Parameter",line=1.3,cex=0.8,col="#000040")
        mtext(2,text="Value",line=1.3,cex=0.8,col="#000040")
        mtext(3,text=i.graph.subtitle,cex=0.8,col="#000040")
        mtext(4,text=paste("mem R library - Jose E. Lozano - https://github.com/lozalojo/mem",sep=""),line=0.75,cex=0.6,col="#404040")
        legend(x="topright",y=NULL,inset=c(0,-0.05),xjust=0,legend=etiquetas,bty="n",lty=c(1,1),lwd=c(1,1),col=colores[c(1,1)],pch=c(21,21),pt.bg=colores[c(2,3)],cex=1,x.intersp=0.5,y.intersp=0.7,text.col="#000000",ncol=1)

      }

      if (any(!is.na(resultados$positive.predictive.value)) & any(!is.na(resultados$negative.predictive.value))){

        d.x<-resultados$value
        d.y<-cbind(resultados$positive.predictive.value,resultados$negative.predictive.value)
        etiquetas<-c("Positive predictive value","Negative predictive value")
        otick<-optimal.tickmarks(0,1,10)
        range.y<-c(otick$range[1],otick$range[2]+otick$by/2)
        matplot(d.x,d.y,type="l",lty=rep(1,2),lwd=rep(1,2),col=colores[c(1,1)],xlab="",ylab="",axes=F,ylim=range.y,main=i.graph.title)
        points(d.x,d.y[,1],pch=19,type="p",col=colores[2],cex=0.5)
        points(d.x,d.y[,2],pch=19,type="p",col=colores[3],cex=0.5)
        axis(1,at=d.x,labels=d.x,cex.axis=0.7,col.axis="#404040",col="#C0C0C0")
        axis(2,at=otick$tickmarks,lwd=1,cex.axis=0.6,col.axis="#404040",col="#C0C0C0")
        mtext(1,text="Parameter",line=1.3,cex=0.8,col="#000040")
        mtext(2,text="Value",line=1.3,cex=0.8,col="#000040")
        mtext(3,text=i.graph.subtitle,cex=0.8,col="#000040")
        mtext(4,text=paste("mem R library - Jose E. Lozano - https://github.com/lozalojo/mem",sep=""),line=0.75,cex=0.6,col="#404040")
        legend(x="topright",y=NULL,inset=c(0,-0.05),xjust=0,legend=etiquetas,bty="n",lty=c(1,1),lwd=c(1,1),col=colores[c(1,1)],pch=c(21,21),pt.bg=colores[c(2,3)],cex=1,x.intersp=0.5,y.intersp=0.7,text.col="#000000",ncol=1)
      }
      # d.x<-resultados$value
      # d.y<-resultados$percent.agreement
      # etiquetas<-c("Percent agreement")
      # otick<-optimal.tickmarks(0,1,10)
      # range.y<-c(otick$range[1],otick$range[2]+otick$by/2)
      # matplot(d.x,d.y,type="l",lty=rep(1,2),lwd=rep(1,2),col=colores[c(1,1)],xlab="",ylab="",axes=F,ylim=range.y,main=i.graph.title)
      # points(d.x,d.y,pch=19,type="p",col=colores[2],cex=0.5)
      # axis(1,at=d.x,labels=d.x,cex.axis=0.7,col.axis="#404040",col="#C0C0C0")
      # axis(2,at=otick$tickmarks,lwd=1,cex.axis=0.6,col.axis="#404040",col="#C0C0C0")
      # mtext(1,text="Parameter",line=1.3,cex=0.8,col="#000040")
      # mtext(2,text="Value",line=1.3,cex=0.8,col="#000040")
      # mtext(3,text=i.graph.subtitle,cex=0.8,col="#000040")
      # mtext(4,text=paste("mem R library - Jose E. Lozano - https://github.com/lozalojo/mem",sep=""),line=0.75,cex=0.6,col="#404040")
      # legend(x="topright",y=NULL,inset=c(0,-0.05),xjust=0,legend=etiquetas,bty="n",lty=c(1,1),lwd=c(1,1),col=colores[c(1,1)],pch=c(21,21),pt.bg=colores[c(2,3)],cex=1,x.intersp=0.5,y.intersp=0.7,text.col="#000000",ncol=1)

      if (any(!is.na(resultados$percent.agreement)) & any(!is.na(resultados$matthews.correlation.coefficient))){

        d.x<-resultados$value
        d.y<-cbind(resultados$percent.agreement,resultados$matthews.correlation.coefficient)
        etiquetas<-c("Percent agreement","Matthews correlation coefficient")
        otick<-optimal.tickmarks(0,1,10)
        range.y<-c(otick$range[1],otick$range[2]+otick$by/2)
        matplot(d.x,d.y,type="l",lty=rep(1,2),lwd=rep(1,2),col=colores[c(1,1)],xlab="",ylab="",axes=F,ylim=range.y,main=i.graph.title)
        points(d.x,d.y[,1],pch=19,type="p",col=colores[2],cex=0.5)
        points(d.x,d.y[,2],pch=19,type="p",col=colores[3],cex=0.5)
        axis(1,at=d.x,labels=d.x,cex.axis=0.7,col.axis="#404040",col="#C0C0C0")
        axis(2,at=otick$tickmarks,lwd=1,cex.axis=0.6,col.axis="#404040",col="#C0C0C0")
        mtext(1,text="Parameter",line=1.3,cex=0.8,col="#000040")
        mtext(2,text="Value",line=1.3,cex=0.8,col="#000040")
        mtext(3,text=i.graph.subtitle,cex=0.8,col="#000040")
        mtext(4,text=paste("mem R library - Jose E. Lozano - https://github.com/lozalojo/mem",sep=""),line=0.75,cex=0.6,col="#404040")
        legend(x="topright",y=NULL,inset=c(0,-0.05),xjust=0,legend=etiquetas,bty="n",lty=c(1,1),lwd=c(1,1),col=colores[c(1,1)],pch=c(21,21),pt.bg=colores[c(2,3)],cex=1,x.intersp=0.5,y.intersp=0.7,text.col="#000000",ncol=1)
      }

      if (any(!is.na(resultados$specificity)) & any(!is.na(resultados$sensitivity))){

        d.x<-1-resultados$specificity
        d.y<-resultados$sensitivity[order(d.x)]
        d.x<-d.x[order(d.x)]
        otick<-optimal.tickmarks(0,1,10)
        range.x<-c(otick$range[1],otick$range[2]+otick$by/2)
        range.y<-c(otick$range[1],otick$range[2]+otick$by/2)
        matplot(d.x,d.y,type="l",lty=rep(1,2),lwd=rep(1,2),col=colores[c(1,1)],xlab="",ylab="",axes=F,xlim=range.x,ylim=range.y,main=i.graph.title)
        points(d.x,d.y,pch=19,type="p",col=colores[2],cex=0.5)
        axis(1,at=otick$tickmarks,cex.axis=0.7,col.axis="#404040",col="#C0C0C0")
        axis(2,at=otick$tickmarks,lwd=1,cex.axis=0.6,col.axis="#404040",col="#C0C0C0")
        mtext(1,text="1 - specificity",line=1.3,cex=0.8,col="#000040")
        mtext(2,text="Sensitivity",line=1.3,cex=0.8,col="#000040")
        mtext(3,text=i.graph.subtitle,cex=0.8,col="#000040")
        mtext(4,text=paste("mem R library - Jose E. Lozano - https://github.com/lozalojo/mem",sep=""),line=0.75,cex=0.6,col="#404040")
      }
      par(opar)
      if (i.graph.file) dev.off()
    }

    return(roc.analysis.output)
}
