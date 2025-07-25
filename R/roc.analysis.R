#' @title Analysis of different indicators to find the optimum value of the window parameter
#'
#' @description
#' Function \code{roc.analysis} perform a ROC analysis
#'
#' @name roc.analysis
#'
#' @param i.data Data frame of input data.
#' @param i.param.values range of i.param values to test.
#' @param i.min.seasons minimum number of seasons to perform the analysis, default=6.
#' @param i.graph create a graph with the outputs (TRUE/FALSE).
#' @param i.graph.file write the graph to a file.
#' @param i.graph.file.name name of the output file.
#' @param i.graph.title title of the graph.
#' @param i.graph.subtitle subtitle of the graph.
#' @param i.output output directory.
#' @param i.mem.info include information about the package in the graph.
#' @param i.min.sensitivity set a minimum sensitivity when finding the optimum
#' @param i.min.specificity set a minimum specificity when finding the optimum
#' @param ... other paramaters to be used by memgoodness function.
#'
#' @return
#' \code{roc.analysis} returns a list.
#' An object of class \code{mem} is a list containing at least the following components:
#' \describe{
#'   \item{optimum}{optimum value.}
#'   \item{results}{Detailed results of each iteration.}
#' }
#'
#' @details
#' Optimize is an iterative process that calculates goodness indicators using different window
#' parameters for the fixed criterium and compares all estimators in order to find the optimum
#' window parameter.
#'
#' The output shows the different window parameters and their respective indicators to decide
#' which one is better for your data.
#'
#' @examples
#' # Castilla y Leon Influenza Rates data
#' data(flucyl)
#' # ROC analysis
#' epi.roc <- roc.analysis(flucyl,
#'   i.param.values = seq(2.6, 2.8, 0.1),
#'   i.detection.values = seq(2.6, 2.8, 0.1)
#' )
#' epi.roc$results
#' @author Jose E. Lozano \email{lozalojo@@gmail.com}
#'
#' @references
#' Vega T, Lozano JE, Ortiz de Lejarazu R, Gutierrez Perez M. Modelling influenza epidemic - can we
#' detect the beginning and predict the intensity and duration? Int Congr Ser. 2004 Jun;1263:281-3.
#'
#' Vega T, Lozano JE, Meerhoff T, Snacken R, Mott J, Ortiz de Lejarazu R, et al. Influenza surveillance
#' in Europe: establishing epidemic thresholds by the moving epidemic method. Influenza Other Respir
#' Viruses. 2013 Jul;7(4):546-58. DOI:10.1111/j.1750-2659.2012.00422.x.
#'
#' Vega T, Lozano JE, Meerhoff T, Snacken R, Beaute J, Jorgensen P, et al. Influenza surveillance in
#' Europe: comparing intensity levels calculated using the moving epidemic method. Influenza Other
#' Respir Viruses. 2015 Sep;9(5):234-46. DOI:10.1111/irv.12330.
#'
#' Lozano JE. lozalojo/mem: Second release of the MEM R library. Zenodo [Internet]. [cited 2017 Feb 1];
#' Available from: \url{https://zenodo.org/record/165983}. DOI:10.5281/zenodo.165983
#'
#' @keywords influenza
#'
#' @export
roc.analysis <- function(i.data,
                         i.param.values = seq(1.0, 5.0, 0.1),
                         i.min.seasons = 6,
                         i.graph = FALSE,
                         i.graph.file = FALSE,
                         i.graph.file.name = "",
                         i.graph.title = "",
                         i.graph.subtitle = "",
                         i.output = ".",
                         i.mem.info = TRUE,
                         i.min.sensitivity = NA,
                         i.min.specificity = NA,
                         ...) {
  specificity <- sensitivity <- NULL
  if (is.null(dim(i.data))) {
    roc.analysis.output <- NULL
    cat("Incorrect number of dimensions, input must be a data.frame.\n")
  } else {
    if (is.matrix(i.data)) i.data <- as.data.frame(i.data)
    datos <- i.data[apply(i.data, 2, function(x) sum(x, na.rm = TRUE) > 0)]
    anios <- NCOL(datos)
    if (!(anios > 2)) {
      roc.analysis.output <- NULL
      cat("Incorrect number of dimensions, at least three seasons of data required.\n")
    } else if (anios < i.min.seasons) {
      roc.analysis.output <- NULL
      cat("Not enough valid columns, minimum: ", i.min.seasons, "; valid: ", anios, ".\n", sep = "")
    } else {
      n.values <- length(i.param.values)
      resultados <- data.frame()

      for (i in 1:n.values) {
        cat("[", format(round(100 * (i - 1) / n.values, 1), digits = 3, nsmall = 1), "%][Parameter: ", format(round(
          i.param.values[i],
          1
        ), digits = 3, nsmall = 1), "] Analysis started (", i, " out of ", n.values, ")\n", sep = "")
        good.i <- memgoodness(i.data = datos, i.method = 2, i.param = i.param.values[i], i.min.seasons = i.min.seasons, i.graph = FALSE, ...)

        resultados.i <- data.frame(value = i.param.values[i], t(good.i$results))
        resultados <- rbind(resultados, resultados.i)
      }

      names(resultados) <- tolower(names(resultados))

      if (any(is.na(i.min.sensitivity))) i.min.sensitivity <- 0
      if (any(is.null(i.min.sensitivity))) i.min.sensitivity <- 0
      if (any(is.na(i.min.specificity))) i.min.specificity <- 0
      if (any(is.null(i.min.specificity))) i.min.specificity <- 0
      resultados <- filter(resultados, specificity >= i.min.specificity)
      resultados <- filter(resultados, sensitivity >= i.min.sensitivity)

      # Ranking 1: Rank(sensitivity) + Rank(specificity)
      if (!any(!is.na(resultados$sensitivity)) || !any(!is.na(resultados$specificity))) {
        rankings.1 <- NA
        optimo.1 <- NA
      } else {
        rankings.1 <- rank(-resultados$sensitivity, na.last = TRUE) + rank(-resultados$specificity, na.last = TRUE)
        optimo.1 <- i.param.values[which.min(rankings.1)]
      }
      # Ranking 2: Rank(sensitivity x Rank specificity)
      if (!any(!is.na(resultados$sensitivity)) || !any(!is.na(resultados$specificity))) {
        rankings.2 <- NA
        optimo.2 <- NA
      } else {
        rankings.2 <- rank(-resultados$sensitivity * resultados$specificity, na.last = TRUE)
        optimo.2 <- i.param.values[which.min(rankings.2)]
      }
      # Ranking 3: Rank(positive.likehood.ratio)
      if (!any(!is.na(resultados$positive.likehood.ratio))) {
        rankings.3 <- NA
        optimo.3 <- NA
      } else {
        rankings.3 <- rank(-resultados$positive.likehood.ratio, na.last = TRUE)
        optimo.3 <- i.param.values[which.min(rankings.3)]
      }
      # Ranking 4: Rank(negative.likehood.ratio)
      if (!any(!is.na(resultados$negative.likehood.ratio))) {
        rankings.4 <- NA
        optimo.4 <- NA
      } else {
        rankings.4 <- rank(-resultados$negative.likehood.ratio, na.last = TRUE)
        optimo.4 <- i.param.values[which.min(rankings.4)]
      }
      # Ranking 5: Rank(sensitivity-specificity) + Rank(sensitivity+specificity) + Rank(sensitivity^2+specificity^2)
      if (!any(!is.na(resultados$sensitivity)) || !any(!is.na(resultados$specificity))) {
        rankings.5 <- NA
        optimo.5 <- NA
      } else {
        qf <- abs(resultados$sensitivity - resultados$specificity)
        qe <- 2 - resultados$sensitivity - resultados$specificity
        qs <- (1 - resultados$sensitivity)^2 + (1 - resultados$specificity)^2
        rankings.5 <- rank(qf) + rank(qe) + rank(qs)
        optimo.5 <- i.param.values[which.min(rankings.5)]
      }
      # Ranking 6: Rank(percent.agreement)
      if (!any(!is.na(resultados$percent.agreement))) {
        rankings.6 <- NA
        optimo.6 <- NA
      } else {
        rankings.6 <- rank(-resultados$percent.agreement, na.last = TRUE)
        optimo.6 <- i.param.values[which.min(rankings.6)]
      }
      # Ranking 7: Rank(matthews.correlation.coefficient)
      if (!any(!is.na(resultados$matthews.correlation.coefficient))) {
        rankings.7 <- NA
        optimo.7 <- NA
      } else {
        rankings.7 <- rank(-resultados$matthews.correlation.coefficient, na.last = TRUE)
        optimo.7 <- i.param.values[which.min(rankings.7)]
      }
      # Ranking 8: Rank(Youden's J statistic=Youden's index)
      if (!any(!is.na(resultados$youdens.index))) {
        rankings.8 <- NA
        optimo.8 <- NA
      } else {
        rankings.8 <- rank(-resultados$youdens.index, na.last = TRUE)
        optimo.8 <- i.param.values[which.min(rankings.8)]
      }

      optimum <- data.frame(
        pos.likehood = optimo.3, neg.likehood = optimo.4, aditive = optimo.1, multiplicative = optimo.2,
        mixed = optimo.5, percent = optimo.6, matthews = optimo.7, youden = optimo.8
      )

      rankings <- data.frame(
        pos.likehood = rankings.3, neg.likehood = rankings.4, aditive = rankings.1, multiplicative = rankings.2,
        mixed = rankings.5, percent = rankings.6, matthews = rankings.7, youden = rankings.8
      )

      roc.analysis.output <- list(optimum = optimum, rankings = rankings, roc.data = resultados, data = datos, param.data = i.data, param.param.values = i.param.values)
      roc.analysis.output$call <- match.call()

      if (i.graph) {
        colores <- c("#EBEAEA", "#5B9BD5", "#ED7D31")

        if (i.graph.file.name == "") graph.name <- "roc analysis" else graph.name <- i.graph.file.name
        if (i.graph.file) {
          tiff(
            filename = paste(i.output, "/", graph.name, ".tiff", sep = ""), width = 8, height = 6, units = "in", pointsize = "12",
            compression = "lzw", bg = "white", res = 300, antialias = "none"
          )
        }

        opar <- par(mar = c(5, 3, 3, 3) + 0.1, mgp = c(3, 0.5, 0), xpd = TRUE, mfrow = c(2, 2))

        if (any(!is.na(resultados$sensitivity)) && any(!is.na(resultados$specificity))) {
          d.x <- resultados$value
          d.y <- cbind(resultados$sensitivity, resultados$specificity)
          etiquetas <- c("Sensitivity", "Specificity")
          otick <- optimal.tickmarks(0, 1, 10)
          range.y <- c(otick$range[1], otick$range[2] + otick$by / 2)
          matplot(d.x, d.y, type = "l", lty = rep(1, 2), lwd = rep(1, 2), col = colores[c(1, 1)], xlab = "", ylab = "", axes = FALSE, ylim = range.y, main = i.graph.title)
          points(d.x, d.y[, 1], pch = 19, type = "p", col = colores[2], cex = 0.5)
          points(d.x, d.y[, 2], pch = 19, type = "p", col = colores[3], cex = 0.5)
          axis(1, at = d.x, labels = d.x, cex.axis = 0.7, col.axis = "#404040", col = "#C0C0C0")
          axis(2, at = otick$tickmarks, lwd = 1, cex.axis = 0.6, col.axis = "#404040", col = "#C0C0C0")
          mtext(1, text = "Parameter", line = 1.3, cex = 0.8, col = "#000040")
          mtext(2, text = "Value", line = 1.3, cex = 0.8, col = "#000040")
          mtext(3, text = i.graph.subtitle, cex = 0.8, col = "#000040")
          if (i.mem.info) mtext(4, text = paste("mem R library - Jose E. Lozano - https://github.com/lozalojo/mem", sep = ""), line = 0.75, cex = 0.6, col = "#404040")
          legend(
            x = "topright", y = NULL,
            inset = c(0, -0.05),
            xjust = 0,
            legend = etiquetas,
            bty = "n", lty = c(1, 1), lwd = c(1, 1),
            col = colores[c(1, 1)],
            pch = c(21, 21), pt.bg = colores[c(2, 3)],
            cex = 1,
            x.intersp = 0.5, y.intersp = 0.7,
            text.col = "#000000", ncol = 1
          )
        }

        if (any(!is.na(resultados$positive.predictive.value)) && any(!is.na(resultados$negative.predictive.value))) {
          d.x <- resultados$value
          d.y <- cbind(resultados$positive.predictive.value, resultados$negative.predictive.value)
          etiquetas <- c("Positive predictive value", "Negative predictive value")
          otick <- optimal.tickmarks(0, 1, 10)
          range.y <- c(otick$range[1], otick$range[2] + otick$by / 2)
          matplot(d.x, d.y, type = "l", lty = rep(1, 2), lwd = rep(1, 2), col = colores[c(1, 1)], xlab = "", ylab = "", axes = FALSE, ylim = range.y, main = i.graph.title)
          points(d.x, d.y[, 1], pch = 19, type = "p", col = colores[2], cex = 0.5)
          points(d.x, d.y[, 2], pch = 19, type = "p", col = colores[3], cex = 0.5)
          axis(1, at = d.x, labels = d.x, cex.axis = 0.7, col.axis = "#404040", col = "#C0C0C0")
          axis(2, at = otick$tickmarks, lwd = 1, cex.axis = 0.6, col.axis = "#404040", col = "#C0C0C0")
          mtext(1, text = "Parameter", line = 1.3, cex = 0.8, col = "#000040")
          mtext(2, text = "Value", line = 1.3, cex = 0.8, col = "#000040")
          mtext(3, text = i.graph.subtitle, cex = 0.8, col = "#000040")
          if (i.mem.info) mtext(4, text = paste("mem R library - Jose E. Lozano - https://github.com/lozalojo/mem", sep = ""), line = 0.75, cex = 0.6, col = "#404040")
          legend(
            x = "topright", y = NULL,
            inset = c(0, -0.05),
            xjust = 0,
            legend = etiquetas,
            bty = "n", lty = c(1, 1), lwd = c(1, 1),
            col = colores[c(1, 1)],
            pch = c(21, 21), pt.bg = colores[c(2, 3)],
            cex = 1,
            x.intersp = 0.5, y.intersp = 0.7,
            text.col = "#000000", ncol = 1
          )
        }
        if (any(!is.na(resultados$percent.agreement)) && any(!is.na(resultados$matthews.correlation.coefficient))) {
          d.x <- resultados$value
          d.y <- cbind(resultados$percent.agreement, resultados$matthews.correlation.coefficient)
          etiquetas <- c("Percent agreement", "Matthews correlation coefficient")
          otick <- optimal.tickmarks(0, 1, 10)
          range.y <- c(otick$range[1], otick$range[2] + otick$by / 2)
          matplot(d.x, d.y, type = "l", lty = rep(1, 2), lwd = rep(1, 2), col = colores[c(1, 1)], xlab = "", ylab = "", axes = FALSE, ylim = range.y, main = i.graph.title)
          points(d.x, d.y[, 1], pch = 19, type = "p", col = colores[2], cex = 0.5)
          points(d.x, d.y[, 2], pch = 19, type = "p", col = colores[3], cex = 0.5)
          axis(1, at = d.x, labels = d.x, cex.axis = 0.7, col.axis = "#404040", col = "#C0C0C0")
          axis(2, at = otick$tickmarks, lwd = 1, cex.axis = 0.6, col.axis = "#404040", col = "#C0C0C0")
          mtext(1, text = "Parameter", line = 1.3, cex = 0.8, col = "#000040")
          mtext(2, text = "Value", line = 1.3, cex = 0.8, col = "#000040")
          mtext(3, text = i.graph.subtitle, cex = 0.8, col = "#000040")
          if (i.mem.info) mtext(4, text = paste("mem R library - Jose E. Lozano - https://github.com/lozalojo/mem", sep = ""), line = 0.75, cex = 0.6, col = "#404040")
          legend(
            x = "topright", y = NULL,
            inset = c(0, -0.05),
            xjust = 0,
            legend = etiquetas,
            bty = "n",
            lty = c(1, 1), lwd = c(1, 1),
            col = colores[c(1, 1)],
            pch = c(21, 21),
            pt.bg = colores[c(2, 3)],
            cex = 1,
            x.intersp = 0.5, y.intersp = 0.7,
            text.col = "#000000", ncol = 1
          )
        }

        if (any(!is.na(resultados$specificity)) && any(!is.na(resultados$sensitivity))) {
          d.x <- 1 - resultados$specificity
          d.y <- resultados$sensitivity[order(d.x)]
          d.x <- d.x[order(d.x)]
          otick <- optimal.tickmarks(0, 1, 10)
          range.x <- c(otick$range[1], otick$range[2] + otick$by / 2)
          range.y <- c(otick$range[1], otick$range[2] + otick$by / 2)
          matplot(d.x, d.y, type = "l", lty = rep(1, 2), lwd = rep(1, 2), col = colores[c(1, 1)], xlab = "", ylab = "", axes = FALSE, xlim = range.x, ylim = range.y, main = i.graph.title)
          points(d.x, d.y, pch = 19, type = "p", col = colores[2], cex = 0.5)
          axis(1, at = otick$tickmarks, cex.axis = 0.7, col.axis = "#404040", col = "#C0C0C0")
          axis(2, at = otick$tickmarks, lwd = 1, cex.axis = 0.6, col.axis = "#404040", col = "#C0C0C0")
          mtext(1, text = "1 - specificity", line = 1.3, cex = 0.8, col = "#000040")
          mtext(2, text = "Sensitivity", line = 1.3, cex = 0.8, col = "#000040")
          mtext(3, text = i.graph.subtitle, cex = 0.8, col = "#000040")
          if (i.mem.info) mtext(4, text = paste("mem R library - Jose E. Lozano - https://github.com/lozalojo/mem", sep = ""), line = 0.75, cex = 0.6, col = "#404040")
        }
        par(opar)
        if (i.graph.file) dev.off()
      }
    }
  }
  return(roc.analysis.output)
}
