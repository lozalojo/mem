#' adjust a mixed model of two normal distributions
#'
#' @keywords internal
#'
#' @importFrom mixtools normalmixEM
#' @importFrom stats dnorm
transformseries.twowaves <- function(i.data, i.scale = 10000) {
  seasons <- names(i.data)
  n.seasons <- dim(i.data)[2]
  weeks <- rownames(i.data)
  n.weeks <- dim(i.data)[1]
  resultados.1 <- i.data
  resultados.2 <- i.data
  inicios <- data.frame(dummy = NA)
  detalles <- list()
  for (i in 1:n.seasons) {
    cat(names(i.data)[i],"\n")
    resultados.i <- data.frame(rates = i.data[, i])
    rownames(resultados.i) <- weeks
    resultados.i$rates.no.miss <- fill.missing(resultados.i$rates)
    total.rates <- sum(resultados.i$rates.no.miss, na.rm = T)
    x1 <- 1:n.weeks
    y1 <- round(resultados.i$rates.no.miss * i.scale/total.rates,
                0)
    x2 <- x1[!is.na(y1)]
    y2 <- y1[!is.na(y1)]
    data.rep <- rep(x2, times = y2)
    mixmdl.normal <- normalmixEM(data.rep, k = 2, maxit=10000, maxrestarts=10000)
    if (mixmdl.normal$mu[1] < mixmdl.normal$mu[2]) {
      resultados.i$normal1 <- mixmdl.normal$lambda[1] *
        dnorm(x1, mixmdl.normal$mu[1], mixmdl.normal$sigma[1]) *
        total.rates
      resultados.i$normal2 <- mixmdl.normal$lambda[2] *
        dnorm(x1, mixmdl.normal$mu[2], mixmdl.normal$sigma[2]) *
        total.rates
    } else {
      resultados.i$normal2 <- mixmdl.normal$lambda[1] *
        dnorm(x1, mixmdl.normal$mu[1], mixmdl.normal$sigma[1]) *
        total.rates
      resultados.i$normal1 <- mixmdl.normal$lambda[2] *
        dnorm(x1, mixmdl.normal$mu[2], mixmdl.normal$sigma[2]) *
        total.rates
    }
    resultados.i$normal<-resultados.i$normal1+resultados.i$normal2
    resultados.i$coeficiente<-resultados.i$rates.no.miss/resultados.i$normal
    if (any(diff(resultados.i$normal1 < resultados.i$normal2) ==
            1))
      inicio.normal <- min(x1[c(0, diff(resultados.i$normal1 <
                                          resultados.i$normal2)) == 1], na.rm = T) else inicio.normal <- 1
    resultados.i$season.sub.normal <- "1"
    resultados.i$season.sub.normal[inicio.normal:n.weeks] <- "2"
    resultados.i$part1 <- ifelse(resultados.i$season.sub.normal ==
                                   "1", resultados.i$rates.no.miss, NA)
    resultados.i$part2 <- ifelse(resultados.i$season.sub.normal ==
                                   "2", resultados.i$rates.no.miss, NA)

    detalles$nombre <- mixmdl.normal
    names(detalles)[names(detalles) == "nombre"] <- seasons[i]

    inicios.i <- data.frame(inicio = as.numeric(inicio.normal))
    names(inicios.i)[names(inicios.i) == "inicio"] <- seasons[i]
    resultados.i.1 <- resultados.i[names(resultados.i) %in%
                                     c("part1", "part2")]
    resultados.i.2 <- resultados.i[names(resultados.i) %in%
                                     c("normal1", "normal2")]

    names(resultados.i.1) <- c(paste(seasons[i], "(1)", sep = ""),
                               paste(seasons[i], "(2)", sep = ""))
    names(resultados.i.2) <- c(paste(seasons[i], "(1)", sep = ""),
                               paste(seasons[i], "(2)", sep = ""))
    resultados.1 <- cbind(resultados.1, resultados.i.1)
    resultados.2 <- cbind(resultados.2, resultados.i.2)

    inicios <- cbind(inicios, inicios.i)
    rm("resultados.i.1","resultados.i.2","inicios.i")
  }
  resultados.1 <- resultados.1[!(names(resultados.1) %in% seasons)]
  resultados.2 <- resultados.2[!(names(resultados.2) %in% seasons)]
  inicios <- inicios[!(names(inicios) %in% "dummy")]
  return(list(data.observed = resultados.1, data.expected = resultados.2, breaks = inicios, details = detalles))
}
