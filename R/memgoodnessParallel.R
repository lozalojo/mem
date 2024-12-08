#' max function, removing Inf and -Inf
#'
#' @keywords internal
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
memgoodnessParallel <- function(i.data,
                        i.seasons = 10,
                        i.type.threshold = 5,
                        i.level.threshold = 0.95,
                        i.tails.threshold = 1,
                        i.type.intensity = 6,
                        i.level.intensity = c(0.40, 0.90, 0.975),
                        i.tails.intensity = 1,
                        i.type.curve = 2,
                        i.level.curve = 0.95,
                        i.type.other = 2,
                        i.level.other = 0.95,
                        i.method = 2,
                        i.param = 2.8,
                        i.n.max = -1,
                        i.type.boot = "norm",
                        i.iter.boot = 10000,
                        i.calculation.method = "default",
                        i.goodness.method = "cross",
                        i.goodness.threshold = NA,
                        i.goodness.intensity = NA,
                        i.detection.values = seq(1.0, 5.0, 0.1),
                        i.weeks.above = 1,
                        i.output = ".",
                        i.graph = FALSE,
                        i.prefix = "",
                        i.min.seasons = 6,
                        i.labels.axis = c("Week", "Weekly rate"),
                        i.labels.periods = c("Pre", "Epidemic", "Post"),
                        i.labels.intensities = c("Epidemic thr", "Medium thr", "High thr", "Very high thr"),
                        i.labels.details = c("algorithm", "threshold", "Method used", "weeks above/below the threshold", "week(s) above the threshold", "Sensitivity", "Specificity")) {
  if (is.null(dim(i.data))) {
    memgoodness.output <- NULL
    cat("Incorrect number of dimensions, input must be a data.frame.\n")
  } else {
    if (is.matrix(i.data)) i.data <- as.data.frame(i.data)
    datos <- i.data[apply(i.data, 2, function(x) sum(x, na.rm = TRUE) > 0)]
    anios <- NCOL(datos)
    semanas <- dim(datos)[1]
    if (!(anios > 2)) {
      memgoodness.output <- NULL
      cat("Incorrect number of dimensions, at least thress seasons of data required.\n")
    } else if (anios < i.min.seasons) {
      memgoodness.output <- NULL
      cat("Not enough valid columns, minimum: ", i.min.seasons, "; valid: ", anios, ".\n", sep = "")
    } else {
      if (is.null(i.seasons)) i.seasons <- NA
      validacion <- array(NA, dim = c(15, anios))
      colnames(validacion) <- names(datos)
      maximos <- numeric()
      maximos.seasons <- character()
      if (is.na(i.seasons)) i.seasons <- anios
      if (is.null(i.seasons)) i.seasons <- anios
      i.goodness.threshold <- i.goodness.threshold[!is.na(i.goodness.threshold)]
      i.goodness.intensity <- i.goodness.intensity[!is.na(i.goodness.intensity)]
      if (length(i.goodness.threshold) == 2) {
        i.goodness.threshold.pre <- i.goodness.threshold[1]
        i.goodness.threshold.pos <- i.goodness.threshold[2]
      } else if (length(i.goodness.threshold) == 1) {
        i.goodness.threshold.pre <- i.goodness.threshold
        i.goodness.threshold.pos <- i.goodness.threshold
      } else {
        i.goodness.threshold.pre <- NA
        i.goodness.threshold.pos <- NA
        if (i.goodness.method == "threshold") i.goodness.method <- "cross"
      }
      if (length(i.goodness.intensity) != 3) i.goodness.intensity <- i.goodness.threshold * c(3, 6, 10)
	  

		
      if (i.goodness.method == "sequential") {
        # Metodo 1: secuencial
        for (i in i.min.seasons:anios) {
          indices.modelo <- max(1, i - i.seasons):(i - 1)
          indices.actual <- i
          datos.actual <- datos[indices.actual]
          datos.modelo <- memmodel(datos[indices.modelo],
            i.seasons = i.seasons,
            i.type.threshold = i.type.threshold,
            i.level.threshold = i.level.threshold,
            i.tails.threshold = i.tails.threshold,
            i.type.intensity = i.type.intensity,
            i.level.intensity = i.level.intensity,
            i.tails.intensity = i.tails.intensity,
            i.type.curve = i.type.curve,
            i.level.curve = i.level.curve,
            i.type.other = i.type.other,
            i.level.other = i.level.other,
            i.method = i.method,
            i.param = i.param,
            i.n.max = i.n.max,
            i.type.boot = i.type.boot,
            i.iter.boot = i.iter.boot
          )
          validacion.i <- calcular.indicadores(
            i.current = datos.actual,
            i.umbral.pre = datos.modelo$pre.post.intervals[1, 3],
            i.umbral.pos = datos.modelo$pre.post.intervals[2, 3],
            i.intensidades = datos.modelo$epi.intervals[, 4],
            i.duracion.intensidad = datos.modelo$mean.length,
            i.metodo.calculo = i.calculation.method,
            i.semanas.por.encima = i.weeks.above,
            i.valores.parametro.deteccion = i.detection.values,
            i.output = i.output,
            i.graph = i.graph,
            i.graph.name = paste(i.prefix, " Goodness ", i, sep = ""),
            i.labels.axis = i.labels.axis,
            i.labels.periods = i.labels.periods,
            i.labels.intensities = i.labels.intensities,
            i.labels.details = i.labels.details
          )
          validacion[, i] <- validacion.i$indicadores.t
          rownames(validacion) <- rownames(validacion.i$indicadores.t)
          peak.i <- maxFixNA(datos.actual)
          peak.week.i <- as.numeric(row.names(datos.actual)[min((1:semanas)[peak.i == datos.actual])])
          umbrales.i <- memintensity(datos.modelo)$intensity.thresholds
          if (is.na(umbrales.i[1])) umbrales.i[1] <- 0
          if (umbrales.i[1] > umbrales.i[2]) umbrales.i[2] <- umbrales.i[1] * 1.0000001
          level.i <- as.numeric(cut(peak.i, c(-Inf, umbrales.i, Inf)))
          maximos <- cbind(maximos, c(peak.i, peak.week.i, umbrales.i, level.i))
          maximos.seasons <- c(maximos.seasons, names(datos)[indices.actual])
          rm("indices.modelo", "indices.actual", "datos.actual", "datos.modelo", "validacion.i", "peak.i", "peak.week.i", "umbrales.i", "level.i")
        }
      } else if (i.goodness.method == "threshold") {
        # Metodo 2: fixed threshold and intensities
        for (i in 1:anios) {
          indices.2 <- (1:anios) - i
          indices.1 <- abs(indices.2)
          indices.modelo <- order(indices.1, indices.2)[2:(i.seasons + 1)]
          indices.modelo <- sort(indices.modelo[!is.na(indices.modelo)])
          indices.actual <- i
          datos.actual <- datos[indices.actual]
          validacion.i <- calcular.indicadores(
            i.current = datos.actual,
            i.umbral.pre = i.goodness.threshold.pre,
            i.umbral.pos = i.goodness.threshold.pos,
            i.intensidades = i.goodness.intensity,
            i.duracion.intensidad = 10,
            i.metodo.calculo = i.calculation.method,
            i.semanas.por.encima = i.weeks.above,
            i.valores.parametro.deteccion = i.detection.values,
            i.output = i.output,
            i.graph = i.graph,
            i.graph.name = paste(i.prefix, " Goodness ", i, sep = ""),
            i.labels.axis = i.labels.axis,
            i.labels.periods = i.labels.periods,
            i.labels.intensities = i.labels.intensities,
            i.labels.details = i.labels.details
          )
          validacion[, i] <- validacion.i$indicadores.t
          rownames(validacion) <- rownames(validacion.i$indicadores.t)
          peak.i <- maxFixNA(datos.actual)
          peak.week.i <- as.numeric(row.names(datos.actual)[min((1:semanas)[peak.i == datos.actual], na.rm = TRUE)])
          umbrales.i <- c(i.goodness.threshold.pre, i.goodness.intensity)
          if (is.na(umbrales.i[1])) umbrales.i[1] <- 0
          if (umbrales.i[1] > umbrales.i[2]) umbrales.i[2] <- umbrales.i[1] * 1.0000001
          level.i <- as.numeric(cut(peak.i, c(-Inf, umbrales.i, Inf)))
          maximos <- cbind(maximos, c(peak.i, peak.week.i, umbrales.i, level.i))
          maximos.seasons <- c(maximos.seasons, names(datos)[indices.actual])
          rm("indices.modelo", "indices.actual", "datos.actual", "validacion.i", "peak.i", "peak.week.i", "umbrales.i", "level.i")
        }
      } else {
        # Metodo 3: cruzada
        
        n_cores <- detectCores()
        cluster <- makeCluster(n_cores - 1)
        registerDoParallel(cluster)
        
        combine_custom_j <- function(LL1, LL2) {
          
          vi <- cbind(LL1$vi, LL2$vi)
          maximosi <- cbind(LL1$maximosi, LL2$maximosi)
          maximossi <- c(LL1$maximossi, LL2$maximossi)
          return(list(vi = vi, maximosi = maximosi, maximossi = maximossi))
        }
        
        temp1 <- foreach(i = 1:anios, .combine = 'combine_custom_j') %dopar% {
          indices.2 <- (1:anios) - i
          indices.1 <- abs(indices.2)
          indices.modelo <- order(indices.1, indices.2)[2:(i.seasons + 
                                                             1)]
          indices.modelo <- sort(indices.modelo[!is.na(indices.modelo)])
          indices.actual <- i
          datos.actual <- datos[indices.actual]
          datos.modelo <- mem::memmodel(datos[indices.modelo], 
                                        i.seasons = i.seasons, i.type.threshold = i.type.threshold, 
                                        i.level.threshold = i.level.threshold, i.tails.threshold = i.tails.threshold, 
                                        i.type.intensity = i.type.intensity, i.level.intensity = i.level.intensity, 
                                        i.tails.intensity = i.tails.intensity, i.type.curve = i.type.curve, 
                                        i.level.curve = i.level.curve, i.type.other = i.type.other, 
                                        i.level.other = i.level.other, i.method = i.method, 
                                        i.param = i.param, i.n.max = i.n.max, i.type.boot = i.type.boot, 
                                        i.iter.boot = i.iter.boot)
          validacion.i <- calcular.indicadores(i.current = datos.actual, 
                                                     i.umbral.pre = datos.modelo$pre.post.intervals[1, 
                                                                                                    3], i.umbral.pos = datos.modelo$pre.post.intervals[2, 
                                                                                                                                                       3], i.intensidades = datos.modelo$epi.intervals[, 
                                                                                                                                                                                                       4], i.duracion.intensidad = datos.modelo$mean.length, 
                                                     i.metodo.calculo = i.calculation.method, 
                                                     i.semanas.por.encima = i.weeks.above, i.valores.parametro.deteccion = i.detection.values, 
                                                     i.output = i.output, i.graph = i.graph, i.graph.name = paste(i.prefix, 
                                                                                                                  " Goodness ", i, sep = ""), i.labels.axis = i.labels.axis, 
                                                     i.labels.periods = i.labels.periods, i.labels.intensities = i.labels.intensities, 
                                                     i.labels.details = i.labels.details)
          vi <- validacion.i$indicadores.t
          rownames(vi) <- rownames(validacion.i$indicadores.t)
          peak.i <- maxFixNA(datos.actual)
          peak.week.i <- as.numeric(row.names(datos.actual)[min((1:semanas)[peak.i == 
                                                                              datos.actual], na.rm = TRUE)])
          umbrales.i <- mem::memintensity(datos.modelo)$intensity.thresholds
          if (is.na(umbrales.i[1])) 
            umbrales.i[1] <- 0
          if (umbrales.i[1] > umbrales.i[2]) 
            umbrales.i[2] <- umbrales.i[1] * 1.0000001
          level.i <- as.numeric(cut(peak.i, c(-Inf, umbrales.i, 
                                              Inf)))
          maximosi <- c(peak.i, peak.week.i, 
                        umbrales.i, level.i)
          
          maximossi <- names(datos)[indices.actual]
          list(vi=vi, maximosi=maximosi, maximossi=maximossi)
        }
        stopCluster(cl = cluster)
        maximos.seasons=temp1$maximossi
        maximos=temp1$maximosi
        validacion=temp1$vi
        names(validacion) <- maximos.seasons
      }
	  

      resultado <- apply(validacion, 1, sum, na.rm = TRUE)
      # sensibilidad
      resultado[7] <- resultado[3] / (resultado[3] + resultado[6])
      # especificidad
      resultado[8] <- resultado[5] / (resultado[5] + resultado[4])
      # vpp
      resultado[9] <- resultado[3] / (resultado[3] + resultado[4])
      # vpn
      resultado[10] <- resultado[5] / (resultado[5] + resultado[6])
      # positive likehood ratio
      resultado[11] <- resultado[7] / (1 - resultado[8])
      # negative likehood ratio
      resultado[12] <- (1 - resultado[7]) / resultado[8]
      # percentage agreement/accuracy
      resultado[13] <- (resultado[3] + resultado[5]) / (resultado[3] + resultado[4] + resultado[5] + resultado[6])
      # Matthews correlation coefficient
      resultado[14] <- (resultado[3] * resultado[5] - resultado[4] * resultado[6]) / sqrt((resultado[3] + resultado[4]) * (resultado[3] + resultado[6]) * (resultado[5] + resultado[4]) * (resultado[5] + resultado[6]))
      # Youden's index
      resultado[15] <- resultado[7] + resultado[8] - 1

      resultado[is.nan(resultado)] <- NA

      temp1 <- data.frame(Description = c("Baseline", "Low", "Medium", "High", "Very high"), Level = 1:5, stringsAsFactors = FALSE)
      maximos <- data.frame(t(maximos), stringsAsFactors = FALSE)
      names(maximos) <- c("Peak", "Peak week", "Epidemic threshold", "Medium threshold", "High threshold", "Very high threshold", "Level")
      maximos$id <- seq_len(NROW(maximos))
      maximos <- merge(maximos, temp1, by = "Level", all.x = TRUE)
      maximos <- maximos[order(maximos$id), ]
      rownames(maximos) <- maximos.seasons
      maximos$id <- NULL
      maximos <- maximos[c(2:7, 1, 8)]

      temp2 <- data.frame(table(as.numeric(maximos[, 7]), exclude = c(NA, NaN)), stringsAsFactors = FALSE)
      names(temp2) <- c("Level", "Count")
      temp3 <- merge(temp1, temp2, all.x = TRUE)
      temp3$Count[is.na(temp3$Count)] <- 0
      temp3$Percentage <- temp3$Count / sum(temp3$Count)
      temp4 <- data.frame(
        Level = c(0, -1),
        Description = c("No data seasons", "Total seasons"),
        Count = c(NROW(maximos) - sum(temp3$Count), NROW(maximos)),
        Percentage = c((NROW(maximos) - sum(temp3$Count)) / NROW(maximos), 1), stringsAsFactors = FALSE
      )
      maximos.resultados <- rbind(temp3, temp4)

      memgoodness.output <- list(
        validity.data = validacion,
        results = resultado,
        peaks = maximos.resultados,
        peaks.data = maximos,
        data = datos,
        param.data = i.data,
        param.seasons = i.seasons,
        param.type.threshold = i.type.threshold,
        param.level.threshold = i.level.threshold,
        param.tails.threshold = i.tails.threshold,
        param.type.intensity = i.type.intensity,
        param.level.intensity = i.level.intensity,
        param.tails.intensity = i.tails.intensity,
        param.type.curve = i.type.curve,
        param.level.curve = i.level.curve,
        param.type.other = i.type.other,
        param.level.other = i.level.other,
        param.method = i.method,
        param.param = i.param,
        param.n.max = i.n.max,
        param.type.boot = i.type.boot,
        param.iter.boot = i.iter.boot,
        param.calculation.method = i.calculation.method,
        param.goodness.method = i.goodness.method,
        param.goodness.threshold = i.goodness.threshold,
        param.goodness.intensity = i.goodness.intensity,
        param.detection.values = i.detection.values,
        param.weeks.above = i.weeks.above,
        param.output = i.output,
        param.graph = i.graph,
        param.prefix = i.prefix,
        param.min.seasons = i.min.seasons
      )
      memgoodness.output$call <- match.call()
    }
  }
  return(memgoodness.output)
}
