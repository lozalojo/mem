#' @title Creates the surveillance graph of the current season
#'
#' @description
#' Function \code{memsurveillance} creates a surveillance graph for the current season.
#'
#' @name memsurveillance
#'
#' @param i.current Current season weekly rates.
#' @param i.epidemic.thresholds Pre and post epidemic threholds.
#' @param i.intensity.thresholds Intensity thresholds.
#' @param i.mean.length Mean length of epidemic.
#' @param i.force.length If you want to force the epidemic to be exactly as the mean length.
#' @param i.output Directory where graph is saved.
#' @param i.graph.title Title of the graph.
#' @param i.graph.subtitle Subtitle of the graph.
#' @param i.graph.file Graph to a file.
#' @param i.graph.file.name Name of the graph.
#' @param i.week.report Week to use in the report.
#' @param i.equal If post epidemic and preepidemic thresholds must be equal (force post epidemic to be equal to the pre epidemic threshold).
#' @param i.pos.epidemic Print post epidemic threhsold.
#' @param i.no.epidemic Force no start of the epidemic, print only the epidemic threshold.
#' @param i.no.intensity Do not print intensity threholds.
#' @param i.epidemic.start Week to force start of the epidemic.
#' @param i.range.x Range of weeks.
#' @param i.range.x.53 Is there a week 53 this season.
#' @param i.range.y Range of graph.
#' @param i.no.labels Do not use labels.
#' @param i.start.end.marks Do not place start and end marks of the epidemic.
#' @param i.mem.info include information about the package in the graph.
#'
#' @return
#' \code{memsurveillance} writes a tiff graph of the surveillance of this season.
#'
#' @details
#' Input data must be the current season and an object of class \code{mem}. The output
#' graph contains the weekly rates series along with the epidemic and intensity threshols
#' located at the exact situation where the epidemic started. If there is no epidemic yet,
#' only the epidemic threshold is placed.
#'
#' Surveillance consist on:
#' \enumerate{
#' \item Draw weekly values along with the pre-epidemic threshold.
#' \item When the weekly value rises above the threshold a marker of the start of
#' the epidemic is placed and the intensity thresholds are drawn.
#' \item When the weekly value goes down the post-epidemic threshold, the marker of
#' the end of the epidemic is placed and the post-epidemic threshold is added to the graph.
#' }
#' The Surveillance Week allows select the week to use in the surveillance, the values of
#' the surveillance season are shown only up to this week, and the program will ignore
#' values past this week.
#'
#' The Force epidemic start allows to force the placement of the epidemic start marker
#' at a given week instead of using the first week above the epidemic threshold.
#'
#' @examples
#' \donttest{
#' # Castilla y Leon Influenza Rates data
#' data(flucyl)
#' # Data of the last season
#' cur <- flucyl[8]
#' # The model
#' epi <- memmodel(flucyl[1:7])
#' # Epidemic thresholds
#' e.thr <- epi$epidemic.thresholds
#' # Intensity threhsolds
#' i.thr <- epi$intensity.thresholds
#' # Set the working directory to whererever you want to store the graph file
#' setwd(".")
#' # The graph, default values
#' # uncomment to execute
#' # m1 <- memsurveillance(cur, e.thr, i.thr,
#' #    i.graph.file = TRUE,
#' #    i.graph.file.name = "graph 1"
#' # )
#' # No intensity levels
#' # m2 <- memsurveillance(cur, e.thr, i.thr,
#' #    i.graph.file = TRUE,
#' #    i.graph.file.name = "graph 2", i.no.intensity = TRUE
#' # )
#' # No start/end tickmarks
#' # m3 <- memsurveillance(cur, e.thr, i.thr,
#' #    i.graph.file = TRUE,
#' #    i.graph.file.name = "graph 3", i.start.end.marks = FALSE
#' # )
#' # Post-epidemic threshold
#' # m4 <- memsurveillance(cur, e.thr, i.thr,
#' #    i.graph.file = TRUE,
#' #    i.graph.file.name = "graph 4", i.pos.epidemic = TRUE
#' # )
#' # Report for week 2, instead of all data
#' # m5 <- memsurveillance(cur, e.thr, i.thr,
#' #    i.graph.file = TRUE,
#' #    i.graph.file.name = "graph 5", i.week.report = 2
#' # )
#' }
#'
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
#' @importFrom grDevices dev.off rgb tiff
#' @importFrom graphics abline axis legend matplot mtext par points text lines
memsurveillance <- function(i.current,
                            i.epidemic.thresholds = NA,
                            i.intensity.thresholds = NA,
                            i.mean.length = 10,
                            i.force.length = FALSE,
                            i.output = ".",
                            i.graph.title = "",
                            i.graph.subtitle = "",
                            i.graph.file = TRUE,
                            i.graph.file.name = "",
                            i.week.report = NA,
                            i.equal = FALSE,
                            i.pos.epidemic = FALSE,
                            i.no.epidemic = FALSE,
                            i.no.intensity = FALSE,
                            i.epidemic.start = NA,
                            i.range.x = c(40, 20),
                            i.range.x.53 = FALSE,
                            i.range.y = NA,
                            i.no.labels = FALSE,
                            i.start.end.marks = TRUE,
                            i.mem.info = TRUE) {
  if (is.null(dim(i.current))) stop("Incorrect number of dimensions, input must be a data.frame.") else if (!(ncol(i.current) == 1)) stop("Incorrect number of dimensions, only one season required.")

  if (!is.numeric(i.epidemic.thresholds) || length(i.epidemic.thresholds) == 1) i.epidemic.thresholds <- rep(NA, 2)
  if (!is.numeric(i.intensity.thresholds) || length(i.intensity.thresholds) == 1) i.intensity.thresholds <- rep(NA, 3)
  # Esquema de las semanas
  if (!is.numeric(i.range.x) || length(i.range.x) != 2) i.range.x <- c(40, 20)
  if (i.range.x.53) esquema.temporadas.1 <- 53 else esquema.temporadas.1 <- 52
  if (i.range.x[1] == i.range.x[2]) i.range.x[2] <- i.range.x[1] - 1
  if (i.range.x[1] < i.range.x[2]) {
    esquema.temporadas.2 <- max(1, i.range.x[1])
    esquema.temporadas.3 <- min(esquema.temporadas.1, i.range.x[2])
    esquema.temporadas.4 <- c(esquema.temporadas.2:esquema.temporadas.3)
  } else {
    esquema.temporadas.2 <- min(esquema.temporadas.1, i.range.x[1])
    esquema.temporadas.3 <- max(1, i.range.x[2])
    esquema.temporadas.4 <- c(esquema.temporadas.2:esquema.temporadas.1, 1:esquema.temporadas.3)
  }
  semanas <- length(esquema.temporadas.4)
  esquema.semanas <- data.frame(numero.semana = 1:semanas, nombre.semana = esquema.temporadas.4)

  # Acomodamos i.current al esquema
  current.season <- i.current
  names(current.season) <- "rates"
  current.season$nombre.semana <- rownames(i.current)
  rownames(current.season) <- NULL
  current.season <- merge(esquema.semanas, current.season, by = "nombre.semana", all.x = TRUE)
  current.season <- current.season[order(current.season$numero.semana), ]
  rownames(current.season) <- NULL

  # limitamos a la semana del informe (i.week.report)
  if (!is.na(i.week.report) && any(i.week.report == as.numeric(esquema.semanas$nombre.semana))) {
    semana.report <- ((1:semanas)[i.week.report == as.numeric(esquema.semanas$nombre.semana)])[1]
    if (!is.na(semana.report) && semana.report < semanas) current.season$rates[(semana.report + 1):semanas] <- NA
  } else {
    if (all(is.na(current.season$rates))) semana.report <- semanas else semana.report <- max((1:semanas)[!is.na(current.season$rates)], na.rm = TRUE)
  }

  # Preparacion de datos necesarios
  umbral.pre <- as.numeric(i.epidemic.thresholds[1])
  if (i.equal) umbral.pos <- as.numeric(i.epidemic.thresholds[1]) else umbral.pos <- as.numeric(i.epidemic.thresholds[2])
  duracion.media <- i.mean.length

  # Si el inicio forzado de la epidemia es posterior a la semana del informe, quitamos
  if (!is.na(i.epidemic.start)) semana.inicio.forzado <- ((1:semanas)[i.epidemic.start == as.numeric(esquema.semanas$nombre.semana)])[1] else semana.inicio.forzado <- NA
  if (any(current.season$rates > umbral.pre, na.rm = TRUE)) semana.inicio.real <- min((1:semanas)[current.season$rates > umbral.pre], na.rm = TRUE) else semana.inicio.real <- NA
  if (!is.na(semana.inicio.forzado)) {
    if (semana.inicio.forzado > semana.report) semana.inicio.forzado <- NA
  }
  if (!is.na(semana.inicio.forzado) && !is.na(semana.inicio.real)) {
    if (semana.inicio.forzado == semana.inicio.real) semana.inicio.forzado <- NA
  }
  if (!is.na(semana.inicio.forzado)) {
    semana.inicio <- semana.inicio.forzado
  } else {
    semana.inicio <- semana.inicio.real
  }

  week.peak <- which.max(current.season$rates)

  if (!is.na(semana.inicio)) {
    if (i.force.length) {
      semana.fin <- semana.inicio + i.mean.length
      if (semana.fin > semanas) semana.fin <- NA
    } else {
      punto.de.busqueda <- max(semana.inicio, semana.inicio.real, week.peak, na.rm = TRUE)
      semana.fin.1 <- (1:semanas)[current.season$rates < umbral.pos & punto.de.busqueda < (1:semanas)]
      if (any(semana.fin.1, na.rm = TRUE)) semana.fin <- min(semana.fin.1, na.rm = TRUE) else semana.fin <- NA
    }
  } else {
    semana.fin <- NA
  }
  if (i.no.epidemic) {
    semana.inicio <- NA
    semana.fin <- NA
  }
  limites.niveles <- as.vector(i.intensity.thresholds)
  limites.niveles[limites.niveles < 0] <- 0

  # Datos para el grafico
  if (is.na(semana.inicio)) {
    # No iniciada
    umbrales.1 <- rep(umbral.pre, semana.report + 1)
    umbrales.2 <- rep(NA, semanas)
    intensidades.1 <- array(dim = c(semanas, 3))
    intensidades.2 <- array(dim = c(semanas, 3))
  } else {
    if (is.na(semana.fin)) {
      # Iniciada y no finalizada
      umbrales.1 <- rep(umbral.pre, semana.inicio - 1)
      umbrales.2 <- rep(NA, max(duracion.media, semana.report - semana.inicio + 1))
      if (!i.no.intensity) {
        intensidades.1 <- array(dim = c(semana.inicio - 1, 3))
        intensidades.2 <- matrix(rep(limites.niveles, max(duracion.media, semana.report - semana.inicio + 1)), ncol = 3, byrow = TRUE)
      } else {
        intensidades.1 <- array(dim = c(semana.inicio - 1, 3))
        intensidades.2 <- array(dim = c(max(duracion.media, semana.report - semana.inicio + 1), 3))
      }
    } else {
      # Iniciada y finalizada
      umbrales.1 <- rep(umbral.pre, semana.inicio - 1)
      umbrales.2 <- rep(NA, semana.fin - semana.inicio)
      if (!i.no.intensity) {
        intensidades.1 <- array(dim = c(semana.inicio - 1, 3))
        intensidades.2 <- matrix(rep(limites.niveles, semana.fin - semana.inicio), ncol = 3, byrow = TRUE)
      } else {
        intensidades.1 <- array(dim = c(semana.inicio - 1, 3))
        intensidades.2 <- array(dim = c(semana.fin - semana.inicio, 3))
      }
    }
  }
  if (i.pos.epidemic) umbrales.3 <- rep(umbral.pos, semanas) else umbrales.3 <- rep(NA, semanas)
  umbrales <- c(umbrales.1, umbrales.2, umbrales.3)[1:semanas]
  intensidades.3 <- array(dim = c(semanas, 3))
  intensidades <- rbind(intensidades.1, intensidades.2, intensidades.3)[1:semanas, ]
  dgraf <- as.data.frame(cbind(current.season$rates, umbrales, intensidades))
  names(dgraf) <- c("Value", "Epidemic threshold", paste("Intensidad", 1:3))
  if (i.graph.file.name == "") graph.name <- "mem surveillance graph" else graph.name <- i.graph.file.name
  etiquetas <- c("Weekly values", "Epidemic", "Medium", "High", "Very high")
  tipos <- c(1, 2, 2, 2, 2)
  anchos <- c(3, 2, 2, 2, 2)
  colores <- c("#808080", "#8c6bb1", "#88419d", "#810f7c", "#4d004b")

  if (is.numeric(i.range.y)) range.y.bus <- i.range.y else range.y.bus <- c(0, maxFixNA(dgraf))
  otick <- optimal.tickmarks(range.y.bus[1], range.y.bus[2], 10)
  range.y <- c(otick$range[1], otick$range[2] + otick$by / 2)

  if (i.graph.file) {
    tiff(
      filename = paste(i.output, "/", graph.name, ".tiff", sep = ""), width = 8, height = 6, units = "in", pointsize = "12",
      compression = "lzw", bg = "white", res = 300, antialias = "none"
    )
  }

  opar <- par(mar = c(5, 3, 3, 3) + 0.1, mgp = c(3, 0.5, 0), xpd = TRUE)
  # Grafico principal
  matplot(1:semanas,
    dgraf,
    type = "l",
    lty = tipos, lwd = anchos, col = colores,
    xlab = "", ylab = "", axes = FALSE,
    ylim = range.y, main = i.graph.title
  )
  # Puntos de la serie de tasas
  points(1:semanas, dgraf[, 1], pch = 19, type = "p", col = "#000000", cex = 1)
  # Marcas de inicio y fin
  # if (is.na(semana.inicio.forzado) & i.start.end.marks){
  if (i.start.end.marks) {
    if (!is.na(semana.inicio)) points(x = semana.inicio, y = current.season$rates[semana.inicio], pch = 1, bg = "#FFFFFF", col = "#FF0000", lwd = 7)
    if (!is.na(semana.fin) && i.pos.epidemic) {
      if (is.na(current.season$rates[semana.fin])) {
        points(x = semana.fin, y = 0, pch = 13, bg = "#FFFFFF", col = "#40FF40", lwd = 7)
      } else {
        points(x = semana.fin, y = current.season$rates[semana.fin], pch = 1, bg = "#FFFFFF", col = "#40FF40", lwd = 7)
      }
    }
  }
  # Ejes
  axis(1,
    at = seq(1, semanas, 1),
    labels = FALSE,
    cex.axis = 0.7,
    col.axis = "#404040",
    col = "#C0C0C0"
  )
  axis(1,
    at = seq(1, semanas, 2),
    tick = FALSE,
    labels = esquema.semanas$nombre.semana[seq(1, semanas, 2)],
    cex.axis = 0.7,
    col.axis = "#404040",
    col = "#C0C0C0"
  )
  axis(1,
    at = seq(2, semanas, 2),
    tick = FALSE,
    labels = esquema.semanas$nombre.semana[seq(2, semanas, 2)],
    cex.axis = 0.7,
    line = 0.75,
    col.axis = "#404040", col = "#C0C0C0"
  )
  axis(2,
    at = otick$tickmarks,
    lwd = 1,
    cex.axis = 0.6,
    col.axis = "#404040",
    col = "#C0C0C0"
  )
  mtext(1, text = "Week", line = 2.5, cex = 0.8, col = "#000040")
  mtext(2, text = "Weekly value", line = 1.3, cex = 0.8, col = "#000040")
  mtext(3, text = i.graph.subtitle, cex = 0.8, col = "#000040")
  if (i.mem.info) mtext(4, text = paste("mem R library - Jose E. Lozano - https://github.com/lozalojo/mem", sep = ""), line = 0.75, cex = 0.6, col = "#404040")
  # Etiquetas de los 4 umbrales
  if (!i.no.labels) {
    if (is.na(semana.inicio)) {
      # No iniciada
      text.x <- semana.report - semanas / 25
      text.y <- umbral.pre
      text.l <- round(umbral.pre, 2)
      text.p <- 3
      text.s <- 1
      text.c <- colores[2]
    } else {
      if (is.na(semana.fin)) {
        # Iniciada y no finalizada
        lugar.intensidad <- min(semanas, semana.inicio + max(duracion.media, semana.report - semana.inicio + 1))
      } else {
        # Iniciada y finalizada
        lugar.intensidad <- semana.fin - 1
      }
      text.x <- c(semana.inicio, rep(lugar.intensidad, 3), semanas) - semanas / 25
      text.y <- c(umbral.pre, limites.niveles, umbral.pos)
      text.l <- round(c(umbral.pre, limites.niveles, umbral.pos), 2)
      text.p <- rep(3, 5)
      text.s <- rep(1, 5)
      text.c <- colores[c(2:5, 2)]
      quitar.columnas <- numeric()
      if (!i.pos.epidemic || is.na(semana.fin)) quitar.columnas <- c(quitar.columnas, 5)
      if (i.no.intensity) quitar.columnas <- c(quitar.columnas, 2:4)
      if (length(quitar.columnas) > 0) {
        text.x <- text.x[-quitar.columnas]
        text.y <- text.y[-quitar.columnas]
        text.l <- text.l[-quitar.columnas]
        text.p <- text.p[-quitar.columnas]
        text.s <- text.s[-quitar.columnas]
        text.c <- text.c[-quitar.columnas]
      }
    }
    text(text.x, text.y, text.l, pos = text.p, col = text.c, cex = text.s)
  }
  # Etiquetas de la leyenda
  etiquetas.leyenda <- c("End", "Start", etiquetas)
  tipos.leyenda <- c(NA, NA, tipos)
  anchos.leyenda <- c(7, 7, anchos)
  colores.leyenda <- c("#40FF40", "#FF0000", colores)
  puntos.leyenda <- c(1, 1, rep(NA, 5))
  bg.leyenda <- c("#FFFFFF", "#FFFFFF", rep(NA, 5))
  quitar.columnas <- numeric()
  if (!i.start.end.marks || !is.na(semana.inicio.forzado)) quitar.columnas <- c(quitar.columnas, 1:2)
  if (!i.pos.epidemic || is.na(semana.fin) || is.na(i.epidemic.thresholds[2])) quitar.columnas <- c(quitar.columnas, 1)
  if (is.na(semana.inicio)) quitar.columnas <- c(quitar.columnas, 2)
  if (i.no.epidemic || is.na(i.epidemic.thresholds[1])) quitar.columnas <- c(quitar.columnas, 4)
  if (i.no.intensity) quitar.columnas <- c(quitar.columnas, 5:7)
  quitar.columnas <- c(quitar.columnas, (5:7)[is.na(i.intensity.thresholds)])
  if (length(quitar.columnas) > 0) {
    etiquetas.leyenda <- etiquetas.leyenda[-quitar.columnas]
    tipos.leyenda <- tipos.leyenda[-quitar.columnas]
    anchos.leyenda <- anchos.leyenda[-quitar.columnas]
    colores.leyenda <- colores.leyenda[-quitar.columnas]
    puntos.leyenda <- puntos.leyenda[-quitar.columnas]
    bg.leyenda <- bg.leyenda[-quitar.columnas]
  }
  if (is.na(semana.inicio) || is.na(semana.fin)) {
    xa <- "topright"
    ya <- NULL
  } else {
    if (semana.fin < 0.80 * semanas) xa <- "topright" else xa <- "topleft"
    ya <- NULL
  }
  legend(
    x = xa, y = ya, inset = c(0, -0.05), xjust = 0,
    legend = rev(etiquetas.leyenda),
    bty = "n",
    lty = rev(tipos.leyenda),
    lwd = rev(anchos.leyenda),
    col = rev(colores.leyenda),
    pch = rev(puntos.leyenda),
    bg = rev(bg.leyenda),
    cex = 0.75,
    x.intersp = 0.5,
    y.intersp = 0.7,
    text.col = "#000000",
    ncol = 1
  )
  par(opar)
  if (i.graph.file) dev.off()

  n.season.scheme <- dim(current.season)[1]
  season.scheme <- rep(0, n.season.scheme)
  if (is.na(semana.inicio)) {
    season.scheme[1:n.season.scheme] <- 1
  } else {
    if (is.na(semana.fin)) {
      if (semana.inicio > 1) season.scheme[1:(semana.inicio - 1)] <- 1
      season.scheme[semana.inicio:n.season.scheme] <- 2
    } else {
      if (semana.inicio > 1) season.scheme[1:(semana.inicio - 1)] <- 1
      season.scheme[semana.inicio:(semana.fin - 1)] <- 2
      season.scheme[semana.fin:n.season.scheme] <- 3
    }
  }
  season.scheme[is.na(current.season$rates)] <- NA

  current.season$season.scheme <- season.scheme

  memsurveillance.output <- list(
    graph.name = paste(i.output, "/", graph.name, ".tiff", sep = ""),
    current.season = current.season,
    real.start.week = semana.inicio.real,
    forced.start.week = semana.inicio.forzado,
    start.week = semana.inicio,
    end.week = semana.fin,
    data.graph = dgraf,
    param.current = i.current,
    param.epidemic.thresholds = i.epidemic.thresholds,
    param.intensity.thresholds = i.intensity.thresholds,
    param.mean.length = i.mean.length,
    param.force.length = i.force.length,
    param.output = i.output,
    param.graph.title = i.graph.title,
    param.graph.file = i.graph.file,
    param.graph.file.name = i.graph.file.name,
    param.week.report = i.week.report,
    param.equal = i.equal,
    param.pos.epidemic = i.pos.epidemic,
    param.no.epidemic = i.no.epidemic,
    param.no.intensity = i.no.intensity,
    param.epidemic.start = i.epidemic.start,
    param.range.x = i.range.x,
    param.range.x.53 = i.range.x.53,
    param.range.y = i.range.y,
    param.no.labels = i.no.labels,
    param.start.end.marks = i.start.end.marks
  )
  memsurveillance.output$call <- match.call()
  return(memsurveillance.output)
}
