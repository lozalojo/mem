#' Castilla y Leon influenza standarised rates
#'
#' This data set contains \emph{Influenza Like Illness} (ILI) rates, in cases per 100,000
#' inhabitants collected by the \emph{Influenza Surveillance Programme} of the
#' \bold{Castilla y Leon Health Sentinel Network} (CyLHSN) from 2001 to 2008.
#'
#' The \bold{Castilla y Leon Health Sentinel Network} is a spanish regional influenza
#' surveillance system based upon volunteer health professionals. The \emph{Influenza
#' Surveillance Programme} consists on a random sample of general practitioners (covering
#' 30,000 population) which collect ILI cases weekly from 40th week (October) to 20th
#' week (May) of the following year to provide estimations of the ILI weekly rate for the
#' entire region.\cr
#' The data set contains 8 surveillance seasons, from 2001/2002 to 2008/2009.
#'
#' @docType data
#' @keywords datasets
#' @name flucylraw
#' @usage data(flucylraw)
#' @format A data frame with 267 observations on 2 variables. Each observation is one surveillance
#' week and rate,
#' \describe{
#'   \item{\code{year}}{a numeric vector - year.}
#'   \item{\code{week}}{a numeric vector - week.}
#'   \item{\code{rates}}{a numeric vector - standarised rates per 100,000 inhabitants.}
#' }
#' @source
#' Influenza Surveillance Programme. Castilla y Leon Health Sentinel Network. Consejeria
#' de Sanidad. Junta de Castilla y Leon 2001-2008.
#' @references
#' Castilla y Leon Health Sentinel Network Reports (Informes de la Red Centinela Sanitaria
#' de Castilla y Leon: \url{http://www.salud.jcyl.es/centinelas}).
#' Influenza Surveillance Programme (Programa de vigilancia de la gripe:
#' \url{http://www.salud.jcyl.es/centinelas}).
#' @examples
#' data(flucylraw)
#' plot(flucylraw$rates,type="l")
NULL
