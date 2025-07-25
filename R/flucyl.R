#' Castilla y Leon influenza crude rates
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
#' entire region.
#'
#' The data set contains 8 surveillance seasons, from 2001/2002 to 2008/2009.
#'
#' @docType data
#' @keywords datasets
#' @name flucyl
#' @usage data(flucyl)
#' @format A data frame with 33 observations on 8 variables. Each observation is one surveillance
#' week, and each variable is an influenza season.
#' \describe{
#'   \item{\code{2001/2002}}{a numeric vector - 2001/2002 rates per 100,000 inhabitants.}
#'   \item{\code{2002/2003}}{a numeric vector - 2002/2003 rates per 100,000 inhabitants.}
#'   \item{\code{2003/2004}}{a numeric vector - 2003/2004 rates per 100,000 inhabitants.}
#'   \item{\code{2004/2005}}{a numeric vector - 2004/2005 rates per 100,000 inhabitants.}
#'   \item{\code{2005/2006}}{a numeric vector - 2005/2006 rates per 100,000 inhabitants.}
#'   \item{\code{2006/2007}}{a numeric vector - 2006/2007 rates per 100,000 inhabitants.}
#'   \item{\code{2007/2008}}{a numeric vector - 2007/2008 rates per 100,000 inhabitants.}
#'   \item{\code{2008/2009}}{a numeric vector - 2008/2009 rates per 100,000 inhabitants.}
#' }
#' @source
#' Influenza Surveillance Programme. Castilla y Leon Health Sentinel Network. Consejeria
#' de Sanidad. Junta de Castilla y Leon 2001-2008.
#' @references
#' Castilla y Leon Health Sentinel Network Reports (Informes de la Red Centinela Sanitaria
#' de Castilla y Leon).\cr
#' Influenza Surveillance Programme (Programa de vigilancia de la gripe).\cr
#' \url{https://www.saludcastillayleon.es/profesionales/es/centinelas}
#' @examples
#' data(flucyl)
#' plot(flucyl[, 1], type = "l")
NULL
