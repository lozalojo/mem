#' Moving Epidemic Method R Package
#'
#' This package creates the model described in the \emph{Moving Epidemics Method} (MEM),
#' used to monitor influenza activity during the seasonal surveillance.
#'
#' \tabular{ll}{
#' Package: \tab mem\cr
#' Type: \tab Package\cr
#' Title: \tab Moving Epidemics Method R Package.\cr
#' Version: \tab 1.4\cr
#' Date: \tab 2014-07-10\cr
#' Author: \tab Jose E. Lozano Alonso <lozalojo@jcyl.es>\cr
#' Maintainer: \tab Jose E. Lozano Alonso <lozalojo@jcyl.es>\cr
#' Depends: \tab R (>= 3.2.0)\cr
#' Description: \tab Modelization of influenza epidemics in order to monitor future
#' activity.\cr
#' License: \tab GPL (>= 2)\cr
#' }
#'
#' The Moving Epidemics Method (MEM) is a tool developed in the Health Sentinel Network of Castilla y Leon (Spain)
#' to help in the routine influenza surveillance in health systems. It gives a better understanding of the annual
#' influenza epidemics and allows the weekly assessment of the epidemic status and intensity.
#'
#' Although in its conception it was originally created to be used with influenza data and health sentinel networks,
#' MEM has been tested with different respiratory infectious diseases and surveillance systems so nowadays it could
#' be used with any parameter which present a seasonal accumulation of cases that can be considered an epidemic.
#' MEM development started in 2001 and the first record appeared in 2003 in the Options for the Control of Influenza V.
#' It was presented to the baselines working group of the European Influenza Surveillance Scheme (EISS) in the 12th
#' EISS Annual Meeting (Malaga, Spain, 2007), with whom started a collaboration that continued when, in 2008, was
#' established the European Influenza Surveillance Network.
#'
#' In 2009 MEM is referenced in an official European document: the Who European guidance for influenza surveillance
#' in humans. A year later MEM was implemented in The European Surveillance System (TESSy), of the European Centre
#' for Disease Prevention and Control (ECDC), and in 2012, after a year piloting, in the EuroFlu regional influenza
#' surveillance platform, of the World Health Organization Regional Office for Europe (WHO-E).
#'
#' As a result of the collaboration with ECDC and WHO-E, two papers have been published, one related to the establishment
#' of epidemic thresholds and other to the comparison of intensity levels in Europe.
#'
#' In 2014 a tool was created to help users around the world to apply mem on their data. It was released in July 2014 as
#' a package for R, a free software environment for statistical computing and graphics. This is the first version of the
#' library (also referred to as the stable version).
#'
#' The second version of the mem R library was released in 2015 and included a lot of new features and graphics. It was
#' published as an open source project at GitHub, a web-based Git or version control repository and Internet hosting
#' service. It is available directly from github and it is also known as the development version since it is constantly
#' being updated.
#'
#' \url{https://github.com/lozalojo/mem}
#'
#' This version was incorporated to the official R repositories, The Comprehensive R Archive Network (CRAN), in June 2017.
#' In 2017 a web application was created to serve as a graphical user interface for the R mem library using Shiny, a web
#' application framework for R. This application is based on the development version of the mem R library. It is hosted
#' at GitHub and also at CRAN.
#'
#' \url{https://github.com/lozalojo/memapp}
#'
#' @name mem-package
#'
#' @docType package
#'
#' @aliases mem
#'
#' @return \code{NULL}
#'
#' @examples
#' # Castilla y Leon Influenza Rates data
#' data(flucyl)
#' # Optimal timing of an epidemic
#' tim<-memtiming(flucyl[1])
#' print(tim)
#' summary(tim)
#' plot(tim)
#' # Threshold calculation
#' epi<-memmodel(flucyl[1:7])
#' print(epi)
#' summary(epi)
#' plot(epi)
#' # Intensity thresholds
#' intensity<-memintensity(epi)
#' intensity
#' # Trend parameters
#' trend<-memtrend(epi)
#' trend
#' # Epidemic thresholds
#' e.thr<-epi$epidemic.thresholds
#' # Intensity threhsolds
#' i.thr<-epi$intensity.thresholds
#' # Surveillance
#' memsurveillance(flucyl[8],e.thr,i.thr,i.graph.file=FALSE)
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
#' @keywords package
NULL
