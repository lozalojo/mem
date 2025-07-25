% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimum.by.inspection.R
\name{optimum.by.inspection}
\alias{optimum.by.inspection}
\title{Inspection calcultation of the optimum}
\usage{
optimum.by.inspection(
  i.data,
  i.param.values = seq(1, 5, 0.1),
  i.graph = FALSE,
  i.graph.file = FALSE,
  i.graph.file.name = "",
  i.graph.title = "",
  i.graph.subtitle = "",
  i.output = ".",
  i.labels.axis = c("Week", "Weekly rate"),
  i.labels.periods = c("Pre", "Epidemic", "Post"),
  i.labels.details = c("Sensitivity", "Specificity",
    "First row represents real and second row observed pos/neg values"),
  i.tolerance = 0.25
)
}
\arguments{
\item{i.data}{Data frame of input data.}

\item{i.param.values}{values to use in the i.param value of \code{memtiming}.}

\item{i.graph}{create a graph with the outputs (T/FALSE).}

\item{i.graph.file}{write the graph to a file.}

\item{i.graph.file.name}{name of the output file.}

\item{i.graph.title}{title of the graph.}

\item{i.graph.subtitle}{subtitle of the graph.}

\item{i.output}{output directory.}

\item{i.labels.axis}{different labels used by output graphs}

\item{i.labels.periods}{different labels used by output graphs}

\item{i.labels.details}{different labels used by output graphs}

\item{i.tolerance}{tolerance (in inches) when identifying points in the graph}
}
\value{
\code{optimum.by.inspection} returns a list.
An object of class \code{mem} is a list containing at least the following components:
  \item{optimum}{optimum value.}
  \item{optimum.data}{Data related to the optimum value.}
  \item{summary.data}{Data for all values tested.}
  \item{inspection.data}{Detailed results of each iteration.}
}
\description{
Function \code{optimum.by.inspection} perform an analysis of mem parameters
to find the one that fits better a panel of experts inspection criterium.

To be written
}
\examples{
\donttest{
# Castilla y Leon Influenza Rates data
data(flucyl)
# Inspection. It runs interactively (uncomment to run)
# if (interactive()) {
#   opt.ins <- optimum.by.inspection(flucyl, i.param.values = seq(2.0, 3.0, 0.1))
#   opt.ins$optimum
# }
}

}
\references{
Vega T, Lozano JE, Ortiz de Lejarazu R, Gutierrez Perez M. Modelling influenza epidemic - can we
detect the beginning and predict the intensity and duration? Int Congr Ser. 2004 Jun;1263:281-3.

Vega T, Lozano JE, Meerhoff T, Snacken R, Mott J, Ortiz de Lejarazu R, et al. Influenza surveillance
in Europe: establishing epidemic thresholds by the moving epidemic method. Influenza Other Respir
Viruses. 2013 Jul;7(4):546-58. DOI:10.1111/j.1750-2659.2012.00422.x.

Vega T, Lozano JE, Meerhoff T, Snacken R, Beaute J, Jorgensen P, et al. Influenza surveillance in
Europe: comparing intensity levels calculated using the moving epidemic method. Influenza Other
Respir Viruses. 2015 Sep;9(5):234-46. DOI:10.1111/irv.12330.

Lozano JE. lozalojo/mem: Second release of the MEM R library. Zenodo [Internet]. [cited 2017 Feb 1];
Available from: \url{https://zenodo.org/record/165983}. DOI:10.5281/zenodo.165983
}
\author{
Jose E. Lozano \email{lozalojo@gmail.com}
}
\keyword{influenza}
