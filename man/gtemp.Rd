
\name{gtemp}
\alias{gtemp}
\docType{data}
\title{
Global mean land-ocean temperature deviations
}
\description{
Global mean land-ocean temperature deviations, measured by
the Goddard Institute for Space Studies and supplied in package \code{astsa},
used in teaching Shumway and Stoffer's Time Series Analysis textbook.
}
\usage{data(gtemp)}
\format{
  A data frame with 130 observations on the following 2 variables.
  \describe{
    \item{\code{year}}{a numeric vector}
    \item{\code{temp}}{a numeric vector}
  }
}

\source{
  \url{https://data.giss.nasa.gov/gistemp/}  
}
\references{
  https://www.stat.pitt.edu/stoffer/tsa3/

  Sankaran, K and Holmes, S. structSSI: Simultaneous and Selective
  Inference for Grouped or Hierarchically Structured Data. Journal of
  Statistical Software, 59(13), 1-21. 2014. https://jstatsoft.org/v59/i13/

}
\examples{
data(gtemp)
if (interactive()) {
  plot(gtemp, type = 'l')
}
}

\keyword{datasets}