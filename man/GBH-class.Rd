\name{GBH-class}
\Rdversion{1.1}
\docType{class}
\alias{GBH-class}
\alias{plot,GBH,ANY-method}
\alias{show,GBH-method}
\alias{summary,GBH-method}

\title{Class \code{"GBH"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("GBH", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{GBH.adjust}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{pi0}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{adaptive}:}{Object of class \code{"logical"} ~~ }
    \item{\code{alpha}:}{Object of class \code{"numeric"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "GBH", y = "ANY")}: ... }
    \item{show}{\code{signature(object = "GBH")}: ... }
    \item{summary}{\code{signature(object = "GBH")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("GBH")
}
\keyword{classes}
