\name{GBH-class}
\Rdversion{1.1}
\docType{class}
\alias{GBH-class}
\alias{plot,GBH,ANY-method}
\alias{show,GBH-method}
\alias{summary,GBH-method}

\title{Class \code{"GBH"}}
\description{Class to facilitate performing the Group
  Benjamini-Hochberg procedure and interpreting its output. }

\section{Slots}{
  \describe{
    \item{\code{p.vals}:}{Object of class \code{"data.frame"}. Each
      row correpsonds to an individual hypothesis. The first column
      stores the p-values before GBH adjustment, while the second
      gives the GBH adjusted p-values. The hypotheses are sorted
      in order of significance according to these GBH adjusted
      p-values. The \code{group} column gives the group membership
      of each hypothesis, and \code{adj.significnace} codes the
      significance of each hypothesis, according to the GBH adjusted
      p-values.}
    \item{\code{pi0}:}{Object of class \code{"numeric"}. The proportion
      of null hypotheses within each group. This is either known
      a priori or estimated adaptively from the unadjusted p-values.
    }
    \item{\code{adaptive}:}{Object of class \code{"logical"}. An
      indicator of whether the proportion \code{pi0} was estimated
      adaptively from the data or known a priori.}
    \item{\code{alpha}:}{Object of class \code{"numeric"}. The level
      at which the FDR is controlled, during the GBH procedure.}
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "GBH", y = "ANY")}:
      ... }{Plots the p-values of the hypothesis, sorted according to
      GBH adjusted significance, shape coded according to group
      membership, and color coded according to pre and post
      GBH p-value adjustment.}
    \item{show}{\code{signature(object = "GBH")}: ... }{Prints the
      entire table of adjusted p-values and their associated FDR adjusted
      significance levels, together with the estimated proportions
      of null hypotheses, within each group.}
    \item{summary}{\code{signature(object = "GBH")}: ... }{Prints
      the most significant hypothesis, after adjusting for multiple
      testing via GBH. Also supplies the estimated proportion of null
      hypothesis within each group and a table of counts of
      adjusted significance across groups.}
    }
  }
\references{
  Hu, J.X., Zhao, H., and Zhou, H.H. False discovery rate control with
  groups. Journal of the American Statistical Association, volume 104,
  number 491. Pages 1215-1227. 2010.

  Sankaran, K and Holmes, S. structSSI: Simultaneous and Selective
  Inference for Grouped or Hierarchically Structured Data. Journal of
  Statistical Software, 59(13), 1-21. 2014. http://jstatsoft.org/v59/i13/

}
\author{
Kris Sankaran
}

\seealso{
  \code{\link{Adaptive.GBH}}
  \code{\link{Oracle.GBH}}
}
\examples{
## These are the unadjusted p-values corresponding to
## the outcome of some multiple testing experiment. The
## first 500 hypotheses are null and the last 1500 are
## true alternatives.

unadjp <- c(runif(500, 0, 0.01), runif(1500, 0, 1))
names(unadjp) <- paste("Hyp: ", 1:2000)

## These are the unadjusted p-values corresponding to
## the outcome of some multiple testing experiment. The
## first 500 hypotheses are null and the last 1500 are
## true alternatives.
unadjp <- c(runif(500, 0, 0.01), runif(1500, 0, 1))
names(unadjp) <- paste("Hyp: ", 1:2000)

## Here there are two groups total we have randomly
##  assigned hypotheses to these two groups.
group.index <- c(sample(1:2, 2000, replace = TRUE))

# Perform the GBH adjustment.
result <-  Adaptive.GBH(unadjp, group.index, method = "storey")

# A summary of the GBH adjustment
summary(result)
}

\keyword{classes}
