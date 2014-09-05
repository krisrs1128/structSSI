\name{Adaptive.GBH}
\alias{Adaptive.GBH}

\title{
Adaptive Group Benjamini-Hochberg Procedure
}

\description{
Performs the Group Benjamini-Hochberg procedure using adaptive estimates
of the proportions of null p-values in given groups. The method is
applicable when we know some a-priori structure about whether certain
hypotheses can be grouped. Once the hypotheses are grouped and tested
individually, a Benjamini-Hochberg correction is performed within each
of the groups. Finally, the fact that the Benjamini-Hochberg correction
controls the FDR at level q*pi0_group within each group (q is the level
used in the and p-value comparison and pi0 is the proportion of null
hypotheses within the particular group) is used to increase the power of
the procedure. The procedure is described in more detail in the paper
"False Discovery Rate Control with Groups" by Hu, Zhao, and Zhou (see
references below).
}

\usage{Adaptive.GBH(unadj.p, group.index, alpha = 0.05, method = 'lsl', lambda = 0.5)}

\arguments{
  \item{unadj.p}{
    A vector of the unadjusted p-values resulting from a multiple testing
    experiment.
  }
  \item{group.index}{
    A vector of the same length as the vector of unadjusted p-values,
    where a "G" in the jth coordinate indicates that the jth unadjusted
    p-values in \code{unadj.p} belongs to group "G". This code can be
    either a factor giving group names, or a numeric index.
  }
  \item{alpha}{
    The level of that we are attempting to control the FDR at.
  }
  \item{method}{
    The method for adaptively estimating the proportion of true null
    hypotheses within a vector of unadjusted p-values. The possible
    options are "storey", "lsl", and "tst". See the documentation for
    \code{estimatePi0} for more details.
  }
  \item{lambda}{
    The value of the tuning parameter for estimating the proportion
    of null hypotheses, in the "storey" method.
  }
}

\value{
  An object of class \code{GBH}, which provides the adjusted p-values.
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
  \code{\link{estimate.pi0}}
  \code{\link{Oracle.GBH}}
}
\examples{
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

# The estimated proportions of null hypotheses, between groups
result@pi0

# Visualizing the sorted p-values, before and after adjustment
plot(result, adjust = TRUE)
plot(result, adjust = FALSE)
}

\keyword{ ~htest }
