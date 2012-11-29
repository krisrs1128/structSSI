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

\usage{
Adaptive.GBH(unadj.p, groups, alpha, method)
}

\arguments{
  \item{unadj.p}{
    A vector of the unadjusted p-values resulting from a multiple testing
    experiment.
  }
  \item{groups}{
    A vector of the same length as the vector of unadjusted p-values,
    where an 'i' in the jth coordinate indicates that the jth unadjusted
    p-values in \code{unadj.p} belongs to group i. The entries in this
    vector should range from 1 to $K$, where $K$ is the total number of
    groups identified among our hypotheses.
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
}

\value{
  A list containing the adjusted p-values of the experiment, the
  hypotheses corresponding to these adjusted p-values, the names of the
  rejected hypotheses, the names of the unrejected hypotheses, and the
  estimates of the proportion of null hypotheses within each of the
  grousp that was used in the adjustment.
}

\references{
  Hu, J.X., Zhao, H., and Zhou, H.H. False discovery rate control with
  groups. Journal of the American Statistical Association, volume 104,
  number 491. Pages 1215-1227. 2010.
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

## This is the expected format of the groups argument: a
## numeric vector where the ith entry indicates the group
## membership of hypothesis i. The entries are expected to be
## between 1 and N, where N is the number of groups under
## consideration. Notice here that there are two groups total
## and that we have randomly assigned hypotheses to these two
## groups.

groups <- c(sample(1:2, 2000, replace = TRUE))

# Perform the GBH adjustment.

result <-  Adaptive.GBH(unadjp, groups, method = "storey", alpha = 0.05)

names(result)

# The head of adjusted p-values.

head(result$adjp)

# The hypotheses corresponding to the most significant
# p-values.

head(result$adjp.index)

}


\keyword{ ~htest }