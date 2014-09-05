\name{Oracle.GBH}
\alias{Oracle.GBH}

\title{
Oracle Group Benjamini-Hochberg Correction
}
\description{
Performs the Group Benjamini-Hochberg procedure when the true proportion
of null hypotheses is known within each group. The procedure is
applicable whenever group structure about the relationship between
different hypotheses is known before testing begins. The idea is to
control the FDR within each group and to use the proportion of null
hypotheses present within each group to make the testing procedure
within that group either more or less conservative -- this is in fact
the idea behind all adaptive multiple testing procedures.

The Oracle GBH method can also be used when we would like to use the
Adaptive GBH procedure but with estimates of proportions of true null
hypotheses within groups that are not made directly available through
the \code{Adaptive.GBH} function -- in this case those estimates can be
inputted as the argument \code{pi.groups} in the this function \code{Oracle.GBH}.
}

\usage{
Oracle.GBH(unadj.p, group.index, pi.groups, alpha = 0.05)
}

\arguments{
  \item{unadj.p}{
    A vector of the unadjusted p-values resulting from a multiple
    testing experiment.
  }
  \item{group.index}{
    A vector of the same length as the vector of unadjusted p-values,
    where a "G" in the jth coordinate indicates that the jth unadjusted
    p-values in \code{unadj.p} belongs to group "G". This code can be
    either a factor giving group names, or a numeric index.
  }
  \item{pi.groups}{
    A vector of the known proportions of true null hypotheses within
    each of the groups. This vector should be named so that each
    element of the \code{group.index} vector correspond to one of the names
    of the \code{pi.groups} vector.
  }
  \item{alpha}{
    The level that we are attempting to control the FDR at.
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
  \code{\link{GBH-class}}
  \code{\link{estimate.pi0}}
  \code{\link{Adaptive.GBH}}
}

\examples{
# A very simple example, with only 5 hypotheses.
unadjp <- c(0.002, 0.1, 0.001, 0.01, 0.4)
names(unadjp) <- paste("hyp", 1:5)
groups <- c(1, 2, 1, 2, 2)

# Say we know goup 1 has pi_0,1 = 0.3 and pi_0,2 = 0.9
pi.groups <- c("1" = 0.3, "2" = 0.9)
Oracle.GBH(unadjp, groups, pi.groups)

# An example where we use an external pi0 estimation routine

unadjp.2 <- c(runif(500, 0, 0.01), runif(1500, 0, 1))
names(unadjp.2) <- paste("hyp", 1:2000)
groups.2 <- c(sample(1:2, 2000, replace = TRUE))
pi.groups <- c("1" = NA, "2" = NA)
for(i in 1:2){
    pi.groups[i] <- estimate.pi0(unadjp.2[which(groups.2 == i)], method =
                                 "storey")
}

result <- Oracle.GBH(unadjp.2, groups.2, pi.groups, 0.05)
result@pi0
result@p.vals
}
\keyword{ ~htest }
