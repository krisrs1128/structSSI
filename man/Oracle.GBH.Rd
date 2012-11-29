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
Oracle.GBH(unadj.p, groups, pi.groups, alpha)
}

\arguments{
  \item{unadj.p}{
    A vector of the unadjusted p-values resulting from a multiple
    testing experiment.
  }
  \item{groups}{
    A vector of the same length as the vector of unadjusted p-values,
    where an 'i' in the jth coordinate indicates that the jth unadjusted
    p-values in \code{unadj.p} belongs to group i.
  }
  \item{pi.groups}{
    A vector of the known proportions of true null hypotheses within
    each of the groups. The ith element of this vector is the proportion
    of null hypotheses in group i within the groups argument.
  }
  \item{alpha}{
    The level that we are attempting to control the FDR at.
  }
}

\value{

  A list containing the sorted GBH adjusted p-values, sorted indices
  corresponding to those adjusted p-values, sorted names of rejected
  hypotheses, and unrejected hypotheses.


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
  \code{\link{Adaptive.GBH}}
}

\examples{
# A very simple example, with only 5 hypotheses.

unadjp <- c(0.002, 0.1, 0.001, 0.01, 0.4)
names(unadjp) <- paste("hyp", 1:5)
groups <- c(1, 2, 1, 2, 2)

# Say we know goup 1 has pi_0,1 = 0.3 and pi_0,2 = 0.9

Oracle.GBH(unadjp, groups, c(0.3, 0.9), 0.05)

# An example where we use an external pi0 estimation routine

unadjp.2 <- c(runif(500, 0, 0.01), runif(1500, 0, 1))
names(unadjp.2) <- paste("hyp", 1:2000)
groups.2 <- c(sample(1:2, 2000, replace = TRUE))
pi.groups <- c(1:2)
for(i in 1:2){
    pi.groups[i] <- estimate.pi0(unadjp.2[which(groups.2 == i)], method =
                                 "storey")
}

result <- Oracle.GBH(unadjp.2, groups.2, pi.groups, 0.05)
head(result$adjp)
head(result$rejected)
head(result$not.rejected)

}

\keyword{ ~htest }