\name{estimate.pi0}
\alias{estimate.pi0}

\title{
Estimation of Proportion of Null Hypotheses among p-values
}

\description{

  This function makes three routines available for estimating the
  true proportion of null hypotheses from a vector of unadjusted
  p-values arising from a multiple testing experiment.
  The specific  methods are the Least Slope method (\code{lsl}), the
  Two Step Test method (\code{tst}), and the Storey tail proportion of
  p-values  method (\code{storey}). These methods are derived and
  explained in the references given below.

}

\usage{
estimate.pi0(pvalues, method, alpha = 0.05, lambda = 0.5)
}

\arguments{
  \item{pvalues}{
    A vector of the unadjusted p-values resulting from a multiple
    testing experiment.
  }

  \item{method}{
    The technique used to estimate the proportion of true null
    hypotheses. Valid arguments are \code{lsl}, \code{tst}, or
    \code{storey}, which correspond to the Least Slope, Two Step Test,
    and Storey tail proportion of p-values methods, respectively.
  }
  \item{alpha}{
    In the Two Step Test method, the level of the
    Benjamini-Hochberg procedure used to estimate the propotion of true
    null hypotheses.
  }
  \item{lambda}{
    In the Storey tail proportion of p-values method, the
    cutoff used to differentiate p-values.
  }
}

\details{
  The Least Slope method uses the insight that, if we plot the sorted
  unadjusted  p-values, then the p-values corresponding to null
  hypotheses will have steep slopes, as they are uniformly distributed
  between 0 and 1. If we find the position where the slope of the
  sorted p-values increases the most, we can fix that slope and
  extrapolate to the x-axis, and the position where the line intersects
  the x-axis is the proportion of p-values estimated to be null. The
  formal derivation is presented in the reference below.

  Storey's method uses the idea that most of the p-values above some
  parameter lambda (usually set to 0.5) correspons to  null
  hypotheses. The number of hypotheses in this interval can then be used
  to estimate the number of hypotheses overall which are null
  hypotheses. See the paper

  The Two Step Test method uses the idea that the result of a multiple
  comparisons procedure gives an estimate for how many hypotheses are
  null. That is, if we perform the BH procedure on 100 hypotheses, and
  10 of them are rejected, then a reasonable estimate of the proportion
  of null hypotheses among those 100 is pi0 = 0.9.


}

\value{
  An estimate of the proportion of true null hypotheses from the result
  of the multiple testing experiment that the unadjusted p-values were
  extracted from.
}

\references{

  Banjamini, Y, Krieger, A.M., and Yekutieli, D. Adaptive linear step-up
  procedures that control the false discovery rate. Biometrica,
  93(3):491, 2006.

  Benjamini, Y, and Hochberg, Y. ``On the adaptive control of the false
  discovery rate in multiple testing with independent statistics.''
  Journal of Educational and Behavioral Statistics, 25(1):60, 2000.

  Storey, J.D., Taylor, J.E., and Siegmund, D. Strong control,
  conservative point estimation, and simultaneous conservative
  consistency of false discovery rates: a unified approach. Journal of
  the Royal Statistical Society: Series B (Statistical
  Methodology),66(1):187-205. 2004.

}

\author{

  Kris Sankaran

}

\examples{

true.p.1 <- runif(20, 0, 0.01)
null.p.1 <- runif(980, 0, 1)
p.val.1 <- c(true.p.1, null.p.1)

estimate.pi0(p.val.1, "lsl")
estimate.pi0(p.val.1, "storey", lambda = 0.2)
estimate.pi0(p.val.1, "tst")

}

\keyword{ ~htest }