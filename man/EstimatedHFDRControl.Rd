

\name{EstimatedHFDRControl}
\alias{EstimatedHFDRControl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Estimate FDR control variations for HFDR procedure}
\description{
  This function estimates two types of HFDR control appropriate
  for trees of hypotheses. If the BH procedure is applied
  at level alpha within each of the tree families, this is defined as

  \deqn{FDR.est := \alpha * [\#\textit{discoveries} + \#\textit{families tested}] /
  [\textit{\#discoveries} + 1]}

  where a discovery is defined as an adjusted p value
  below alpha within the entire tree or at
  the tips for tree and tips FDR, respectively.  
}
\usage{
EstimatedHFDRControl(hyp.tree)
}

\arguments{
  \item{hyp.tree}{
    An object of class \code{HypothesesTree}, such as the
    result to a call of \code{hFDR.adjust}.
}
}

\value{
  \item{tree}{The estimated full-tree FDR.}
  \item{tip}{The estimated outer-nodes FDR.}
  \item{n.families.tested}{The number of families of hypotheses
    tested by the HFDR procedure.}
  \item{n.tree.discoveries}{The number of discoveries over the
    whole tree.}
  \item{n.tip.discoveries}{The number of discoveries among the tree
    tips.}
}
\references{
Yekutieli, D. Hierarchical false discovery rate-controlling
methodology. Journal of the American Statistical Association,
103(481):309-316, 2008.

Benjamini, Y, and Yekutieli, D. Hierarchical fdr testing of trees of
hypotheses. 2002.

Sankaran, K and Holmes, S. structSSI: Simultaneous and Selective
Inference for Grouped or Hierarchically Structured Data. Journal of
Statistical Software, 59(13), 1-21. 2014. http://jstatsoft.org/v59/i13/
}
\author{
Kris Sankaran
}

\seealso{
  \code{\link{hypothesesTree-class}}
  \code{\link{hFDR.adjust}}
}
\examples{
library('igraph')
library('ape')

alternative.indices <- sample(1:49, 30)
unadj.p.values <- vector("numeric", length = 49)
unadj.p.values[alternative.indices] <- runif(30, 0, 0.01)
unadj.p.values[-alternative.indices] <- runif(19, 0, 1)
unadj.p.values[c(1:5)] <- runif(5, 0, 0.01)
names(unadj.p.values) <- paste("Hyp ", c(1:49))

tree <- as.igraph(rtree(25))
V(tree)$name <- names(unadj.p.values)
tree.el <- get.edgelist(tree)

hyp.tree <- hFDR.adjust(unadj.p.values, tree.el, 0.05)

EstimatedHFDRControl(hyp.tree)
}
\keyword{  ~htest }

