\name{hFDR.adjust}
\alias{hFDR.adjust}

\title{

FDR Controlling Procedure for Hierarchically Structured Hypotheses

}

\description{

  This function implements the Hierarchical FDR controlling procedure
  developed Benjamini and Yekutieli. The procedure incorporates
  structural information about the hierarchical relationships between
  hypotheses in order to increase power and interpretability of a
  testing procedure while controlling the False Discovery Rate at a
  prespecified level. It is applicable whenever we there  is a natural
  hierarchical structure between the hypotheses being tested before the
  data analysis begins. For example, the method has been used before in
  Clinical Studies, where nodes deeper in the tree correspond to
  secondary or tertiary endpoints. It has also been used in QTL
  analysis, where we first make statements about regions of chromosomes
  being associated with specific brain activity and then focus on more
  and more detailed subsets of chromosomes.

}

\usage{

hFDR.adjust(unadjp, tree.el, alpha = 0.05)

}

\arguments{
  \item{unadjp}{A vector of raw p-values resulting from an
    experiment. The names of this vector should be contained
    in the edgelist parameterizing the hierarchical structure between
    hypothesis, inputted as \code{tree.el}.
  }
  \item{tree.el}{The edgelist parameterizing the hierarchical
    structure between hypotheses. The edges must be stored so that
    each edge is a row of a two column matrix, where the first
    column gives the parent and the second gives the child.}
  
  \item{alpha}{
    The level of FDR control within families of the tree. Note that this
    is NOT necessarily the level of FDR control within the entire
    tree. Refer to the paper by Yekutieli and Benjamini for bounds on
    various FDR criterion.
  }
}
\details{
  The FDR controlling procedure is described in more detail in the paper
  by Yekutieli and Benjamini 2009. The idea is to control for multiple
  testing error within families of hypotheses, and only test a
  descendant family of hypotheses if the associated parent hypotheses
  was deemed significant in the higher level. The families of hypotheses
  are taken to be the children of any particular node, and error is
  controlled within these families using the Benjamini-Hochberg
  procedure. Different bounds can be proven for the FDR when considered
  along whole tree, within a single level, and tips. In particular, the
  whole tree FDR is typically controlled at three times the FDR control
  within individual families, and this result holds for arbitrary
  hypotheses tests and configurations of trees.
}

\value{
  An object of class \code{hypothesesTree}.
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

\author{Kris Sankaran}

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

## We can visualize the difference between the unadjusted and the
## adjusted trees.
plot(hyp.tree, adjust = FALSE)
plot(hyp.tree, adjust = TRUE)
}

\seealso{\code{\link{hypothesesTree-class}}}
\keyword{ ~htest }
