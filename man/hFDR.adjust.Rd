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

hFDR.adjust(hyp.tree, alpha = 0.05)

}

\arguments{
  \item{hyp.tree}{
    An object of class \code{hypothesesTree}.
}
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

  A list where the first element contains a character vector of the names
  of those hypotheses that were rejected and the second element contains
  a vector of the adjusted p-values resulting from the procedure.

}

\references{

Yekutieli, D. Hierarchical false discovery rate-controlling
methodology. Journal of the American Statistical Association,
103(481):309-316, 2008.

Benjamini, Y, and Yekutieli, D. Hierarchical fdr testing of trees of
hypotheses. 2002.

}

\author{

  Kris Sankaran

}

\examples{

library('igraph')
library('ape')

set.seed(12309)
tree <- as.matrix(get.adjacency(as.igraph(rtree(25)), sparse = FALSE))
hypotheses.names <- paste("Hyp ", c(1:49))
alternative.indices <- sample(1:49, 30)
unadj.p.values <- vector("numeric", length = 49)
unadj.p.values[alternative.indices] <- runif(30, 0, 0.001)
unadj.p.values[-alternative.indices] <- runif(19, 0, 1)
unadj.p.values[c(1:5)] <- runif(5, 0, 0.0001)

hyp.tree <- new("hypothesesTree", tree = tree,
                 hypotheses.names = hypotheses.names,
                 unadj.p.values = unadj.p.values)

hyp.tree.adjust <- hFDR.adjust(hyp.tree, 0.05)
hyp.tree@rejected.hypotheses <- hyp.tree.adjust$rejected.hypotheses
hyp.tree@adj.p.values <- hyp.tree.adjust$adjp.values

## We can visualize the difference between the unadjusted and the
## adjusted trees.

plot(hyp.tree, p.values.type = "unadjusted")
plot(hyp.tree, p.values.type = "adjusted")


}

\keyword{ ~htest }
