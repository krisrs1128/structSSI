\name{hypothesesTree-class}
\Rdversion{1.1}
\docType{class}
\alias{hypothesesTree-class}
\alias{plot,hypothesesTree,ANY-method}
\alias{show,hypothesesTree-method}
\alias{summary,hypothesesTree-method}

\title{Class \code{"hypothesesTree"}}

\description{
Class for performing hierarchical multiple testing corrections.
}

\section{Slots}{

  \describe{
    \item{\code{tree}:}{Object of class \code{"matrix"}. The edgelist
      for the hypotheses tree.}
    \item{\code{p.vals}:}{Object of class \code{"data.frame"}. Each
      row correpsonds to an individual hypothesis. The first column
      stores the p-values before GBH adjustment, while the second
      gives the hFDR adjusted p-values. The hypotheses are sorted
      in order of significance according to these GBH adjusted
      p-values. The \code{group} column gives the group membership
      of each hypothesis, and \code{adj.significnace} codes the
      significance of each hypothesis, according to the GBH adjusted
      p-values.}
    \item{\code{alpha}:}{Object of class \code{"numeric"}. The
      level at which the FDR is controlled among children
      of each parent node.}
  }
}
  
\section{Methods}{
    \describe{
    \item{initialize}{\code{signature(.Object = "hypothesesTree")}:
      ...}{
      Check that the hypotheses tree is correctly initialized. It
      ensures that the number of unadjusted p-values, hypotheses names, and
      nodes in the hypotheses tree all agree. It also checks that the
      hypotheses tree is in fact a tree.}
    \item{plot}{\code{signature(x = "hypothesesTree", y = "ANY")}: ... }{
    Plots the tree of hypotheses and their p-values either before or
    after adjustment. If a particular node hypothesis was not tested, it
    is colored grey. If it was tested and rejected, it is green; if it was
    tested and not rejected, it is shaded blue. The deeper the shade, the
    lower (more significant) the p-value was.}
  \item{show}{\code{signature(object = "hypothesesTree")}: ... }{
    This prints the unadjusted and adjusted p-values of the hypotheses
    tree associated with the HFDR procedure.}
  \item{summary}{\code{signature(object = "hypothesesTree")}:
    This prints the most significant adjusted p-values, along with
    estimates of the FDR across the tree and at tips.}
}
}


\author{
  Kris Sankaran
}

\seealso{
  \code{\link{hFDR.adjust}}
  \code{\link{EstimatedHFDRControl}}
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
plot(hyp.tree)
}

\keyword{classes}
