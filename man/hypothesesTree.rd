\name{hypothesesTree-class}
\Rdversion{1.1}
\docType{class}
\alias{hypothesesTree-class}

\title{Class \code{"hypothesesTree"}}
\description{
Class for performing hierarchical multiple testing corrections.
}

\section{Slots}{
  \describe{
    \item{\code{tree}:}{Object of class \code{"matrix"}. The adjacency
  matrix for the tree. The ijth entry of the matrix is 0 if there is no
  link between hypotheses i and j, and it is 1 otherwise.}
    \item{\code{unadj.p.values}:}{Object of class \code{"numeric"}. A
  vector of the uandjusted p-values resulting from some multiple testing
  experiment. The \code{i}th element of this vector corresponds to
  hypotheses \code{i} in the argument \code{hypotheses.names} and the
  \code{i} row and column of the adjacency matrix in \code{tree}.}
    \item{\code{adj.p.values}:}{Object of class \code{"numeric"}. A
  vector of the p-values for these hypotheses after a multiple testing
  correction has been performed.}
    \item{\code{hypotheses.names}:}{Object of class
  \code{"character"}. A vector of the names of the  hypotheses under
  consideration. The hypotheses in position \code{i} of this vector
  corresponds to the \code{i}th row and column of the adjacency matrix.
  }
    \item{\code{rejected.hypotheses}:}{Object of class
  \code{"character"} A vector of those hypotheses that were rejeceted by
  some unprespecified testing procedure.}
  }
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "hypothesesTree")}:
      ...}{
      Check that the hypotheses tree is correctly initialized. It
      ensures that the number of unadjusted p-values, hypotheses names, and
      nodes in the hypotheses tree all agree. It also checks that the
      hypotheses tree is in fact a tree.
  \item{plot}{\code{signature(x = "hypothesesTree", y = "ANY")}: ... }{
    Plots the tree of hypotheses and their p-values either before or
  after adjustment. If a particular node hypothesis was not tested, it
  is colored grey. If it was tested and rejected, it is green; if it was
  tested and not rejected, it is shaded blue. The deeper the shade, the
  lower (more significant) the p-value was. }
\item{show}{\code{signature(object = "hypothesesTree")}: ... }{
  This prints the main information contained in the hypotheses tree (the
  number of hypotheses, the hypotheses names, adjusted p-values,
  rejected hypotheses, etc).
}
}
}
}

\author{
  Kris Sankaran
}

\seealso{
  \code{\link{hFDR.adjust}}
  \code{\link{plot-methods}}
}

\examples{

library('ape')
library('igraph')
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

}

\keyword{classes}
