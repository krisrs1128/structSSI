\name{plot-methods}
\docType{methods}
\alias{plot,hypothesesTree,ANY-method}
\title{Method to \code{plot} an object of class \code{hypothesesTree}}
\description{
Method to  \code{plot} an object of class \code{hypothesesTree}
}
\section{Methods}{
\describe{

\item{\code{signature(x = "hypothesesTree", y = "ANY")}}{

A method to \code{plot} an object of class \code{hypothesesTree}. The
user has two options: plot the tree with unadjusted p-values or plot hte
tree with p-values adjusted according to the Hierarchical FDR
controlling procedure. If a particular node hypothesis was not tested, it
  is colored grey. If it was tested and rejected, it is green; if it was
  tested and not rejected, it is shaded blue. The deeper the shade, the
  lower (more significant) the p-value was.

}

}
}

\keyword{methods}