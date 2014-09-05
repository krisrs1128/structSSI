\name{structSSI-package}
\alias{structSSI-package}
\alias{structSSI}
\docType{package}
\title{

  Structured Simultaneous and Selective Inference for Grouped or
  Hierarchically Structured Data

}

\description{

  Performs multiple corrections that take specific structure of hypotheses
  into account. The two procedures implemented are the Hierarchically
  FDR controlling procedure of Benjamini and Yekutieli and the Group
  Benjamini-Hochberg procedure of Hu, Zhou, and Zhao. The methods are
  applicable whenever information about hierarchical or group relationship
  between hypotheses can be ascertained before any data analysis.

}

\details{
  \tabular{ll}{
    Package: \tab structSSI\cr
    Type: \tab Package\cr
    Version: \tab 1.0\cr
    Date: \tab 2012-03-14\cr
    License: \tab GPL-2 \cr
  }

  This package implements two recently developed techniques in the field
  of selective and simultaneous inference (SSI). The first method is the
  Adaptive Groups Benjamini-Hochberg procedure of Hu, Zhou, and Zhao
  2011. The second is the Hierarchical FDR Controlling Procedure of
  Benjamini and Yekutieli. Both methods attempt to employ apriori known
  information about the relationships between hypotheses in testing them
  and correcting for the  multiple testing problem. These
  methods have been employed in genetics, QTL analysis, and clinical
  trials; more deatils about these applications can be read in the
  references stated below.

  The Group Benjamini-Hochberg procedure is implemented in its adaptive
  and oracle varieties through the functions \code{Adaptive.GBH} and
  \code{Oracle.GBH}, respectively. The Hierarchical Procedure is
  implemented in the function \code{hFDR.adjust} and uses the class
  \code{hypothesesTree} to organize the information required for the
  procedure. These functions describe the procedures in more
  detail. Further, the references listed below present the derivations
  and applications of these two procedures.

}

\author{

  Kris Sankaran

  Maintainer: kriss1@stanford.edu

}

\references{

  Benjamini, Y, and Yekutieli, D. Hierarchical fdr testing of trees of
  hypotheses. 2002.

  Hu, J.X., Zhao, H., and Zhou, H.H. False discovery rate control with
  groups. Journal of the American Statistical Association, volume 104,
  number 491. Pages 1215-1227. 2010.

  Sankaran, K and Holmes, S. structSSI: Simultaneous and Selective
  Inference for Grouped or Hierarchically Structured Data. Journal of
  Statistical Software, 59(13), 1-21. 2014. http://jstatsoft.org/v59/i13/

  Yekutieli, D. Hierarchical false discovery rate-controlling
  methodology. Journal of the American Statistical Association,
  103(481):309-316, 2008.

}

\keyword{ package }
\keyword{ htest }

\examples{

## Example of using the Adaptive Benjamini-Hochberg Procedure.

set.seed(2249)
unadjp <- c(runif(40, 0, 0.001), runif(30, 0, 0.1), runif(130, 0, 1))
names(unadjp) <- paste("hyp", c(1:200))
groups <- c(sample(1:3, 200, replace = TRUE))
result <- Adaptive.GBH(unadjp, groups, method = "lsl", alpha = 0.05)

## Example of using the Hierarchical FDR controlling procedure.
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