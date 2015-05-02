\name{treePValues}
\alias{treePValues}

\title{
Create tree of p-values for phyloseq data
}

\description{
  This helper function is used to aggregate abundances of individual
  microbes to higher levels in the tree and test whether those
  aggregated abundances are significantly different between
  environments, using the data structures from the phyloseq package framework.
}

\usage{
treePValues(tree, abundances, environments)
}

\arguments{
  \item{tree}{
    An edgelist for a tree containing the phylogenetic
    relationships between different microbes.
  }
  \item{abundances}{
    A phyloseq class OTU table specifying the abundances of different
    microbes across environments.
  }
  \item{environments}{
    A phyloseq class Sample Data object associating the different
    environments with variables of interest.
  }
}

\value{
  A vector containing the p-values of the linear model predicting
  the abundances of microbes aggregated to different levels in the
  taxonomy from environmental variables.
}

\author{
  Kris Sankaran
}

\examples{
library('igraph')

# Example with random data
if(require('ape')) {
  rand.tree <- as.igraph(rtree(50))
  V(rand.tree)$name <- paste("hyp", 1:50)
  rand.tree <- get.edgelist(rand.tree)
  X <- matrix(rnorm(50 * 4), 50 , 4)
  rownames(X)  <- paste("hyp" , 1:50)
  colnames(X)  <- 1:4
  X[, 1:2] <- X[, 1:2] + 1
  groups <- factor(c("A", "A", "B", "B"))
  treePValues(rand.tree, X, groups)
}

# Example using phyloseq
if(require('ape') & require('phyloseq')) {
    data(chlamydiae)
    abundances <- otu_table(chlamydiae)
    environments <- sample_data(chlamydiae)$SampleType
    ch.tree <- get.edgelist(as.igraph(phy_tree(chlamydiae)))
    ch.pval <- treePValues(ch.tree, abundances, environments)
}


}