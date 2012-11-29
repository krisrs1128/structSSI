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
    A phyloseq class phylogenetic tree containing the phylogenetic
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
## Load data from the phyloseq package
library(phyloseq)
data(GlobalPatterns)
GP.chl <- subset_taxa(GlobalPatterns, Phylum == "Chlamydiae")
abundances <- otu_table(GP.chl)
environments <- sample_data(GP.chl)$SampleType

GPChltree <- get.adjacency(as.igraph(phy_tree(GP.chl)))
ChlPVal <- treePValues(GPChltree, abundances, environments)
}