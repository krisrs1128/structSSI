#' Chlamydiae Sub-Taxa of Global Patterns Data
#'
#' A \code{phyloseq} class object containing
#'  data on 16s rRNA diversity within the Chlamydiae
#'  bacteria taxon, originally appearing in PNAS.
#'
#' A small subtree of the GlobalPatterns dataset, originaly available in the
#' phyloseq package.
#' @import phyloseq
#' @source \url{https://github.com/joey711/phyloseq/raw/c2605619682acb7167487f703d135d275fead748/data/GlobalPatterns.RData}
#' @references 
#' McMurdie P.J., Holmes S. (2013). phyloseq: A Bioconductor Package for
#' Handling and Analysis of High-Throughput Phylogenetic
#' Sequence Data. PLoS ONE 8(4): e61217. \url{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0061217}.
#'
#' Caporaso, J. G., et al. (2011). Global patterns of 16S rRNA
#' diversity at a depth of millions of sequences per sample. PNAS,
#' 108, 4516-4522. PMCID: PMC3063599
#'
#' This can be viewed/downloaded at:
#' \url{https://www.pnas.org/content/108/suppl.1/4516.short}
#
#' Sankaran, K and Holmes, S. structSSI: Simultaneous and Selective
#' Inference for Grouped or Hierarchically Structured Data. Journal of
#' Statistical Software, 59(13), 1-21. 2014. https://jstatsoft.org/v59/i13/
#' @examples
#' if (requireNamespace("phyloseq", quietly = TRUE)) {
#'  data(chlamydiae)
#' }
#' @name chlamydiae
#' @docType data
#' @usage data(chlamydiae)
#' @format A phyloseq object
NULL