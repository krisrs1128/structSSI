
#" @title Estimate FDR control variations for HFDR procedure
#"
#" @description This function estimates two types of HFDR control appropriate
#"   for trees of hypotheses. If the BH procedure is applied at level alpha
#"   within each of the tree families, this is defined as
#"
#    \deqn{FDR.est := \alpha * [\#\textit{discoveries} + \#\textit{families tested}] / [\textit{\#discoveries} + 1]}
#" where a discovery is defined as an adjusted p value below alpha within the
#" entire tree or at the tips for tree and tips FDR, respectively.
#" @param hyp.tree An object of class \code{HypothesesTree}, such as the result
#"   to a call of \code{hFDR.adjust}.
#" @return A list with the following elements,
#"   \item{tree}{The estimated full-tree FDR.}
#"   \item{tip}{The estimated outer-nodes FDR.}
#"   \item{n.families.tested}{The number of families of hypotheses
#"   tested by the HFDR procedure.}
#"   \item{n.tree.discoveries}{The number of discoveries over the whole tree.}
#"   \item{n.tip.discoveries}{The number of discoveries among the tree tips.}
#" @details
#" Yekutieli, D. Hierarchical false discovery rate-controlling methodology.
#"   Journal of the American Statistical Association, 103(481):309-316, 2008.
#" Benjamini, Y, and Yekutieli, D. Hierarchical fdr testing of trees of
#"   hypotheses. 2002.
#" Sankaran, K and Holmes, S. structSSI: Simultaneous and Selective Inference
#"   for Grouped or Hierarchically Structured Data. Journal of Statistical
#"   Software, 59(13), 1-21. 2014. http://jstatsoft.org/v59/i13/
#" @export
#" @examples
#" library("igraph")
#" library("ape")
#"
#" alternative.indices <- sample(1:49, 30)
#" unadj.p.values <- vector("numeric", length = 49)
#" unadj.p.values[alternative.indices] <- runif(30, 0, 0.01)
#" unadj.p.values[-alternative.indices] <- runif(19, 0, 1)
#" unadj.p.values[c(1:5)] <- runif(5, 0, 0.01)
#" names(unadj.p.values) <- paste("Hyp ", c(1:49))
#"
#" tree <- as.igraph(rtree(25))
#" V(tree)$name <- names(unadj.p.values)
#" tree.el <- get.edgelist(tree)
#"
#" hyp.tree <- hFDR.adjust(unadj.p.values, tree.el, 0.05)
#"
#" EstimatedHFDRControl(hyp.tree)
EstimatedHFDRControl <- function(hyp.tree) {
  alpha <- hyp.tree@alpha
  parent.ix <- hyp.tree@p.vals$hypothesis %in% hyp.tree@tree[, 1]
  n.families.tested <- sum(hyp.tree@p.vals[parent.ix, "adjp"] < alpha, na.rm = T)
  n.tree.discoveries <- sum(hyp.tree@p.vals[, "adjp"] < alpha, na.rm = T)
  n.tip.discoveries <- sum(hyp.tree@p.vals[!parent.ix, "adjp"] < alpha, na.rm = T)

  fdr.tree.est <- min(1, (n.tree.discoveries + n.families.tested) / (n.tree.discoveries + 1) * alpha)
  fdr.tip.est <-  min(1, (n.tip.discoveries + n.families.tested) / (n.tip.discoveries + 1) * alpha)

  list(
    tree = fdr.tree.est,
    tip = fdr.tip.est,
    n.families.tested = n.families.tested,
    n.tree.discoveries = n.tree.discoveries,
    n.tip.discoveries = n.tip.discoveries
  )
}

#" @title FDR Controlling Procedure for Hierarchically Structured Hypotheses
#" @description This function implements the Hierarchical FDR controlling
#"   procedure developed Benjamini and Yekutieli. The procedure incorporates
#"   structural information about the hierarchical relationships between
#"   hypotheses in order to increase power and interpretability of a testing
#"   procedure while controlling the False Discovery Rate at a prespecified
#"   level. It is applicable whenever we there is a natural hierarchical
#"   structure between the hypotheses being tested before the data analysis
#"   begins. For example, the method has been used before in Clinical Studies,
#"   where nodes deeper in the tree correspond to secondary or tertiary
#"   endpoints. It has also been used in QTL analysis, where we first make
#"   statements about regions of chromosomes being associated with specific
#"   brain activity and then focus on more and more detailed subsets of
#"   chromosomes.
#" @param unadjp A vector of raw p-values resulting from an experiment. The
#"   names of this vector should be contained in the edgelist parameterizing the
#"   hierarchical structure between hypothesis, inputted as \code{tree.el}.
#" @param tree.el The edgelist parameterizing the hierarchical structure between
#"   hypotheses. The edges must be stored so that each edge is a row of a two
#"   column matrix, where the first column gives the parent and the second gives
#"   the child.
#" @param alpha The level of FDR control within families of the tree. Note that
#"   this is NOT necessarily the level of FDR control within the entire tree.
#"   Refer to the paper by Yekutieli and Benjamini for bounds on various FDR
#"   criterion.
#" @details The FDR controlling procedure is described in more detail in the
#"   paper by Yekutieli and Benjamini 2009. The idea is to control for multiple
#"   testing error within families of hypotheses, and only test a descendant
#"   family of hypotheses if the associated parent hypotheses was deemed
#"   significant in the higher level. The families of hypotheses are taken to be
#"   the children of any particular node, and error is controlled within these
#"   families using the Benjamini-Hochberg procedure. Different bounds can be
#"   proven for the FDR when considered along whole tree, within a single level,
#"   and tips. In particular, the whole tree FDR is typically controlled at
#"   three times the FDR control within individual families, and this result
#"   holds for arbitrary hypotheses tests and configurations of trees.
#"
#"   Yekutieli, D. Hierarchical false discovery rate-controlling methodology.
#"   Journal of the American Statistical Association, 103(481):309-316, 2008.
#"
#"   Benjamini, Y, and Yekutieli, D. Hierarchical fdr testing of trees of
#"   hypotheses. 2002.
#"
#"   Sankaran, K and Holmes, S. structSSI: Simultaneous and Selective Inference
#"   for Grouped or Hierarchically Structured Data. Journal of Statistical
#"   Software, 59(13), 1-21. 2014. http://jstatsoft.org/v59/i13/
#"
#" @importFrom multtest mt.rawp2adjp
#" @export
#" @examples
#" library("igraph")
#" library("ape")
#"
#" alternative.indices <- sample(1:49, 30)
#" unadj.p.values <- vector("numeric", length = 49)
#" unadj.p.values[alternative.indices] <- runif(30, 0, 0.01)
#" unadj.p.values[-alternative.indices] <- runif(19, 0, 1)
#" unadj.p.values[c(1:5)] <- runif(5, 0, 0.01)
#" names(unadj.p.values) <- paste("Hyp ", c(1:49))
#"
#" tree <- as.igraph(rtree(25))
#" V(tree)$name <- names(unadj.p.values)
#" tree.el <- get.edgelist(tree)
#"
#" hyp.tree <- hFDR.adjust(unadj.p.values, tree.el, 0.05)
#"
#" # We can visualize the difference between the unadjusted and the adjusted
#" # trees.
#" plot(hyp.tree, adjust = FALSE)
#" plot(hyp.tree, adjust = TRUE)
hFDR.adjust <- function(unadjp, tree.el, alpha = 0.05) {
  # If user does not name unadjusted p-values or tree nodes, assume i^th element
  # of unadjp corresponds to the i^th row / column of tree.
  if(is.null(names(unadjp))) {
    names(unadjp) <- seq_along(unadjp)
  }
  if(!all(names(unadjp) %in% unique(as.vector(tree.el)))) {
    stop("Names of elements in unadjp do not match names of tree nodes")
  }

  p.vals <- data.frame(unadjp, adjp = NA)
  hyp.tree.unadjusted <- new("hypothesesTree", tree = tree.el, p.vals = p.vals, alpha = alpha)

  # Check to see if possible to descend from the root node (if not significant
  # reject no hypotheses).
  root <- FindRoot(hyp.tree.unadjusted@tree)
  if(hyp.tree.unadjusted@p.vals[root, "unadjp"] > alpha){
    warning("Root hypothesis p-value equal to ", hyp.tree.unadjusted@p.vals[root, "unadjp"], ". Fail to reject any hypotheses, terminating procedure.")
    hyp.tree.unadjusted@p.vals[root, "adjp"] <- hyp.tree.unadjusted@p.vals[root, "unadjp"]
    hyp.tree.unadjusted@p.vals <- format_pvals(hyp.tree.unadjusted@p.vals, alpha)
    return(hyp.tree.unadjusted)
  }

  # Perform correction, and format output
  hyp.tree <- hFDR.internal(hyp.tree.unadjusted)
  hyp.tree@p.vals[root, "adjp"] <- hyp.tree@p.vals[root, "unadjp"]
  hyp.tree@p.vals <- format_pvals(hyp.tree@p.vals, alpha)

  hyp.tree
}

format_pvals <- function(p.vals, alpha) {
  p.vals[, "significance"] <- SignificanceStars(alpha, p.vals[, "adjp"])
  p.vals[, "hypothesis"] <- row.names(p.vals)
  p.vals[, "original_index"] <- seq_len(nrow(p.vals))

  row.names(p.vals) <- NULL
  col_order <- c("hypothesis", "original_index", "unadjp", "adjp", "significance")
  p.vals[, col_order]
}

hFDR.internal <- function(hyp.tree) {
  tree <- graph.edgelist(hyp.tree@tree)
  tree.el.tmp <- data.frame(
    "parent" = hyp.tree@tree[, 1],
    "child" = hyp.tree@tree[, 2],
    stringsAsFactors = F
  )

  root <- FindRoot(tree.el.tmp)
  children <- tree.el.tmp[which(tree.el.tmp$parent == root), "child"]
  children.p.vals <- hyp.tree@p.vals[children, ]
  adjust <- mt.rawp2adjp(children.p.vals$unadjp, "BH")
  children.p.vals$adjp <- adjust$adjp[order(adjust$index), "BH"]
  hyp.tree@p.vals[children, "adjp"] <- children.p.vals$adjp

  rejected <- children.p.vals[which(children.p.vals$adjp < hyp.tree@alpha), "hypothesis"]
  for(child in rejected){
    subcomp <- subcomponent(tree, child, "out")
    if(length(subcomp) > 1){
      subtree.igraph <- induced.subgraph(graph = tree, vids = subcomp)
      subtree.names <- get.vertex.attribute(subtree.igraph, "name")
      subtree <- new("hypothesesTree", alpha = hyp.tree@alpha,
                     tree = get.edgelist(subtree.igraph))
      subtree@p.vals <- hyp.tree@p.vals[subtree.names, ]
      hyp.tree@p.vals[subtree.names, ] <- hFDR.internal(subtree)@p.vals
    }
  }

  hyp.tree
}

#" @title Create tree of p-values for phyloseq data
#" @description This helper function is used to aggregate abundances of
#"   individual microbes to higher levels in the tree and test whether those
#"   aggregated abundances are significantly different between environments,
#"   using the data structures from the phyloseq package framework.
#" @param tree An edgelist for a tree containing the phylogenetic relationships
#"   between different microbes.
#" @param abundances A phyloseq class OTU table specifying the abundances of
#"   different microbes across environments.
#" @param environments A phyloseq class Sample Data object associating the
#"   different environments with variables of interest.
#" @return A vector containing the p-values of the linear model predicting the
#"   abundances of microbes aggregated to different levels in the taxonomy from
#"   environmental variables.
#" @export
#" @import igraph
#" @importFrom stats lm pf
#" @examples
#" library("igraph")
#"
#" # Example with random data
#" if(require("ape")) {
#"   rand.tree <- as.igraph(rtree(50))
#"   V(rand.tree)$name <- paste("hyp", 1:50)
#"   rand.tree <- get.edgelist(rand.tree)
#"   X <- matrix(rnorm(50 * 4), 50 , 4)
#"   rownames(X)  <- paste("hyp" , 1:50)
#"   colnames(X)  <- 1:4
#"   X[, 1:2] <- X[, 1:2] + 1
#"   groups <- factor(c("A", "A", "B", "B"))
#"   treePValues(rand.tree, X, groups)
#" }
#"
#" # Example using phyloseq
#" if(require("ape") & require("phyloseq")) {
#"   data(chlamydiae)
#"   abundances <- otu_table(chlamydiae)
#"   environments <- sample_data(chlamydiae)$SampleType
#"   ch.tree <- get.edgelist(as.igraph(phy_tree(chlamydiae)))
#"   ch.pval <- treePValues(ch.tree, abundances, environments)
#" }
treePValues <- function(tree, abundances, environments){
  igraphTree <- graph.edgelist(tree)
  treePValues <- vector(length = length(V(igraphTree)))
  names(treePValues) <- V(igraphTree)$name

  for(vertex in V(igraphTree)){

    # first, aggregate data descending from that node
    graphDiameter <- diameter(igraphTree)
    curVertexName <- V(igraphTree)[vertex]$name
    descendants <- neighborhood(igraphTree, nodes = curVertexName, mode = "out", order = graphDiameter)
    subtree <- induced.subgraph(igraphTree, vids = descendants[[1]])

    # some nodes don"t have descendants that are in the OTU table, so we can"t
    # consider them in the p-value calculations.
    allDescendantsNames <- V(subtree)$name
    namesInTipsIndex <- which(allDescendantsNames %in% rownames(abundances))

    if(length(namesInTipsIndex) > 0){
      subOtuTable <- abundances[V(subtree)$name[namesInTipsIndex], , drop=F]
      aggregateData <- colSums(subOtuTable)
      aggregateDataWithLabel <- data.frame(
        abund = as.vector(aggregateData),
        type = environments
      )

      # fit the LM to obtain p-values
      abundanceModel <- summary(lm(abund ~ type, data = aggregateDataWithLabel))
      treePValues[curVertexName] <- pf(
        abundanceModel$fstatistic[1],
        abundanceModel$fstatistic[2],
        abundanceModel$fstatistic[3],
        lower.tail = FALSE
      )
    }
  }
  return(treePValues)
}

ListTreePval <- function(tree) {
  # Given an igraph tree, returns format for d3Network
  # Assumes tree has V(tree)$names equal to names we want
  # Also assumes tree has p-values associated with it
  if (length(V(tree)) == 1) {
    return(list(name = V(tree)$name, pval = V(tree)$pval))
  } else {
    parent <- V(tree)[1]$name
    cur.pval <- V(tree)[1]$pval
    edgelist <- get.edgelist(tree, names = FALSE)
    root.position.in.edgelist <- which(edgelist[,1] == 1)
    children <- edgelist[root.position.in.edgelist, 2]
    children.list <- list()

    for(i in seq_along(children)) {
      subcomp.indices <- subcomponent(tree, children[i], "out")
      subgraph <- induced.subgraph(graph = tree, vids = subcomp.indices)
      children.list[[i]] <- ListTreePval(
        induced.subgraph(graph = tree, vids = subcomp.indices)
      )
    }
    result <- list(name = parent, pval = cur.pval, children = children.list)
  }
  return (result)
}

FindRoot <- function(tree.el) {
  tree.el.tmp <- tree.el
  colnames(tree.el.tmp) <- c("parent", "child")
  root <- unique(tree.el.tmp[which(!(tree.el.tmp[, 1] %in% tree.el.tmp[, 2])), 1]) # no edge leads to root
  return(root = root)
}
