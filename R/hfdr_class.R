#' This defines a class of hypothesis trees. It should allow us to manipulate
#' the hypotheses, the results of tests associated with them, and their
#' hierarchical relation. We set a class to make this manpiulation easier and to
#' prevent the bugs that arise from complexity. Note that the tree is defined by
#' its adjacency matrix.
#'
#' @title hypothesesTree class
#' @slot tree Object of class \code{"matrix"}. The edgelist for the hypotheses
#'   tree.
#' @slot p.vals Object of class \code{"data.frame"}. Each row correpsonds to an
#'   individual hypothesis. The first column stores the p-values before GBH
#'   adjustment, while the second gives the hFDR adjusted p-values. The
#'   hypotheses are sorted in order of significance according to these GBH
#'   adjusted p-values. The \code{group} column gives the group membership of
#'   each hypothesis, and \code{adj.significnace} codes the significance of each
#'   hypothesis, according to the GBH adjusted p-values.
#' @slot alpha: Object of class \code{"numeric"}. The level at which the FDR is
#'   controlled among children of each parent node.
#'
#' @rdname hypothesisTree-methods
#' @export
#' @examples
#' library('igraph')
#' library('ape')
#'
#' alternative.indices <- sample(1:49, 30)
#' unadj.p.values <- vector("numeric", length = 49)
#' unadj.p.values[alternative.indices] <- runif(30, 0, 0.01)
#' unadj.p.values[-alternative.indices] <- runif(19, 0, 1)
#' unadj.p.values[c(1:5)] <- runif(5, 0, 0.01)
#' names(unadj.p.values) <- paste("Hyp ", c(1:49))
#'
#' tree <- as.igraph(rtree(25))
#' V(tree)$name <- names(unadj.p.values)
#' tree.el <- get.edgelist(tree)
#'
#' hyp.tree <- hFDR.adjust(unadj.p.values, tree.el, 0.05)
#' plot(hyp.tree)
setClass(
  "hypothesesTree",
  representation = list(
    tree = "matrix",
    p.vals = "data.frame",
    alpha = "numeric")
)

#' @description Check that the hypotheses tree is correctly initialized. It
#'   ensures that the number of unadjusted p-values, hypotheses names, and nodes
#'   in the hypotheses tree all agree. It also checks that the hypotheses tree
#'   is in fact a tree.
#' @rdname hypothesisTree-methods
#' @export
setMethod("initialize", "hypothesesTree", function(.Object, ...) {
  value <- callNextMethod()
  value
})

#' @description This prints the unadjusted and adjusted p-values of the
#'   hypotheses tree associated with the HFDR procedure.
#' @rdname hypothesisTree-methods
#' @export
setMethod("show", "hypothesesTree", function(object) {
  tree <- slot(object, "tree")
  p.vals <- slot(object, "p.vals")
  alpha <- slot(object, "alpha")

  cat("hFDR adjusted p-values:", "\n")
  print(p.vals)
  cat('---', '\n')
  cat('Signif. codes:  0 \'***\'', alpha / 50, '\'**\'', alpha / 5, '\'*\'', alpha, '\'.\'', 2 * alpha, '\'-\' 1', '\n')
})

#' @description This prints the most significant adjusted p-values, along with
#'   estimates of the FDR across the tree and at tips.
#' @rdname hypothesisTree-methods
#' @export
setMethod("summary", "hypothesesTree", function(object) {
  tree <- slot(object, "tree")
  p.vals <- slot(object, "p.vals")
  alpha <- slot(object, "alpha")

  igraph.tree <- graph.edgelist(tree)
  nHypotheses <- length(V(igraph.tree))
  nEdges <- length(E(igraph.tree))
  cat("Number of hypotheses:", nHypotheses, "\n")
  FDR.control <- EstimatedHFDRControl(object)
  cat("Number of tree discoveries:", FDR.control$n.tree.discoveries, "\n")
  cat("Estimated tree FDR:", FDR.control$tree, "\n")
  cat("Number of tip discoveries:", FDR.control$n.tip.discoveries, "\n")
  cat("Estimated tips FDR:", FDR.control$tip, "\n")
  cat("\n", "hFDR adjusted p-values:", "\n")
  n.to.print <- min(nrow(p.vals), 10)
  print(p.vals[order(p.vals$adjp)[1:n.to.print], ])
  if(n.to.print < nrow(object@p.vals)) {
    cat('[only 10 most significant hypotheses shown]', '\n')
  }
  cat('---', '\n')
  cat('Signif. codes:  0 \'***\'', alpha / 50, '\'**\'',
      alpha / 5, '\'*\'', alpha, '\'.\'',
      2 * alpha, '\'-\' 1', '\n')
})

#' @description This prints the most significant adjusted p-values, along with
#'   estimates of the FDR across the tree and at tips.
#' @rdname hypothesisTree-methods
#' @export
setMethod("plot", "hypothesesTree",
          function(x,..., adjust = TRUE,
                   return_script = FALSE, width = 900,
                   height = 500, base_font_size = 12,
                   output_file_name = paste('hyp_tree', gsub("[^\\d]+", "", Sys.time(), perl=TRUE), '.html', sep = "")) {
            PlotHypTree(x, adjust, return_script, width,
                        height, base_font_size, output_file_name)
          })

validHypothesesTree <- function(object){
  p.vals <- slot(object, "p.vals")
  tree <- slot(object, "tree")
  if(ncol(tree) != 2){
    return("Edgelist for '@tree' is invalid, does not two columns (parent -> child).")
  }
  if(nrow(p.vals) != nrow(tree)){
    return("Number of hypotheses in '@p.vals' does not match number of nodes in hypotheses tree in '@tree'")
  }
  TRUE
}
