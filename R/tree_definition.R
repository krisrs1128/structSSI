## This defines a class of hypothesis trees. It should
## allow us to manipulate the hypotheses, the results of
## tests associated with them, and their hierarchical
## relation. We set a class to make this manpiulation
## easier and to prevent the bugs that arise from complexity.
## Note that the tree is defined by its adjacency matrix.
setClass("hypothesesTree", representation = list(tree = "matrix",
                               p.vals = "data.frame",
                               alpha = "numeric"))

setMethod("initialize", "hypothesesTree", function(.Object, ...)
      {
          value <- callNextMethod()
          validObject(value)
          value
      })

setMethod("show", "hypothesesTree", function(object)
      {
          tree <- slot(object, "tree")
          p.vals <- slot(object, "p.vals")
          alpha <- slot(object, "alpha")
          nHypotheses <- nrow(tree)
          igraph.tree <- graph.adjacency(tree)
          nEdges <- length(E(igraph.tree))
          cat("\n", "Number of hypotheses:", "\n")
          print(nHypotheses)
          cat("\n", "Number of tree edges: ", "\n")
          print(nEdges)
          cat("\n", "hFDR adjusted p-values:", "\n")
          print(p.vals)
          cat('Signif. codes:  0 \'***\'', alpha / 50, '\'**\'', alpha / 5, '\'*\'', alpha, '\'.\'', 2 * alpha, '\'-\' 1', '\n')          
      })


setMethod("plot", "hypothesesTree", function(x,..., p.values.type = "unadjusted", alpha = 0.05)
      {
          p.values.type <- tolower(p.values.type)
          p.values.type <- match.arg(p.values.type, c("unadjusted", "adjusted"))
          if(p.values.type == "unadjusted"){
            plotUnadjustedHypothesesTree(x, alpha)
          } else if(p.values.type == "adjusted"){
            plotAdjustedHypothesesTree(x)
          }
        })
