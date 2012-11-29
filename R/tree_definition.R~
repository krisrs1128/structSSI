## This defines a class of hypothesis trees. It should
## allow us to manipulate the hypotheses, the results of
## tests associated with them, and their hierarchical
## relation. We set a class to make this manpiulation
## easier and to prevent the bugs that arise from complexity.
## Note that the tree is defined by its adjacency matrix.

setClass("hypothesesTree", representation = list(tree = "matrix",
                           unadj.p.values = "numeric",
                           adj.p.values = "numeric",
                           hypotheses.names = "character",
                           rejected.hypotheses = "character"
                           ))

setMethod("initialize", "hypothesesTree", function(.Object, ...)
      {
          value <- callNextMethod()
          validObject(value)
          value
      })

setMethod("show", "hypothesesTree", function(object)
      {
          hypothesesNames <- slot(object, "hypotheses.names")
          tree <- slot(object, "tree")
          unadj.p.values <- slot(object, "unadj.p.values")
          adj.p.values <- slot(object, "adj.p.values")
          rejected.hypotheses <- slot(object, "rejected.hypotheses")
          require(igraph)
          nHypotheses <- length(hypothesesNames)
          igraph.tree <- graph.adjacency(tree)
          nEdges <- length(E(igraph.tree))
          cat("Hypotheses Tree", '\n')
          if(length(rejected.hypotheses) == 0 || length(adj.p.values) == 0){
              print(list(nHypotheses = nHypotheses, nTreeEdges = nEdges,
                         hypotheses.names = hypothesesNames,
                         unadj.p.values = unadj.p.values))
          } else {
              print(list(nHypotheses = nHypotheses, nTreeEdges = nEdges,
                         hypotheses.names = hypothesesNames,
                         unadj.p.values = unadj.p.values,
                         adj.p.values = adj.p.values,
                         rejected.hypotheses = rejected.hypotheses))
          }
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
