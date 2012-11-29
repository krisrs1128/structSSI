## We check to see that the tree that is being defined is valid.
## I.e., we check that the number of p-values, nodes in the tree,
## and hypotheses under consideration are consistent.

validHypothesesTree <- function(object){
    unadj.p.values <- slot(object, "unadj.p.values")
    hypotheses.names <- slot(object, "hypotheses.names")
    tree <- slot(object, "tree")
    if(length(unadj.p.values) != length(hypotheses.names)){
        return("Number of hypotheses in '@hypotheses.names' does not match the number of unadjusted p-values in 'unadj.p.values'.")
    }
    if(nrow(tree) != ncol(tree)){
        return("Adjacency matrix for '@tree' is invalid, does not have equal number of rows and columns.")
    }
    if(length(hypotheses.names) != nrow(tree)){
        return("Number of hypotheses in '@hypotheses.names' does not match number of nodes in hypotheses tree in '@tree'")
    }
    TRUE
}

