## This returns a list with two elements. The first
## is a vector of the hypotheses that have been rejected
## by the multiple testing routine. The second is a
## vector of all the p-values of the hypotheses that
## have been tested.


hFDR.adjust <- function(hyp.tree, alpha = 0.05){

    require(igraph0)
    require(multtest)

    if(hyp.tree@unadj.p.values[1] > alpha){
        warning("Root hypothesis p-value equal to ", hyp.tree@unadj.p.values[1], ".
Fail to reject, terminating procedure.")
        return(list(rejected.hypotheses = NA, adjp.values = hyp.tree@unadj.p.values[1]))
    }

    tree <- graph.adjacency(hyp.tree@tree)
    V(tree)$hyp.names <- hyp.tree@hypotheses.names
    V(tree)$p.vals <- hyp.tree@unadj.p.values
    edgelist <- get.edgelist(tree, names = FALSE)
    root.position.in.edgelist <- which(edgelist[,1] == 0)
    children <- edgelist[root.position.in.edgelist, 2]

    # get the corresponding p-values.

    children.p.vals <- hyp.tree@unadj.p.values[children + 1] # corrects for igraph 1 indexing
    adjust <- mt.rawp2adjp(children.p.vals, "BH")
    adjp <- adjust$adjp
    adjp.index <- adjust$index

    rejected.proc <- mt.reject(adjp, alpha)
    rejected.children <- adjp.index[which(rejected.proc$which[,2])]
    rejected <- children[rejected.children]

    # add the running list of adjusted p.values

    adjusted.p.values <- adjp[, 2]
    adjusted.p.values <- adjusted.p.values[adjp.index]
    names(adjusted.p.values) <- hyp.tree@hypotheses.names[children + 1]

    # Now, we test only those hypotheses descending from those children
    # that were just rejected according ato the simultaneous comparison
    # in the family.

    # We can do this by finding all the subtrees descending from the
    # hypotheses that were rejected and testing those as a family.

    for(child in rejected){

        subcomp <- subcomponent(tree, child, "out")

        if(length(subcomp) > 1){
            subtree <- new("hypothesesTree")
            subtree.igraph <- subgraph(tree, subcomp)
            subtree@tree <- get.adjacency(subtree.igraph)
            subtree@hypotheses.names <- V(subtree.igraph)$hyp.names

            subtree@unadj.p.values <- V(subtree.igraph)$p.vals
            new.adjp <- hFDR.adjust(subtree, alpha)$adjp.values
            adjusted.p.values <- c(adjusted.p.values, new.adjp)
        }

    }

    rejected.hyp <- names(adjusted.p.values[which(adjusted.p.values <= alpha)])

    return(list(rejected.hypotheses = rejected.hyp, adjp.values = adjusted.p.values))
}
