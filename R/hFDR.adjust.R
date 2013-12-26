hFDR.adjust <- function(unadjp, tree, alpha = 0.05) {
    # If user does not name unadjusted p-values or tree nodes,
    # assume i^th element of unadjp corresponds to the i^th
    # row / column of tree.    # 
    if(is.null(names(unadjp))) {
        names(unadjp) <- 1:length(unadjp)
    }
    if(is.null(rownames(tree)) & is.null(colnames(tree))) {
        rownames(tree) <- 1:nrow(tree)
        colnames(tree) <- 1:ncol(tree)
    }
    if(any(!(names(unadjp) %in% rownames(tree))) ||
       any(!(names(unadjp) %in% rownames(tree)))) {
        stop("Names of elements in unadjp do not match names of tree nodes")
    }

    p.vals <- data.frame(unadjp, adjp = NA)
    hyp.tree.unadjusted <- new("hypothesesTree", tree = tree,
                               p.vals = p.vals, alpha = alpha)

    hFDR.internal <- function(hyp.tree) {
        tree <- graph.adjacency(hyp.tree@tree)
        el.root <- BuildEdgelist(hyp.tree)
        edgelist <- el.root$el
        root <- el.root$root

        children <- subset(edgelist, subset = parent == root)$child
        children.p.vals <- hyp.tree@p.vals[children, ]
        adjust <- mt.rawp2adjp(children.p.vals$unadjp, 'BH')
        children.p.vals$adjp <- adjust$adjp[adjust$index, 'BH']
        hyp.tree@p.vals[children, 'adjp'] <- children.p.vals$adjp

        alpha <- hyp.tree@alpha
        rejected <- rownames(children.p.vals)[which(children.p.vals$adjp < alpha)]

        for(child in rejected){
            subcomp <- subcomponent(tree, child, "out")
            if(length(subcomp) > 1){
                subtree <- new("hypothesesTree")
                subtree@alpha <- alpha
                subtree.igraph <- induced.subgraph(graph = tree, vids = subcomp)
                subtree.names <- get.vertex.attribute(subtree.igraph, 'name')
                subtree@tree <- as.matrix(get.adjacency(subtree.igraph))
                subtree@p.vals <- hyp.tree@p.vals[subtree.names, ]
                hyp.tree@p.vals[subtree.names, ] <- hFDR.internal(subtree)@p.vals
            }
        }
        return(hyp.tree)
    }

    # Check to see if possible to descend from the root node (if not significant
    # reject no hypotheses).
    root <- BuildEdgelist(hyp.tree.unadjusted)$root
    if(hyp.tree.unadjusted@p.vals[root, 'unadjp'] > alpha){
        warning("Root hypothesis p-value equal to ", hyp.tree@p.vals[root, 'unadjp'], ". Fail to reject any hypotheses, terminating procedure.")
        hyp.tree.unadjusted@p.vals[, 'adj.significance'] <- '-'
        return(hyp.tree.unadjusted)
    }

    # Perform correction, and format output
    hyp.tree <- hFDR.internal(hyp.tree.unadjusted)
    hyp.tree@p.vals[root, 'adjp'] <- hyp.tree@p.vals[root, 'unadjp']
    hyp.tree@p.vals[, 'adj.significance'] <- SignificanceStars(alpha, hyp.tree@p.vals[, 'adjp'])
    return(hyp.tree)
}






hFDR.adjust <- function(hyp.tree, alpha = 0.05){
    tree <- graph.adjacency(hyp.tree@tree)
    V(tree)$hyp.names <- rownames(hyp.tree@p.vals)
    V(tree)$p.vals <- hyp.tree@p.vals[, 1]
    edgelist <- get.edgelist(tree, names = FALSE)
    root.position.in.edgelist <- which(edgelist[,1] == 1)
    children <- edgelist[root.position.in.edgelist, 2]

    root <- unique(edgelist[root.position.in.edgelist, 1])
    if(hyp.tree@p.vals[V(tree)[root], 1] > alpha){
        warning("Root hypothesis p-value equal to ", hyp.tree@unadj.p.values[1], ".
Fail to reject, terminating procedure.")
        return(list(rejected.hypotheses = NA, adjp.values = hyp.tree@unadj.p.values[1]))
    }

                                        # get the corresponding p-values.
    children.p.vals <- hyp.tree@p.vals[children, 1]
    adjust <- mt.rawp2adjp(children.p.vals, "BH")
    adjp <- adjust$adjp
    adjp.index <- adjust$index



    rejected.proc <- mt.reject(adjp, alpha)
    rejected.children <- adjp.index[which(rejected.proc$which[,2])]
    rejected <- children[rejected.children]

                                        # add the running list of adjusted p.values
    adjusted.p.values <- adjp[, 2]
    adjusted.p.values <- adjusted.p.values[adjp.index]
    names(adjusted.p.values) <- rownames(hyp.tree@p.vals[children, ])

                                        # Now, we test only those hypotheses descending from those children
                                        # that were just rejected according ato the simultaneous comparison
                                        # in the family.

                                        # We can do this by finding all the subtrees descending from the
                                        # hypotheses that were rejected and testing those as a family.

    for(child in rejected){

        subcomp <- subcomponent(tree, child, "out")

        if(length(subcomp) > 1){
            subtree <- new("hypothesesTree")
            subtree.igraph <- induced.subgraph(graph = tree, vids = subcomp)
            subtree@tree <- as.matrix(get.adjacency(subtree.igraph))
            subtree@p.vals <- data.frame(unadjp = V(subtree.igraph)$p.vals, adjp = adjusted.p.values)
            rownames(subtree@p.vals) <- V(subtree.igraph)$hyp.names
            subtree@alpha <- alpha
            new.adjp <- hFDR.adjust(subtree, alpha)@p.vals[, 'adjp']
            adjusted.p.values <- c(adjusted.p.values, new.adjp)
        }

    }
    return(subtree)
}
