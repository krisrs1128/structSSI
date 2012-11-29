
## This function takes a tree with adjusted p-values
## and displays the results. It shows how the testing
## procedure proceeded down the tree. It also shows that
## the gradation of adjusted p-values for the nodes
## that were rejected.


plotAdjustedHypothesesTree <- function(hyp.tree){

    require(RColorBrewer)
    require(classInt)

    tree.to.plot <- igraph.to.graphNEL(graph.adjacency(hyp.tree@tree))

    ## Make the labels of this tree correspond to
    ## the names of hypotheses.

    nAttrs <- list()
    nAttrs$label <- hyp.tree@hypotheses.names
    nodeNames <- nodes(tree.to.plot)
    names(nAttrs$label) <- nodeNames

    adj.pval <- hyp.tree@adj.p.values
    nTested <- length(adj.pval)

    hyp.names <- hyp.tree@hypotheses.names
    nHyp <- length(hyp.names)
    rejected.hyp <- hyp.tree@rejected.hypotheses

    p.vals.total <- vector(length = nHyp)
    names(p.vals.total) <- hyp.names
    p.vals.total[names(adj.pval)] <- adj.pval
    p.vals.total[which(p.vals.total == 0)] <- NA

    # Handle the case where no hypotheses are significant, because the
    # root node is not rejected.
    if(length(hyp.tree@adj.p.values) == 1){
        warning("Root node not significant, no hypotheses rejected.")
        return()
    }

    pvals.sig <- p.vals.total[rejected.hyp]

    tested.hyp <- names(adj.pval)
    untested.hyp <- names(p.vals.total[is.na(p.vals.total)])
    if(length(which(tested.hyp %in% rejected.hyp)) > 0){
        unrejected.hyp <- tested.hyp[-which(tested.hyp %in% rejected.hyp)]
    } else {
        unrejected.hyp <- tested.hyp
    }

    # First find the indices of those hypotheses that were not tested

    untested.hyp.indices <- which(names(p.vals.total) %in% untested.hyp)
    rejected.hyp.indices <- which(names(p.vals.total) %in% rejected.hyp)
    unrejected.hyp.indices <-which(names(p.vals.total) %in% unrejected.hyp)

    # Now we find find the p-values corresponding to those hypotheses that were
    # rejected and those that were not rejected.

    rejected.hyp.pvals <- p.vals.total[rejected.hyp.indices]
    unrejected.hyp.pvals <- p.vals.total[unrejected.hyp.indices]

    # Now we're going to shade in these hypotheses testing trees according to
    # whether they were rejected or not (or not tested) and according to the
    # strength of the rejection / non-rejection.

    nAttrs$fillcolor <- vector(length = nHyp)
    names(nAttrs$fillcolor) <- nodeNames

    nAttrs$fillcolor[untested.hyp.indices] <- "grey"

    n.rejected.classes <- min(length(rejected.hyp.pvals), 9)

    # RColorBrewer cannot make palettes with fewer than 3 colors.
    if(n.rejected.classes > 2){

        # We are okay with having only one element in each interval
        pval.rejected.class <- suppressWarnings(classIntervals(rejected.hyp.pvals, n.rejected.classes - 1))
        pvals.palette.green <- brewer.pal(n.rejected.classes, "Greens")
        pvals.palette.green <- pvals.palette.green[-1]
        pvals.palette.green <- pvals.palette.green[c((n.rejected.classes - 1):1)] # we want more significant nodes to be darker color
        colCategory.green <- as.vector(findColours(pval.rejected.class, pvals.palette.green))
        nAttrs$fillcolor[rejected.hyp.indices] <- colCategory.green

    } else {

        nAttrs$fillcolor[rejected.hyp.indices] <- "#006D2C"

    }

    n.unrejected.classes  <- min(length(unrejected.hyp.pvals), 9)

    if(n.unrejected.classes > 2){

        # We are okay with having only one element in each interval
        pval.unrejected.class <- suppressWarnings(classIntervals(unrejected.hyp.pvals, n.unrejected.classes - 1))
        pvals.palette.blue <- brewer.pal(n.unrejected.classes, "Blues")
        pvals.palette.blue <- pvals.palette.blue[-1]
        pvals.palette.blue <- pvals.palette.blue[c((n.unrejected.classes - 1):1)]# we want more significant nodes to be darker color
        colCategory.blue <- as.vector(findColours(pval.unrejected.class, pvals.palette.blue))
        nAttrs$fillcolor[unrejected.hyp.indices] <- colCategory.blue

    } else {

        # use the RColorBrewer blue to shade in the one hypotheses.

        nAttrs$fillcolor[unrejected.hyp.indices] <- "#3182BD"

    }

    # We have checked that the root hypotheses is rejected, so we
    # shade it the RColorBrewer shade of green.

    nAttrs$fillcolor[1] <- "#006D2C"

    plot(tree.to.plot, nodeAttrs = nAttrs)

}
