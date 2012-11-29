plotUnadjustedHypothesesTree <- function(hyp.tree, alpha = 0.05){

    require(RColorBrewer)
    require(classInt)

    tree.to.plot <- igraph.to.graphNEL(graph.adjacency(hyp.tree@tree))

    ## Make the labels of this tree correspond to
    ## the names of hypotheses.

    nAttrs <- list()
    nAttrs$label <- hyp.tree@hypotheses.names
    nodeNames <- nodes(tree.to.plot)
    names(nAttrs$label) <- nodeNames

    u.pval <- hyp.tree@unadj.p.values
    sig <- which(u.pval <= alpha)
    unsig <- which(u.pval > alpha)
    pvals.sig <- u.pval[sig]
    pvals.unsig <- u.pval[unsig]

    nAttrs$fillcolor <- vector(length = length(hyp.tree@unadj.p.values))
    names(nAttrs$fillcolor) <- nodeNames

    n.sig.classes <- min(length(pvals.sig), 9)
    if(n.sig.classes > 1){

        # We are okay with having only one element in each interval
        pval.sig.class <- suppressWarnings(classIntervals(pvals.sig, n.sig.classes - 1))
        pvals.palette.green <- suppressWarnings(brewer.pal(n.sig.classes, "Greens"))
        pvals.palette.green <- pvals.palette.green[-1]
        pvals.palette.green <- pvals.palette.green[c((n.sig.classes - 1):1)] # we want more significant nodes to be darker color
        colCategory.green <- as.vector(findColours(pval.sig.class, pvals.palette.green))
        nAttrs$fillcolor[sig] <- colCategory.green

    } else {

        nAttrs$fillcolor[sig] <- "green"

    }

    n.unsig.classes  <- min(length(pvals.unsig), 9)
    if(n.unsig.classes > 1){

        # We are okay with having only one element in each interval
        pval.unsig.class <- suppressWarnings(classIntervals(pvals.unsig, n.unsig.classes - 1))
        pvals.palette.blue <- suppressWarnings(brewer.pal(n.unsig.classes, "Blues"))
        pvals.palette.blue <- pvals.palette.blue[-1]
        pvals.palette.blue <- pvals.palette.blue[c((n.unsig.classes - 1):1)]
        colCategory.blue <- as.vector(findColours(pval.unsig.class, pvals.palette.blue))
        nAttrs$fillcolor[unsig] <- colCategory.blue
    } else {
        nAttrs$fillcolor[unsig] <- "blue"
    }

    plot(tree.to.plot, nodeAttrs = nAttrs)

}
