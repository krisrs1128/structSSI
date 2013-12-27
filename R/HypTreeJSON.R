HypTreeJSON <- function(hyp.tree, type = 'unadjusted', file = 'hyp_tree.JSON') {
    tree <- graph.edgelist(hyp.tree@tree)
    V(tree)$names <- rownames(hyp.tree@p.vals)
    if(type == 'adjusted') {
        V(tree)$pval <- round(hyp.tree@p.vals[, 'adjp'], 5)
    } else {
        V(tree)$pval <- round(hyp.tree@p.vals[, 'unadjp'], 5)
    }
    writeLines(toJSON(ListTreePval(tree)), con = file)
}
