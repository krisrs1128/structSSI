    BuildEdgelist <- function(hyp.tree) {
        tree <- graph.adjacency(hyp.tree@tree)
        edgelist <- data.frame(get.edgelist(tree), stringsAsFactors = F)
        colnames(edgelist) <- c('parent', 'child')
        root <- unique(edgelist[which(!(edgelist[, 1] %in% edgelist[, 2])), 1]) # no edge leads to root
        return(list(el = edgelist, root = root))
    }
