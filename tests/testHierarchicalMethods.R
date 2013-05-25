## Test the hypothesesTree creation class.

library(structSSI)

set.seed(130229)
library(adephylo)
tree.1 <- as.matrix(get.adjacency(as.igraph(rtree(10))))
tree.2 <- as.matrix(get.adjacency(as.igraph(rtree(50))))

hypotheses.names.1 <- paste("hyp", c(1:19))
hypotheses.names.2 <- paste("hyp", c(1:99))

unadjp.1 <- c(runif(5, 0, 0.01), runif(14, 0, 1))
unadjp.2 <- c(runif(10, 0.01), runif(89, 0, 1))

hypotheses.tree.1 <- new("hypothesesTree", tree = tree.1, unadj.p.values = unadjp.1, hypotheses.names = hypotheses.names.1)
hypotheses.tree.2 <- new("hypothesesTree", tree = tree.2, unadj.p.values = unadjp.2, hypotheses.names = hypotheses.names.2)

hypotheses.tree.1
hypotheses.tree.2

## The  hierarchical adjustment procedure
## applied to this class.

adjust1 <- hFDR.adjust(hypotheses.tree.1, 0.05)
adjust2 <- hFDR.adjust(hypotheses.tree.2, 0.05) # Correctly gives warning.

## Can plot results for the tree without the warning.

hypotheses.tree.1@rejected.hypotheses <- adjust1$rejected.hypotheses
hypotheses.tree.1@adj.p.values <- adjust1$adjp.values

plot(hypotheses.tree.1, p.values.type = "adjusted")
plot(hypotheses.tree.1, p.values.type = "unadjusted", 0.05)
