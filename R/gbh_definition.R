## This defines a class for the output of the GBH procedure.
## The methods defined here should ease user interaction
## with the output from this procedure.

setClass("GBH", representation = list(p.vals = "data.frame",
                    pi0 = "numeric", adaptive = "logical",
                    alpha = "numeric"))

setMethod("initialize", "GBH", function(.Object, ...) {
          value <- callNextMethod()
          value
      })

setMethod("show", "GBH", function(object) {
    cat('\n', 'Estimated proportion of hypotheses that are null, within each group:', '\n')
    print(object@pi0)
    p.vals <- object@p.vals
    cat('\n ', 'GBH adjusted p values:', '\n')
    print(p.vals)
    cat('\n', '---', '\n')
    alpha <- object@alpha
    cat('Signif. codes:  0 \'***\'', alpha / 50, '\'**\'', alpha / 5, '\'*\'', alpha, '\'.\'', 2 * alpha, '\'-\' 1', '\n')
})

setMethod("summary", "GBH", function(object) {   
    cat('\n', 'Estimated proportion of hypotheses that are null, within each group:', '\n')
    print(object@pi0)
    
    p.vals <- object@p.vals
    cat('\n', 'Significance across groups:', '\n')
    print(table(p.vals[, c('group', 'adj.significance')]))
    
    n.to.print <- min(nrow(p.vals), 10)
    cat('\n ', 'GBH adjusted p values:', '\n')
    print(object@p.vals[1:n.to.print, ])
    if(n.to.print < nrow(object@p.vals)) {
        cat('\n', '[only 10 most significant hypotheses shown]', '\n')
    }
    alpha <- object@alpha
    cat('\n', '---', '\n')
    cat('Signif. codes:  0 \'***\'', alpha / 50, '\'**\'', alpha / 5, '\'*\'', alpha, '\'.\'', 2 * alpha, '\'-\' 1', '\n')
})

setMethod("plot", "GBH", function(x,..., title = 'GBH Adjustment') {
    alpha <- x@alpha
    GBH <- data.frame(x@p.vals)
    GBH[, 'sorted.hyp'] <- 1:nrow(GBH)
    GBH[, 'group'] <- as.factor(GBH[, 'group'])

    mGBH <- melt(GBH[, -4], id.vars = c('sorted.hyp', 'group'))
    colnames(mGBH) <- c('sorted.hyp', 'group', 'type', 'pval')
    mGBH[, 'pval'] <- as.numeric(mGBH[, 'pval'])
    p <- ggplot(mGBH) + 
        geom_point(aes(x = sorted.hyp, y = pval, shape = group, col = type)) +
        geom_hline(yintercept = alpha, linetype = 2) +
        ggtitle(title)
    return(p)
})
