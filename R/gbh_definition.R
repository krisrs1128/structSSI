## This defines a class for the output of the GBH procedure.
## The methods defined here should ease user interaction
## with the output from this procedure.

setClass("GBH", representation = list(GBH.adjust = "data.frame",
                    pi0 = "numeric", adaptive = "logical",
                    alpha = "numeric"))

setMethod("initialize", "GBH", function(.Object, ...) {
          value <- callNextMethod()
          value
      })

setMethod("show", "GBH", function(object) {
    cat('\n', 'Estimated proportion of hypotheses that are null, within each group:', '\n')
    print(object@pi0)
    GBH.adjust <- object@GBH.adjust
    cat('\n ', 'GBH adjusted p values:', '\n')
    print(GBH.adjust)
    cat('\n', '---', '\n')
    alpha <- object@alpha
    cat('Signif. codes:  0 \'***\'', alpha / 50, '\'**\'', alpha / 5, '\'*\'', alpha, '\'.\'', 2 * alpha, '\'-\' 1', '\n')
})

setMethod("summary", "GBH", function(object) {   
    cat('\n', 'Estimated proportion of hypotheses that are null, within each group:', '\n')
    print(object@pi0)
    
    GBH.adjust <- object@GBH.adjust
    cat('\n', 'Significance across groups:', '\n')
    print(table(GBH.adjust[, c('group', 'adj.significance')]))
    
    n.to.print <- min(nrow(GBH.adjust), 10)
    cat('\n ', 'GBH adjusted p values:', '\n')
    print(object@GBH.adjust[1:n.to.print, ])
    if(n.to.print < nrow(GBH.adjust)) {
        cat('\n', '[only 10 most significant hypotheses shown]', '\n')
    }
    alpha <- object@alpha
    cat('\n', '---', '\n')
    cat('Signif. codes:  0 \'***\'', alpha / 50, '\'**\'', alpha / 5, '\'*\'', alpha, '\'.\'', 2 * alpha, '\'-\' 1', '\n')
})

setMethod("plot", "GBH", function(x,..., adjust = TRUE) {
    alpha <- x@alpha
    GBH <- data.frame(x@GBH.adjust)
    GBH[, 'sorted.hyp'] <- 1:nrow(GBH)
    GBH[, 'group'] <- as.factor(GBH[, 'group'])
    if(adjust) {
        p <- ggplot(GBH) +
                geom_point(aes(x = sorted.hyp, y = adjp, col = group)) +
                geom_hline(yintercept = alpha, linetype = 2) + 
                ggtitle('GBH Adjusted p values')
    } else {
        p <- ggplot(GBH) +
                geom_point(aes(x = sorted.hyp, y = unadjp, col = group)) +
                geom_hline(yintercept = alpha, linetype = 2) + 
                ggtitle('Unadjusted p values')
    }
    return(p)
})
