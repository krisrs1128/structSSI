## This defines a class for the output of the GBH procedure.
## The methods defined here should ease user interaction
## with the output from this procedure.

setClass("GBH", representation = list(GBH.adjust = "data.frame",
                    pi0 = "numeric", adaptive = "logical"))

setMethod("initialize", "GBH", function(.Object, ...) {
          value <- callNextMethod()
          validObject(value)
          value
      })

setMethod("show", "GBH", function(object) {
    print(object@GBH.adjust)
    print(object@pi0)
    print(object@adaptive)
})

setMethod("summary", "GBH", function(object) {   
}

setMethod("plot", "GBH", function(x,..., adjust = TRUE, alpha = 0.05) {
          GBH <- x@GBH.adjust
          if(adjust) {
              p <- ggplot(GBH) +
                      geom_point(aes(x = 1:nrow(GBH), y = adjp, col = group)) +
                      geom_hline(yintercept = alpha) + 
                      ggtitle('GBH Adjusted p values')
          } else {
              p <- ggplot(GBH) +
                      geom_point(aes(x = 1:nrow(GBH), y = unadjp, col = group)) +
                      geom_hline(yintercept = alpha) +
                      ggtitle('GBH Adjusted p values')
          }
          return(p)
      })
