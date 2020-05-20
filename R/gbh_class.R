#' Manage Group Benjamini-Hochberg Outputs
#'
#' This defines a class GBH for managing outputs from the Group
#' Benjamini-Hochberg procedure. This object makes it easy to print, summarize,
#' and plot the results of the testing procedure.
#'
#' @slot p.vals Object of class \code{'data.frame'}. Each row corresponds to an
#'   individual hypothesis. The different columns correspond to,
#'     * hypothesisIndex: The index of the current hypothesis in the
#'        \code{unadjp} vector
#'     * hypothesisName: The name of the current hypothesis, from the names of
#'        the \code{unadjp} vector
#'     * unadjp: The unadjusted p-values input from \code{unadjp}
#'     * adjp: The adjusted p-values, after the GBH adjustment.
#'     * group: The group to which the original hypothesis belonged
#'     * significance: A code for the significance of each hypothesis
#' @slot pi0 Object of class \code{'numeric'}. The proportion of null
#'   hypotheses within each group. This is either known a priori or estimated
#'   adaptively from the unadjusted p-values.
#' @slot adaptive Object of class \code{'logical'}. An indicator of whether the
#'   proportion \code{pi0} was estimated adaptively from the data or known a
#'   priori.
#' @slot alpha Object of class \code{'numeric'}. The level at which the FDR is
#'   controlled, during the GBH procedure.
#' @export
#' @import methods
#' @rdname gbh-class
#' @examples
#' # These are the unadjusted p-values corresponding to the outcome of some
#' # multiple testing experiment. The first 500 hypotheses are null and the last
#' # 1500 are true alternatives.
#'
#' unadjp <- c(runif(500, 0, 0.01), runif(1500, 0, 1))
#' names(unadjp) <- paste('Hyp: ', 1:2000)
#'
#' # These are the unadjusted p-values corresponding to the outcome of some
#' # multiple testing experiment. The first 500 hypotheses are null and the last
#' # 1500 are true alternatives.
#' unadjp <- c(runif(500, 0, 0.01), runif(1500, 0, 1))
#' names(unadjp) <- paste('Hyp: ', 1:2000)
#'
#' # Here there are two groups total we have randomly assigned hypotheses to these
#' # two groups.
#' group.index <- c(sample(1:2, 2000, replace = TRUE))
#'
#' # Perform the GBH adjustment.
#' result <-  Adaptive.GBH(unadjp, group.index, method = 'storey')
#'
#' # A summary of the GBH adjustment
#' summary(result)
setClass(
  'GBH',
  representation = list(
    p.vals = 'data.frame',
    pi0 = 'numeric',
    adaptive = 'logical',
    alpha = 'numeric'
  )
)

#' Initialize a GBH object
#'
#' @rdname gbh-class
#' @param .Object Dummy to initialize S4 class
#' @param ... Any other arguments are accepted, but they will be ignored.
setMethod('initialize', 'GBH', function(.Object, ...) {
  value <- callNextMethod()
  value
})

#' Print all adjusted hypotheses
#'
#' Prints the entire table of adjusted p-values and their associated FDR
#' adjusted significance levels, together with the estimated proportions of null
#' hypotheses, within each group.
#'
#' @param object A GBH object whose hypotheses we want to summarize.
#' @rdname gbh-class
#' @aliases GBH
#' @export
setMethod('show', 'GBH', function(object) {
  p.vals <- object@p.vals
  cat('GBH adjusted p values:', '\n')
  print(p.vals)
  cat('---', '\n')
  alpha <- object@alpha
  cat('Signif. codes:  0 \'***\'', alpha / 50, '\'**\'', alpha / 5, '\'*\'', alpha, '\'.\'', 2 * alpha, '\'-\' 1', '\n')
  cat('\n', 'Estimated proportion of hypotheses that are null, within each group:', '\n')
  print(object@pi0)

})

#' Print most significant hypothesis
#'
#' Shows results from multiple testing via GBH. Also supplies the estimated
#' proportion of null hypothesis within each group and a table of counts of
#' adjusted significance across groups.
#'
#' @param object A GBH object whose hypotheses we want to summarize.
#' @aliases GBH
#' @rdname gbh-class
#' @export
setMethod('summary', 'GBH', function(object) {
  p.vals <- object@p.vals
  alpha <- object@alpha
  n.to.print <- min(nrow(p.vals), 10)
  cat('GBH adjusted p values:', '\n')
  print(object@p.vals[1:n.to.print, ])
  if(n.to.print < nrow(object@p.vals)) {
    cat('[only 10 most significant hypotheses shown]', '\n')
  }

  cat('---', '\n')
  cat('Signif. codes:  0 \'***\'', alpha / 50, '\'**\'', alpha / 5, '\'*\'', alpha, '\'.\'', 2 * alpha, '\'-\' 1', '\n')
  cat('\n', 'Estimated proportion of hypotheses that are null, within each group:', '\n')
  print(object@pi0)

  cat('\n', 'Significance across groups:', '\n')
  print(table(p.vals[, c('group', 'significance')]))
})

#' Plots the p-values from Adaptive GBH
#'
#' Show results of testing hypothesis, sorted according to GBH adjusted
#' significance, shape coded according to group membership, and color coded
#' according to pre and post GBH p-value adjustment.
#'
#' @param x A GBH object whose p-values to plot.
#' @param title The name added to the top of the plot. Defaults to 'GBH
#'   Adjustment'.
#' @param ... Any other arguments are accepted, but they will be ignored.
#'
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
#' @aliases GBH,ANY
#' @rdname gbh-class
setMethod('plot', 'GBH', function(x, title = 'GBH Adjustment', ...) {
  alpha <- x@alpha
  GBH <- x@p.vals
  GBH[, 'group'] <- as.factor(GBH[, 'group'])
  GBH[, 'index'] <- seq_len(nrow(GBH))

  mGBH <- melt(
    GBH[, c('hypothesisName', 'hypothesisIndex', 'unadjp', 'adjp', 'group')],
    id.vars = c('hypothesisName', 'hypothesisIndex', 'group'),
    variable.name = 'type',
    value.name = 'pval'
  )

  ggplot(mGBH) +
    geom_point(aes_string(x = "hypothesisIndex", y = "pval", shape = "group", col = "type")) +
    geom_hline(yintercept = alpha, linetype = 2) +
    scale_x_continuous('Hypotheses sorted by adjusted p-values') +
    scale_y_continuous('Adjusted p-values') +
    ggtitle(title)
})
