## This defines a class for the output of the GBH procedure.
## The methods defined here should ease user interaction
## with the output from this procedure.

#' @title Class to facilitate performing the Group Benjamini-Hochberg procedure
#'   and interpreting its output.
#' @param p.vals Object of class \code{"data.frame"}. Each row correpsonds to an
#'   individual hypothesis. The first column stores the p-values before GBH
#'   adjustment, while the second gives the GBH adjusted p-values. The
#'   hypotheses are sorted in order of significance according to these GBH
#'   adjusted p-values. The \code{group} column gives the group membership of
#'   each hypothesis, and \code{adj.significnace} codes the significance of each
#'   hypothesis, according to the GBH adjusted p-values.
#' @param pi0 Object of class \code{"numeric"}. The proportion of null
#'   hypotheses within each group. This is either known a priori or estimated
#'   adaptively from the unadjusted p-values.
#' @param adaptive Object of class \code{"logical"}. An indicator of whether the
#'   proportion \code{pi0} was estimated adaptively from the data or known a
#'   priori.
#' @param alpha Object of class \code{"numeric"}. The level at which the FDR is
#'   controlled, during the GBH procedure.
#' @export
#' @rdname gbh-methods
#' @examples
#' # These are the unadjusted p-values corresponding to the outcome of some
#' # multiple testing experiment. The first 500 hypotheses are null and the last
#' # 1500 are true alternatives.
#'
#' unadjp <- c(runif(500, 0, 0.01), runif(1500, 0, 1))
#' names(unadjp) <- paste("Hyp: ", 1:2000)
#'
#' # These are the unadjusted p-values corresponding to the outcome of some
#' # multiple testing experiment. The first 500 hypotheses are null and the last
#' # 1500 are true alternatives.
#' unadjp <- c(runif(500, 0, 0.01), runif(1500, 0, 1))
#' names(unadjp) <- paste("Hyp: ", 1:2000)
#'
#' # Here there are two groups total we have randomly assigned hypotheses to these
#' # two groups.
#' group.index <- c(sample(1:2, 2000, replace = TRUE))
#'
#' # Perform the GBH adjustment.
#' result <-  Adaptive.GBH(unadjp, group.index, method = "storey")
#'
#' # A summary of the GBH adjustment
#' summary(result)
setClass(
  "GBH",
  representation = list(
    p.vals = "data.frame",
    pi0 = "numeric",
    adaptive = "logical",
    alpha = "numeric"
  )
)

#' @rdname gbh-methods
setMethod("initialize", "GBH", function(.Object, ...) {
  value <- callNextMethod()
  value
})

#' @slot show Prints the entire table of adjusted p-values and their associated
#'   FDR adjusted significance levels, together with the estimated proportions
#'   of null hypotheses, within each group.
#' @rdname gbh-methods
#' @aliases GBH
#' @export
setMethod("show", "GBH", function(object) {
  p.vals <- object@p.vals
  cat('GBH adjusted p values:', '\n')
  print(p.vals)
  cat('---', '\n')
  alpha <- object@alpha
  cat('Signif. codes:  0 \'***\'', alpha / 50, '\'**\'', alpha / 5, '\'*\'', alpha, '\'.\'', 2 * alpha, '\'-\' 1', '\n')
  cat('\n', 'Estimated proportion of hypotheses that are null, within each group:', '\n')
  print(object@pi0)

})

#' @slot summary Prints the most significant hypothesis, after adjusting for
#'   multiple testing via GBH. Also supplies the estimated proportion of null
#'   hypothesis within each group and a table of counts of adjusted significance
#'   across groups.
#' @aliases GBH
#' @rdname gbh-methods
#' @export
setMethod("summary", "GBH", function(object) {
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
  print(table(p.vals[, c('group', 'adj.significance')]))
})

#' @slot plot Plots the p-values of the hypothesis, sorted according to GBH
#'   adjusted significance, shape coded according to group membership, and color
#'   coded according to pre and post GBH p-value adjustment.
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
#' @aliases GBH,ANY
#' @rdname gbh-methods
setMethod("plot", "GBH", function(x,..., title = 'GBH Adjustment') {
  alpha <- x@alpha
  GBH <- data.frame(x@p.vals)
  GBH[, 'sorted.hyp'] <- 1:nrow(GBH)
  GBH[, 'group'] <- as.factor(GBH[, 'group'])

  mGBH <- melt(GBH[, -4], id.vars = c('sorted.hyp', 'group'))
  colnames(mGBH) <- c('sorted.hyp', 'group', 'type', 'pval')
  mGBH[, 'pval'] <- as.numeric(mGBH[, 'pval'])
  ggplot(mGBH) +
    geom_point(aes(x = sorted.hyp, y = pval, shape = group, col = type)) +
    geom_hline(yintercept = alpha, linetype = 2) +
    scale_x_discrete('Hypotheses sorted by adjusted p-values') +
    scale_y_continuous('Adjusted p-values') +
    ggtitle(title)
})
