
SignificanceStars <- function(alpha, pvalues) {
  result <- rep(NA, length(pvalues))
  result[pvalues < alpha / 50] <- '***'
  result[pvalues >= alpha / 50 & pvalues < alpha / 5] <- '**'
  result[pvalues >= alpha / 5 & pvalues < alpha] <- '*'
  result[pvalues >= alpha & pvalues < 2 * alpha] <- '.'
  result[pvalues > 2 * alpha] <- '-'
  result
}

pi0.tail.p <- function(lambda, p.values){
  num <- length(which(p.values >= lambda))
  denom <- length(p.values)*(1 - lambda)
  min(num/denom, 1)
}

#' @importFrom multtest mt.rawp2adjp mt.reject
pi0.tst <- function(p.val, alpha = 0.05){
  alpha.prime <- alpha/(1 + alpha)
  n_g <- length(p.val)
  adjustment <- mt.rawp2adjp(p.val, proc = "BH")
  rejected <- mt.reject(adjustment$adjp, alpha.prime)
  n.rejected <- rejected$r[,2]
  (n_g - n.rejected) / n_g
}

pi0.lsl <- function(p.val){
  p.val <- sort(p.val)
  n_g <- length(p.val)

  i <- 1
  while(TRUE){
    if(i >= 2){
      l_g.i.prev <- l_g.i
    } else {
      # We don't want to stop on the first iteration, so we set the values used
      # to estimate pi0 very high
      l_g.i.prev <- Inf
    }
    if(p.val[i] < 1){
      l_g.i <- (n_g + 1 - i)/(1 - p.val[i])
    } else {
      return(l_g.i.prev) # If you're dividing by zero, then you guarantee an increase
    }
    if(l_g.i > l_g.i.prev || i == length(p.val)){
      pi0 <- (floor(l_g.i) + 1)/n_g
      pi0 <- min(pi0, 1)
      return(pi0)
    }
    i <- i + 1
  }
}

#' @title Estimation of Proportion of Null Hypotheses among p-values
#'
#' @description This function makes three routines available for estimating the
#'   true proportion of null hypotheses from a vector of unadjusted p-values
#'   arising from a multiple testing experiment. The specific methods are the
#'   Least Slope method (\code{lsl}), the Two Step Test method (\code{tst}), and
#'   the Storey tail proportion of p-values method (\code{storey}). These
#'   methods are derived and explained in the references given below.
#' @param pvalues A vector of the unadjusted p-values resulting from a multiple
#'   testing experiment.
#' @param method The technique used to estimate the proportion of true null
#'   hypotheses. Valid arguments are \code{lsl}, \code{tst}, or \code{storey},
#'   which correspond to the Least Slope, Two Step Test, and Storey tail
#'   proportion of p-values methods, respectively.
#' @param alpha In the Two Step Test method, the level of the Benjamini-Hochberg
#'   procedure used to estimate the propotion of true null hypotheses.
#' @param lambda In the Storey tail proportion of p-values method, the cutoff
#'   used to differentiate p-values.
#' @return An estimate of the proportion of true null hypotheses from the result
#'   of the multiple testing experiment that the unadjusted p-values were
#'   extracted from.
#' @details The Least Slope method uses the insight that, if we plot the sorted
#'   unadjusted p-values, then the p-values corresponding to null hypotheses
#'   will have steep slopes, as they are uniformly distributed between 0 and 1.
#'   If we find the position where the slope of the sorted p-values increases
#'   the most, we can fix that slope and extrapolate to the x-axis, and the
#'   position where the line intersects the x-axis is the proportion of p-values
#'   estimated to be null. The formal derivation is presented in the reference
#'   below.
#'
#'   Storey's method uses the idea that most of the p-values above some
#'   parameter lambda (usually set to 0.5) correspons to null hypotheses. The
#'   number of hypotheses in this interval can then be used to estimate the
#'   number of hypotheses overall which are null hypotheses.
#'
#'   The Two Step Test method uses the idea that the result of a multiple
#'   comparisons procedure gives an estimate for how many hypotheses are null.
#'   That is, if we perform the BH procedure on 100 hypotheses, and 10 of them
#'   are rejected, then a reasonable estimate of the proportion of null
#'   hypotheses among those 100 is pi0 = 0.9.
#'
#'   Benjamini, Y, Krieger, A.M., and Yekutieli, D. Adaptive linear step-up
#'   procedures that control the false discovery rate. Biometrica, 93(3):491,
#'   2006.
#'
#'   Benjamini, Y, and Hochberg, Y. ``On the adaptive control of the false
#'   discovery rate in multiple testing with independent statistics.'' Journal
#'   of Educational and Behavioral Statistics, 25(1):60, 2000.
#'
#'   Sankaran, K and Holmes, S. structSSI: Simultaneous and Selective Inference
#'   for Grouped or Hierarchically Structured Data. Journal of Statistical
#'   Software, 59(13), 1-21. 2014. http://jstatsoft.org/v59/i13/
#'
#'   Storey, J.D., Taylor, J.E., and Siegmund, D. Strong control, conservative
#'   point estimation, and simultaneous conservative consistency of false
#'   discovery rates: a unified approach. Journal of the Royal Statistical
#'   Society: Series B (Statistical Methodology),66(1):187-205. 2004.
#'
#' @export
#' @examples
#' true.p.1 <- runif(20, 0, 0.01)
#' null.p.1 <- runif(980, 0, 1)
#' p.val.1 <- c(true.p.1, null.p.1)
#'
#' estimate.pi0(p.val.1, "lsl")
#' estimate.pi0(p.val.1, "storey", lambda = 0.2)
#' estimate.pi0(p.val.1, "tst")
estimate.pi0 <- function(pvalues, method, alpha = 0.05, lambda = 0.5){
  method <- tolower(method)
  matched.method <- match.arg(method, c("tst", "lsl", "storey"))
  if(matched.method == "tst"){
    return(pi0.tst(pvalues, alpha))
  } else if(matched.method == "lsl"){
    return(pi0.lsl(pvalues))
  } else if(matched.method == "storey"){
    return(pi0.tail.p(lambda, pvalues))
  } else {
    stop("Method must be one of {'tst', 'lsl', 'storey'}")
  }
}

#' @title Adaptive Group Benjamini-Hochberg Procedure
#'
#' @description Performs the Group Benjamini-Hochberg procedure using adaptive
#'   estimates of the proportions of null p-values in given groups. The method
#'   is applicable when we know some a-priori structure about whether certain
#'   hypotheses can be grouped. Once the hypotheses are grouped and tested
#'   individually, a Benjamini-Hochberg correction is performed within each of
#'   the groups. Finally, the fact that the Benjamini-Hochberg correction
#'   controls the FDR at level q*pi0_group within each group (q is the level
#'   used in the and p-value comparison and pi0 is the proportion of null
#'   hypotheses within the particular group) is used to increase the power of
#'   the procedure. The procedure is described in more detail in the paper
#'   "False Discovery Rate Control with Groups" by Hu, Zhao, and Zhou (see
#'   references below).
#'
#' @param unadj.p A vector of the unadjusted p-values resulting from a multiple
#'   testing experiment.
#' @param group.index A vector of the same length as the vector of unadjusted
#'   p-values, where a "G" in the jth coordinate indicates that the jth
#'   unadjusted p-values in \code{unadj.p} belongs to group "G". This code can
#'   be either a factor giving group names, or a numeric index.
#' @param alpha The level of that we are attempting to control the FDR at.
#' @param method The method for adaptively estimating the proportion of true
#'   null hypotheses within a vector of unadjusted p-values. The possible
#'   options are "storey", "lsl", and "tst". See the documentation for
#'   \code{estimatePi0} for more details.
#' @param lambda The value of the tuning parameter for estimating the proportion
#'   of null hypotheses, in the "storey" method.
#' @return An object of class \code{GBH}, which provides the adjusted p-values.
#' @details Hu, J.X., Zhao, H., and Zhou, H.H. False discovery rate control with
#'   groups. Journal of the American Statistical Association, volume 104, number
#'   491. Pages 1215-1227. 2010.
#'
#'   Sankaran, K and Holmes, S. structSSI: Simultaneous and Selective Inference
#'   for Grouped or Hierarchically Structured Data. Journal of Statistical
#'   Software, 59(13), 1-21. 2014. http://jstatsoft.org/v59/i13/
#' @export
#' @examples
#' # These are the unadjusted p-values corresponding to the outcome of some
#' # multiple testing experiment. The first 500 hypotheses are null and the last
#' # 1500 are true alternatives.
#' unadjp <- c(runif(500, 0, 0.01), runif(1500, 0, 1))
#' names(unadjp) <- paste("Hyp:", 1:2000)
#'
#' # Here there are two groups total we have randomly assigned hypotheses to
#' # these two groups.
#' group.index <- c(sample(1:2, 2000, replace = TRUE))
#'
#' # Perform the GBH adjustment.
#' result <- Adaptive.GBH(unadjp, group.index, method = "storey")
#'
#' # A summary of the GBH adjustment
#' summary(result)
#'
#' # The estimated proportions of null hypotheses, between groups
#' result@pi0
#'
#' # Visualizing the sorted p-values, before and after adjustment
#' plot(result, adjust = TRUE)
#' plot(result, adjust = FALSE)
Adaptive.GBH <- function(unadj.p, group.index, alpha = 0.05, method = 'lsl',
                         lambda = 0.5){
  method <- tolower(method)
  method <- match.arg(method, c("storey", "lsl", "tst"))

  groups <- unique(group.index)
  nGroups <- length(groups)
  pi.groups <- vector(length = nGroups)
  names(pi.groups) <- groups

  for(i in seq_len(nGroups)) {
    if(method == 'storey'){
      pi.groups[i] <- pi0.tail.p(lambda, unadj.p[which(group.index == groups[i])])
    } else if(method == 'lsl'){
      pi.groups[i] <- pi0.lsl(unadj.p[which(group.index == groups[i])])
    } else if(method == 'tst'){
      pi.groups[i] <- pi0.tst(unadj.p[which(group.index == groups[i])], alpha)
      unadj.p <- (1 + alpha) * unadj.p
    }
  }

  result <- Oracle.GBH(unadj.p, group.index, pi.groups, alpha)
  result@adaptive <- TRUE
  result
}

#' @title Oracle Group Benjamini-Hochberg Correction
#'
#' @description Performs the Group Benjamini-Hochberg procedure when the true
#'   proportion of null hypotheses is known within each group. The procedure is
#'   applicable whenever group structure about the relationship between
#'   different hypotheses is known before testing begins. The idea is to control
#'   the FDR within each group and to use the proportion of null hypotheses
#'   present within each group to make the testing procedure within that group
#'   either more or less conservative -- this is in fact the idea behind all
#'   adaptive multiple testing procedures.
#'
#'   The Oracle GBH method can also be used when we would like to use the
#'   Adaptive GBH procedure but with estimates of proportions of true null
#'   hypotheses within groups that are not made directly available through the
#'   \code{Adaptive.GBH} function -- in this case those estimates can be input
#'   as the argument \code{pi.groups} in the this function \code{Oracle.GBH}.
#'
#' @param unadj.p A vector of the unadjusted p-values resulting from a multiple
#'   testing experiment.
#' @param group.index A vector of the same length as the vector of unadjusted
#'   p-values, where a "G" in the jth coordinate indicates that the jth
#'   unadjusted p-values in \code{unadj.p} belongs to group "G". This code can
#'   be either a factor giving group names, or a numeric index.
#' @param pi.groups A vector of the known proportions of true null hypotheses
#'   within each of the groups. This vector should be named so that each element
#'   of the \code{group.index} vector correspond to one of the names of the
#'   \code{pi.groups} vector.
#' @param alpha The level that we are attempting to control the FDR at.
#' @return An object of class \code{GBH}, which provides the adjusted p-values.
#' @details Hu, J.X., Zhao, H., and Zhou, H.H. False discovery rate control
#'   with groups. Journal of the American Statistical Association, volume 104,
#'   number 491. Pages 1215-1227. 2010.
#'
#'   Sankaran, K and Holmes, S. structSSI: Simultaneous and Selective Inference
#'   for Grouped or Hierarchically Structured Data. Journal of Statistical
#'   Software, 59(13), 1-21. 2014. http://jstatsoft.org/v59/i13/
#'
#' @export
#' @examples
#' # A very simple example, with only 5 hypotheses.
#' unadjp <- c(0.002, 0.1, 0.001, 0.01, 0.4)
#' names(unadjp) <- paste("hyp", 1:5)
#' groups <- c(1, 2, 1, 2, 2)
#'
#' # Say we know goup 1 has pi_0,1 = 0.3 and pi_0,2 = 0.9
#' pi.groups <- c("1" = 0.3, "2" = 0.9)
#' Oracle.GBH(unadjp, groups, pi.groups)
#'
#' # An example where we use an external pi0 estimation routine
#'
#' unadjp.2 <- c(runif(500, 0, 0.01), runif(1500, 0, 1))
#' names(unadjp.2) <- paste("hyp", 1:2000)
#' groups.2 <- c(sample(1:2, 2000, replace = TRUE))
#' pi.groups <- c("1" = NA, "2" = NA)
#' for(i in 1:2){
#'   pi.groups[i] <- estimate.pi0(unadjp.2[which(groups.2 == i)], method = "storey")
#' }
#'
#' result <- Oracle.GBH(unadjp.2, groups.2, pi.groups, 0.05)
#' result@pi0
#' result@p.vals
Oracle.GBH <- function(unadj.p, group.index, pi.groups, alpha = 0.05){
    if (!all(group.index %in% names(pi.groups))) {
        stop('Names of pi.groups vector must match the elements of groups vector.')
    }
    if (length(unadj.p) != length(group.index)) {
        stop('Length of p values vector does not match group indexing vector.')
    }
    if (is.null(names(unadj.p))) {
      names(unadj.p) <- seq_along(unadj.p)
    }

    p.weighted <- unadj.p
    N <- length(unadj.p)

    names.sort <- sort(names(pi.groups))
    n_g <- table(group.index)
    pi0 <- 1/N * sum(n_g[names.sort] * pi.groups[names.sort])

    # The first part of the procedure involves weighting p-values. This is where the
    # known group structure information is being explicitly accounted for.
    pi.groups.match <- pi.groups[as.character(group.index)]
    p.weighted <- unadj.p * (pi.groups.match / (1 - pi.groups.match))

    # The second part of the procedure is exactly like Benjamini-Hochberg in
    # that it is a step-up procedure where we compared ordered p-values to some
    # constant factor times alpha, where the constant is determined by the
    # position of the p-value in the ordered list.
    sorting.weighted.p <- sort(p.weighted, index.return = TRUE)
    p.weighted <- sorting.weighted.p$x
    p.weighted.index <- sorting.weighted.p$ix

    if(pi0 < 1) {
        adjp.temp <- N * (1 - pi0) * p.weighted / 1 : N
    } else {
        adjp.temp <- p.weighted
    }

    adjp <- StepUp(adjp.temp)
    p.vals <- data.frame(
      'hypothesis' = names(unadj.p)[p.weighted.index],
      'original_index' = p.weighted.index,
      'unadjp' = unadj.p[p.weighted.index],
      'adjp' = adjp,
      'group' = group.index[p.weighted.index],
      'significance' = SignificanceStars(alpha, adjp),
      row.names = NULL
    )
    new('GBH', p.vals = p.vals, pi0 = pi.groups, adaptive = F, alpha = alpha)
}


StepUp <- function(adjp.temp) {
  N <- length(adjp.temp)
  running.min <- adjp.temp[N]
  for(i in N:1) {
    if(adjp.temp[i] > running.min) {
      adjp.temp[i] <- running.min
    } else {
      running.min <- adjp.temp[i]
    }
  }
  adjp.temp[adjp.temp > 1] <- 1
  adjp.temp
}
