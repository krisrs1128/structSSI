
  # This function performs the Group Benjamini-Hochberg procedure at level alpha.
  #
  # Input: 1) unadjp - [vector numerics between 0 and 1] - The unadjusted p-values that have been computed by testing
  # multiple hypotheses tests. This is the same as what one would input into the
  # multtest function mt.rawp2adjp(), for example.
  # 2) group.index - [same as other groups vectors] - A vector that contains the group labeling for each hypothesis where
  # the index of each hypothesis is the same as the index used for the unadjusted
  # p-values above. For example, if the first 3 hypotheses were in group 1 and
  # the last 2 were in group 2, then we would input (1, 1, 1, 2, 2) as the ``groups''
  # vector.
  # 3) pi.groups - [vector numerics between 0 and 1, length = # groups] - This is the
  # vector of proportion of true null hypotheses that corresponds
  # to each group. In the adaptive procedure, these proportions are estimated from
  # the data. For example, if the hypotheses were divided into two groups, and
  # we found that the proportion of true null hypotheses in the first group was 0.9
  # and the proportion of true null hypotheses in the second group was 0.6, then we
  # would input pi.groups = c(0.9, 0.6). This is the aspect of the Group-Benjamini
  # Hochberg procedure that gives it more information to operate on than the
  # standard BH procedure: We are able to direct attention to those hypotheses
  # that are in groups where the proportion of true null hypotheses is estimated
  # to be low.
  #
  # Output: 1) GBH.results - A list including (a) the sorted adjusted p-values and (b) the
  # indices of the original hypotheses /uadjusted p-values that these adjusted p-values
  # correspond to (c) the hypotheses that were rejected, labeled by their original
  # unadj.p indexing, and (d) the hypotheses that were not rejected, also labeled
  # by their original unadj.p indexing.

