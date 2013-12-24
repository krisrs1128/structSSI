SignificanceStars <- function(alpha, pvalues) {
        result <- vector(length = length(pvalues))
        result[pvalues < alpha / 10] <- '***'
        result[pvalues >= alpha / 10 & pvalues < alpha / 2] <- '**'
        result[pvalues >= alpha / 2 & pvalues < alpha] <- '*'
        result[pvalues > alpha] <- '-'
        return(result)
    }
