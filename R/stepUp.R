StepUp <- function(adjp.temp) {
    N <- length(adjp.temp)
    running.min <- Inf
    for(i in N:1) {

      # ignore NAs, whereever they might occur
      if (!is.finite(adjp.temp[i])) {
        next
      }
      if(adjp.temp[i] > running.min) {
        adjp.temp[i] <- running.min
      } else {
        running.min <- adjp.temp[i]
      }
    }
    adjp.temp[adjp.temp > 1] <- 1
    adjp.temp
}
