### GBH Tests ###

library('structSSI')
set.seed(13082)

## Testing pi0 estimation procedures. ##

unadjp <- c(runif(100, 0, 1), runif(100, 0, 0.01))
estimate.pi0(unadjp, method = "tst")
estimate.pi0(unadjp, method = "lsl")
estimate.pi0(unadjp, method = "storey")

unadjp <- c(runif(990, 0, 1), runif(10, 0, 0.01))
estimate.pi0(unadjp, method = "tst")
estimate.pi0(unadjp, method = "lsl")
estimate.pi0(unadjp, method = "storey")
estimate.pi0(unadjp, method = "storey", lambda = 0.7)

## Testing Adaptive Procedure ##

## We randomly generate some data sets to test.

## Data set 1

unadjp.1 <- c(runif(500, 0, 0.01), runif(1500, 0, 1))
names(unadjp.1) <- paste("Hyp: ", 1:2000)
groups.1 <- c(sample(1:2, 2000, replace = TRUE))
result.1 <-  Adaptive.GBH(unadjp.1, groups.1, method = "storey", alpha = 0.05)
head(result.1$adjp.index)

length(result.1$adjp.index)
length(result.1$adjp)

## Check that the number of rejected hypotheses equals
## the number of adjusted p-values that are less than 0.05

length(which(result.1$adjp <= 0.05))
length(result.1$rejected)

## Data set 2

unadjp.2 <- c(runif(100, 0, 0.05), runif(900, 0, 1))
names(unadjp.2) <- paste("hyp: ", 1:1000)
groups.2 <- c(rep(1, 500), rep(2, 500))
result.2 <- Adaptive.GBH(unadjp.2, groups.2, method = "lsl", alpha = 0.05)
head(result.2$adjp.index)
head(result.2$adjp)
head(result.2$rejected)
head(result.2$not.rejected)

## Testing Oracle GBH Procedure ##

pi0.groups1 <- c(0.1, .4)
pi0.groups2 <- c(0.5, 0.5)
pi0.groups3 <- c(0.9, 0.1)

test.oracle.1 <- Oracle.GBH(unadjp.1, groups.1, pi0.groups1, 0.05)
test.oracle.2 <- Oracle.GBH(unadjp.1, groups.1, pi0.groups2, 0.05)
test.oracle.3 <- Oracle.GBH(unadjp.1, groups.1, pi0.groups3, 0.05)
test.oracle.4 <- Oracle.GBH(unadjp.2, groups.2, pi0.groups1, 0.05)
test.oracle.5 <- Oracle.GBH(unadjp.2, groups.2, pi0.groups2, 0.05)
test.oracle.6 <- Oracle.GBH(unadjp.2, groups.2, pi0.groups3, 0.05)
