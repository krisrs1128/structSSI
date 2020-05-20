library('ape')
library('igraph')
set.seed(130229)

test_that("hfdr returns", {
  tree <- as.igraph(rtree(10))
  V(tree)$name <- paste("hyp", c(1:19))
  tree.el <- get.edgelist(tree)

  unadjp <- c(runif(5, 0, 0.01), runif(14, 0, 1))
  names(unadjp) <- paste("hyp", c(1:19))

  # The hierarchical adjustment procedure applied to this class
  adjust <- hFDR.adjust(unadjp, tree.el)
  expect_s4_class(adjust, "hypothesesTree")
  expect_lt(adjust@p.vals[1, 4], 1e-2)
  expect_equal(adjust@p.vals[1, 4], adjust@p.vals[1, 3])
})

test_that("hfdr plots", {
  tree <- as.igraph(rtree(10))
  V(tree)$name <- paste("hyp", c(1:19))
  tree.el <- get.edgelist(tree)

  unadjp <- c(runif(5, 0, 0.01), runif(14, 0, 1))
  names(unadjp) <- paste("hyp", c(1:19))

  adjust <- hFDR.adjust(unadjp, tree.el)
  expect_output(summary(adjust), "Number of tip discoveries")
  expect_silent(plot(adjust))
})

test_that("returns with warning when nothing significant", {
  tree <- as.igraph(rtree(10))
  V(tree)$name <- paste("hyp", c(1:19))
  tree.el <- get.edgelist(tree)

  unadjp <- rep(1, 19)
  names(unadjp) <- paste("hyp", c(1:19))
  expect_warning(hFDR.adjust(unadjp, tree.el))
  expect_equal(hFDR.adjust(unadjp, tree.el)@p.vals[1, "significance"], "-")
})

test_that("hfdr has no rownames", {
  tree <- as.igraph(rtree(10))
  V(tree)$name <- paste("hyp", c(1:19))
  tree.el <- get.edgelist(tree)

  unadjp <- c(runif(5, 0, 0.01), runif(14, 0, 1))
  names(unadjp) <- paste("hyp", c(1:19))

  adjust <- hFDR.adjust(unadjp, tree.el)
  expect_equal(rownames(adjust@p.vals), as.character(seq_len(19)))
})

test_that("throws error on mismatched names", {
  tree <- as.igraph(rtree(10))
  V(tree)$name <- paste("hyp", c(1:19))
  tree.el <- get.edgelist(tree)
  unadjp <- c(runif(5, 0, 0.01), runif(14, 0, 1))
  names(unadjp) <- paste("different", c(1:19))

  expect_error(hFDR.adjust(unadjp, tree.el))
})

test_that("Works when names are ints", {
  tree <- as.igraph(rtree(10))
  V(tree)$name <- seq_len(19)
  tree.el <- get.edgelist(tree)
  unadjp <- c(runif(5, 0, 0.01), runif(14, 0, 1))
  names(unadjp) <- seq_len(19)

  adjust <- hFDR.adjust(unadjp, tree.el)
  expect_equal(seq_len(19), adjust@p.vals$hypothesisIndex)
})

test_that("names not constant", {
  tree <- as.igraph(rtree(10))
  V(tree)$name <- seq_len(19)
  tree.el <- get.edgelist(tree)
  unadjp <- c(runif(5, 0, 0.01), runif(14, 0, 1))
  names(unadjp) <- seq_len(19)
  adjust <- hFDR.adjust(unadjp, tree.el)
  expect_lte(max(table(adjust@p.vals$hypothesisName)), 1)
})

test_that("hfdr returns", {
  tree <- as.igraph(rtree(50))
  V(tree)$name <- paste("hyp", c(1:99))
  tree.el <- get.edgelist(tree)
  unadjp <- c(runif(10, 0, 0.01), runif(89, 0, 1))
  names(unadjp) <- paste("hyp", c(1:99))

  # The hierarchical adjustment procedure applied to this class
  adjust <- hFDR.adjust(unadjp, tree.el)
  expect_s4_class(adjust, "hypothesesTree")
  expect_lt(adjust@p.vals[1, 3], 1e-2)
  expect_equal(adjust@p.vals[1, 4], adjust@p.vals[1, 3])
})
