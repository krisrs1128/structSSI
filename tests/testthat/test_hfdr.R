
test_that("hfdr returns", {
  set.seed(130229)
  library('ape')
  library('igraph')
  tree <- as.igraph(rtree(10))

  V(tree)$name <- paste("hyp", c(1:19))
  tree.el <- get.edgelist(tree)

  unadjp <- c(runif(5, 0, 0.01), runif(14, 0, 1))
  names(unadjp) <- paste("hyp", c(1:19))

  # The hierarchical adjustment procedure applied to this class
  adjust <- hFDR.adjust(unadjp, tree.el)
  expect_s4_class(adjust, "hypothesesTree")
  expect_lt(adjust@p.vals[1, 1], 1e-2)
  expect_equal(adjust@p.vals[1, 2], adjust@p.vals[1, 1])
})

test_that("hfdr plots", {
  expect_output(summary(adjust), "Number of tip discoveries")
  expect_silent(plot(adjust))
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
  expect_lt(adjust@p.vals[1, 1], 1e-2)
  expect_equal(adjust@p.vals[1, 2], adjust@p.vals[1, 1])
})
