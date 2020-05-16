test_that(
  "pi0 estimates balanced", {
    unadjp <- c(runif(100, 0, 1), runif(100, 0, 0.01))
    est <- estimate.pi0(unadjp, method = "tst")
    expect_equal(est, 0.5, tolerance = 0.1)

    est <- estimate.pi0(unadjp, method = "lsl")
    expect_equal(est, 0.5, tolerance = 0.1)

    est <- estimate.pi0(unadjp, method = "storey")
    expect_equal(est, 0.5, tolerance = 0.1)
  }
)

test_that(
  "pi0 estimates unbalanced", {
    unadjp <- c(runif(990, 0, 1), runif(10, 0, 0.01))
    est <- estimate.pi0(unadjp, method = "tst")
    expect_gte(est, 0.95)

    est <- estimate.pi0(unadjp, method = "lsl")
    expect_gte(est, 0.95)

    est <- estimate.pi0(unadjp, method = "storey")
    expect_gte(est, 0.95)

    est <- estimate.pi0(unadjp, method = "storey", lambda = 0.7)
    expect_gte(est, 0.95)
  }
)

test_that(
  "Adaptive GBH returns", {
    unadjp <- c(runif(500, 0, 0.01), runif(1500, 0, 1))
    names(unadjp) <- paste("Hyp:", 1:2000)
    groups <- c(sample(1:2, 2000, replace = TRUE))
    result <-  Adaptive.GBH(unadjp, groups, method = "storey", alpha = 0.05)

    expect_s4_class(result, "GBH")
    expect_is(result@pi0, "numeric")
    expect_gt(result@pi0[1], 0.7)

    expect_is(result@p.vals, "data.frame")
    expect_equal(nrow(result@p.vals), 2000)
    expect_lt(result@p.vals$unadjp[1], 1e-4)
    expect_true(result@adaptive)
    expect_output(print(result), "Hyp: ")
    expect_output(summary(result), "Significance across groups: ")
  })


test_that(
  "Adaptive GBH returns (2)", {
    unadjp <- c(runif(100, 0, 0.05), runif(900, 0, 1))
    names(unadjp) <- paste("Hyp:", 1:1000)
    groups <- c(rep(1, 500), rep(2, 500))
    result <- Adaptive.GBH(unadjp, groups, method = "lsl", alpha = 0.05)

    expect_is(result@pi0, "numeric")
    expect_gt(result@pi0[1], 0.7)

    expect_is(result@p.vals, "data.frame")
    expect_equal(nrow(result@p.vals), 1000)
    expect_lt(result@p.vals$unadjp[1], 5e-3)
    expect_true(result@adaptive)
    expect_output(print(result), "Hyp: ")
    expect_output(summary(result), "Significance across groups: ")
  })

test_that(
  "Oracle GBH returns", {
    unadjp <- c(runif(100, 0, 0.05), runif(900, 0, 1))
    groups <- c(rep(1, 500), rep(2, 500))
    names(unadjp) <- paste("Hyp:", 1:1000)
    pi0.groups <- c('1' = 0.1, '2' = .4)

    result <- Oracle.GBH(unadjp, groups, pi0.groups)
    expect_is(result@p.vals, "data.frame")
    expect_equal(nrow(result@p.vals), 1000)
    expect_false(result@adaptive)
    expect_output(print(result), "Hyp: ")
    expect_output(summary(result), "Significance across groups: ")
})

test_that(
  "Oracle GBH returns (2)", {
    unadjp <- c(runif(100, 0, 0.05), runif(900, 0, 1))
    names(unadjp) <- paste("Hyp:", 1:1000)
    groups <- c(rep(1, 500), rep(2, 500))
    pi0.groups <- c('1' = 0.9, '2' = 0.1)

    result <- Oracle.GBH(unadjp, groups, pi0.groups)
    expect_is(result@p.vals, "data.frame")
    expect_equal(nrow(result@p.vals), 1000)
    expect_false(result@adaptive)
    expect_output(print(result), "Hyp: ")
    expect_output(summary(result), "Significance across groups: ")
  })
