test_that("the simulation reproduces immortal-time bias and its cure", {
  sim <- simulate_switching(n = 5000, seed = 2020)
  naive <- sim$results[1, ]
  proper <- sim$results[2, ]
  # Truth: switching has no effect (HR = 1).
  expect_lt(naive$hi, 0.5)
  expect_lt(proper$lo, 1)
  expect_gt(proper$hi, 1)
})

test_that("a landmark removes the early deaths and moves the estimate towards 1", {
  tab <- switch_bias_table(characters, landmarks = 10)
  naive <- tab[tab$analysis == "Naive (adjusted for sex only)", ]
  landmark <- tab[tab$analysis == "Landmark at 10 h", ]
  expect_lt(landmark$n, naive$n)
  expect_gt(landmark$hr, naive$hr)
})
