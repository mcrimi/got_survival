test_that("the Breslow baseline matches survival::basehaz", {
  cox <- fit_cox(characters)
  ref <- survival::basehaz(cox$fit, centered = FALSE)
  expect_equal(baseline_at(cox$baseline, ref$time), ref$hazard, tolerance = 1e-10)
})

test_that("portable predictions match survival's own", {
  cox <- fit_cox(characters)
  times <- c(5, 20, 50)
  ours <- predict_survival(cox, characters[1:25, ], times)
  sf <- survival::survfit(cox$fit, newdata = characters[1:25, ])
  theirs <- t(summary(sf, times = times)$surv)
  expect_equal(unname(ours), unname(theirs), tolerance = 1e-8)
})

test_that("survival predictions are valid probabilities that never rise", {
  for (model in list(fit_cox(characters), fit_enet(characters), fit_rsf(characters))) {
    s <- predict_survival(model, characters, 0:60)
    expect_true(all(s >= 0 & s <= 1))
    expect_true(all(s[, 1] == 1))
    expect_true(all(apply(s, 1, diff) <= 1e-12))
  }
})

test_that("the elastic net is tuned, reproducible and shrinks towards zero", {
  a <- fit_enet(characters)
  b <- fit_enet(characters)
  expect_identical(a$coef, b$coef)
  expect_true(a$alpha %in% ENET_ALPHAS)
  expect_lt(sum(abs(a$coef)), sum(abs(fit_cox(characters)$coef)))
})

test_that("folds are balanced on deaths", {
  fold <- make_foldid(characters, 5, 1)
  deaths <- tapply(characters$dead, fold, sum)
  expect_lte(diff(range(deaths)), 1)
})
