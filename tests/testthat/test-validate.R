test_that("out-of-fold risk covers every character exactly once", {
  fitters <- list(cox = function(d) suppressWarnings(fit_cox(d)))
  risk <- oof_risk(characters, nfolds = 5, seed = 1, fitters = fitters)
  expect_equal(dim(risk$cox), c(nrow(characters), length(EVAL_TIMES)))
  expect_false(anyNA(risk$cox))
  expect_true(all(risk$cox >= 0 & risk$cox <= 1))

  scores <- score_repeat(characters, risk)
  expect_setequal(unique(scores$metric), c("ibs", "brier", "auc", "c_index"))
  c_index <- scores$value[scores$metric == "c_index"]
  expect_gt(c_index, 0.5)
  expect_lt(c_index, 0.8)
})

test_that("the deployed model is the Cox-family model with the lower IBS", {
  s <- data.frame(model = c("cox", "enet", "rsf"), metric = "ibs", horizon = 50,
                  mean = c(0.22, 0.21, 0.10))
  expect_equal(select_deployed(s), "enet")
})
