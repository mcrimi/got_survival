test_that("the exported JSON alone reproduces the fitted model", {
  dir <- withr::local_tempdir()
  model <- fit_enet(characters)
  boot <- bootstrap_model(characters, model, B = 20, seed = 1)
  path <- export_model(model, boot, characters, file.path(dir, "model.json"))
  m <- model_from_json(path)

  expect_equal(m$json$meta$n, 359)
  expect_equal(m$json$meta$deaths, 212)
  expect_equal(vapply(m$json$terms, function(t) t$name, ""), BASELINE_TERMS)
  for (term in m$json$terms) expect_equal(term$levels[[1]]$coef, 0)

  for (i in c(1, 5, 100, 359)) {
    profile <- lapply(characters[i, BASELINE_TERMS], as.character)
    expect_equal(json_survival(m, profile, c(10, 30)),
                 drop(predict_survival(model, characters[i, ], c(10, 30))),
                 tolerance = 1e-6)
    interval <- json_interval(m, profile, m$json$meta$horizon_hrs)
    expect_true(interval[1] <= interval[2] && interval[1] >= 0 && interval[2] <= 1)
  }
})

test_that("fixtures include the all-reference profile with a zero linear predictor", {
  dir <- withr::local_tempdir()
  model <- fit_cox(characters)
  boot <- bootstrap_model(characters, model, B = 10, seed = 1)
  path <- export_model(model, boot, characters, file.path(dir, "model.json"))
  fx <- jsonlite::fromJSON(export_fixtures(path, characters, file.path(dir, "fx.json"), n = 5),
                           simplifyVector = FALSE)
  expect_length(fx$cases, 6)
  expect_equal(fx$cases[[1]]$lp, 0)
})

test_that("characters export carries a death record only for the dead", {
  dir <- withr::local_tempdir()
  rows <- jsonlite::fromJSON(export_characters(characters, file.path(dir, "c.json")),
                             simplifyVector = FALSE)
  expect_length(rows, 359)
  dead <- vapply(rows, function(r) r$dead, logical(1))
  has_death <- vapply(rows, function(r) !is.null(r$death), logical(1))
  expect_identical(dead, has_death)
})
