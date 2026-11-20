test_that("the raw file loads without its spreadsheet debris", {
  raw <- load_raw(raw_path)
  expect_equal(nrow(raw), 359)
  expect_false(any(grepl("^X", names(raw))))
  expect_false(anyNA(raw$id))
})

test_that("cleaning keeps every character and every death", {
  expect_equal(nrow(characters), 359)
  expect_equal(sum(characters$dead), 212)
  expect_true(all(characters$time_hrs > 0))
  expect_false(anyNA(characters[BASELINE_TERMS]))
  expect_equal(anyDuplicated(characters$id), 0)
})

test_that("known characters are decoded correctly", {
  jon <- characters[characters$name == "Jon Snow", ]
  expect_equal(as.character(jon$sex), "Male")
  expect_equal(as.character(jon$allegiance), "Night's Watch")
  expect_equal(as.character(jon$social_status), "Highborn")
  expect_equal(jon$dead, 0L)
  expect_equal(as.character(jon$switched), "Yes")
})

test_that("rare levels are pooled and the commonest level is the reference", {
  for (term in BASELINE_TERMS) {
    counts <- table(characters[[term]])
    expect_true(all(counts >= MIN_LEVEL_N), info = term)
    expect_equal(names(counts)[1], names(which.max(counts)), info = term)
  }
  expect_false("Many-Faced God" %in% levels(characters$religion))
})

test_that("a level below the threshold joins Other", {
  x <- c(rep("a", 12), rep("b", 11), rep("c", 3))
  expect_equal(levels(pool_and_order(x)), c("a", "b", "Other"))
})
