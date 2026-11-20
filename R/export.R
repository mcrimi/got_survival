# Export the fitted model and supporting numbers as JSON for the web page.
#
# The page predicts in JavaScript. To make "the shipped model is the fitted
# model" testable, the fixtures are computed in R *from the JSON as written*
# (after rounding), and a JavaScript test must reproduce them.

JSON_DIGITS <- 8
FIXTURE_TIMES <- c(10, 30)

write_json <- function(x, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(x, path, digits = I(JSON_DIGITS), auto_unbox = TRUE,
                       null = "null", na = "null", pretty = FALSE)
  path
}

horizon_hrs <- function(dat) max(dat$time_hrs)

#' Nonparametric bootstrap of the deployed model: refit on resampled
#' characters, keeping each replicate's coefficients and its baseline
#' cumulative hazard on an hourly grid.
bootstrap_model <- function(dat, model, B = 200, seed = 2020) {
  grid <- c(0:floor(horizon_hrs(dat)), horizon_hrs(dat))
  refit <- switch(model$type,
    cox = function(d) suppressWarnings(fit_cox(d)),
    enet = function(d) refit_enet(d, model$alpha, model$lambda)
  )
  coef <- matrix(NA_real_, B, length(model$coef), dimnames = list(NULL, names(model$coef)))
  cumhaz <- matrix(NA_real_, B, length(grid))
  withr::with_seed(seed, {
    for (b in seq_len(B)) {
      m <- refit(dat[sample.int(nrow(dat), replace = TRUE), ])
      coef[b, ] <- m$coef
      cumhaz[b, ] <- baseline_at(m$baseline, grid)
    }
  })
  list(time = grid, coef = coef, cumhaz = cumhaz)
}

term_table <- function(model, dat) {
  lapply(BASELINE_TERMS, function(term) {
    list(
      name = term, label = TERM_LABELS[[term]],
      levels = lapply(levels(dat[[term]]), function(level) {
        column <- paste0(term, level)
        list(level = level,
             coef = if (column %in% names(model$coef)) model$coef[[column]] else 0,
             n = sum(dat[[term]] == level),
             deaths = sum(dat$dead[dat[[term]] == level]))
      })
    )
  })
}

export_model <- function(model, boot, dat, path) {
  write_json(list(
    meta = list(
      model = model$type, alpha = model$alpha, lambda = model$lambda,
      n = nrow(dat), deaths = sum(dat$dead), horizon_hrs = horizon_hrs(dat),
      boot_replicates = nrow(boot$coef), interval_level = 0.95
    ),
    terms = term_table(model, dat),
    baseline = list(time = model$baseline$time, cumhaz = model$baseline$cumhaz),
    boot = list(time = boot$time, columns = colnames(boot$coef),
                coef = unname(boot$coef), cumhaz = boot$cumhaz)
  ), path)
}

export_characters <- function(dat, path) {
  rows <- lapply(seq_len(nrow(dat)), function(i) {
    r <- dat[i, ]
    out <- list(id = r$id, name = r$name)
    for (term in BASELINE_TERMS) out[[term]] <- as.character(r[[term]])
    out$switched <- as.character(r$switched) == "Yes"
    out$intro_season <- r$intro_season
    out$time_hrs <- r$time_hrs
    out$dead <- r$dead == 1
    out$death <- if (r$dead == 1) {
      list(season = r$death_season, episode = r$death_episode, how = r$death_description)
    }
    out
  })
  write_json(rows, path)
}

km_curve <- function(fit, with_ci = FALSE) {
  s <- summary(fit, censored = FALSE)
  out <- list(time = c(0, s$time), surv = c(1, s$surv))
  if (with_ci) {
    out$lo <- c(1, s$lower)
    out$hi <- c(1, s$upper)
  }
  out
}

export_story <- function(dat, cox, cv_summary, calibration, ph, bias, simulation, deployed, path) {
  y <- survival::Surv(dat$time_hrs, dat$dead)
  overall <- survival::survfit(y ~ 1)
  by <- lapply(stats::setNames(BASELINE_TERMS, BASELINE_TERMS), function(term) {
    km_by_group(survival::survfit(y ~ dat[[term]]))
  })
  logrank <- lapply(stats::setNames(BASELINE_TERMS, BASELINE_TERMS), function(term) {
    test <- survival::survdiff(y ~ dat[[term]])
    stats::pchisq(test$chisq, df = length(test$n) - 1, lower.tail = FALSE)
  })
  write_json(list(
    deployed = deployed,
    median_hrs = unname(summary(overall)$table[["median"]]),
    km = list(overall = km_curve(overall, with_ci = TRUE), by = by),
    logrank = logrank,
    hazard_ratios = hazard_ratios(cox),
    ph_test = ph,
    metrics = cv_summary,
    calibration = calibration,
    trap = list(real = bias, simulation = simulation)
  ), path)
}

# --- Fixtures ---------------------------------------------------------------

#' Rebuild a predicting model from the JSON alone.
model_from_json <- function(path) {
  j <- jsonlite::fromJSON(path, simplifyVector = TRUE, simplifyDataFrame = FALSE)
  coef <- unlist(lapply(j$terms, function(term) {
    v <- vapply(term$levels, function(l) l$coef, numeric(1))
    stats::setNames(v, vapply(term$levels, function(l) paste0(term$name, l$level), character(1)))
  }))
  list(json = j, coef = coef,
       baseline = data.frame(time = j$baseline$time, cumhaz = j$baseline$cumhaz))
}

json_lp <- function(m, profile) {
  sum(m$coef[paste0(names(profile), unlist(profile))])
}

json_survival <- function(m, profile, times) {
  exp(-baseline_at(m$baseline, times) * exp(json_lp(m, profile)))
}

json_interval <- function(m, profile, time) {
  boot <- m$json$boot
  columns <- paste0(names(profile), unlist(profile))
  lp <- rowSums(boot$coef[, match(columns, boot$columns, nomatch = 0), drop = FALSE])
  h <- boot$cumhaz[, findInterval(time, boot$time)]
  level <- m$json$meta$interval_level
  unname(stats::quantile(exp(-h * exp(lp)), c((1 - level) / 2, (1 + level) / 2), type = 7))
}

export_fixtures <- function(model_path, dat, path, n = 60, seed = 2020) {
  m <- model_from_json(model_path)
  horizon <- m$json$meta$horizon_hrs
  times <- c(FIXTURE_TIMES, horizon)
  reference <- lapply(dat[BASELINE_TERMS], function(x) levels(x)[1])
  profiles <- withr::with_seed(seed, lapply(seq_len(n), function(i) {
    lapply(dat[BASELINE_TERMS], function(x) sample(levels(x), 1))
  }))
  profiles <- c(list(reference), profiles)
  write_json(list(
    times = times,
    cases = lapply(profiles, function(p) {
      list(profile = p, lp = json_lp(m, p), surv = json_survival(m, p, times),
           interval = json_interval(m, p, horizon))
    })
  ), path)
}
