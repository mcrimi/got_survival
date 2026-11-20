# Out-of-sample validation by repeated, stratified K-fold cross-validation.
# With 359 characters a single train/test split is mostly noise, so every
# number reported comes from predictions made on characters the model had not
# seen, pooled over the folds of a repeat and summarised across repeats.

EVAL_TIMES <- 1:50
REPORT_HORIZONS <- c(10, 30, 50)

model_fitters <- function() {
  list(
    # A level with no deaths in a training fold makes coxph warn about an
    # infinite coefficient. That is the unpenalised model's honest behaviour.
    cox = function(d) suppressWarnings(fit_cox(d)),
    enet = function(d) fit_enet(d, nfolds = 5),
    rsf = function(d) fit_rsf(d)
  )
}

#' Out-of-fold predicted risk (1 - S) for one repeat.
#' Returns a named list of n x length(times) matrices, one per model.
oof_risk <- function(dat, nfolds, seed, times = EVAL_TIMES, fitters = model_fitters()) {
  fold <- make_foldid(dat, nfolds, seed)
  risk <- lapply(fitters, function(f) matrix(NA_real_, nrow(dat), length(times)))
  for (k in seq_len(nfolds)) {
    train <- droplevels_safe(dat[fold != k, ], dat)
    test <- dat[fold == k, ]
    for (m in names(fitters)) {
      model <- fitters[[m]](train)
      risk[[m]][fold == k, ] <- 1 - predict_survival(model, test, times)
    }
  }
  risk
}

# Keep the full set of factor levels in every training fold so the design
# matrix has the same columns everywhere; an absent level simply gets no
# coefficient (0).
droplevels_safe <- function(train, full) {
  for (term in BASELINE_TERMS) train[[term]] <- factor(train[[term]], levels = levels(full[[term]]))
  train
}

score_repeat <- function(dat, risk, times = EVAL_TIMES, horizons = REPORT_HORIZONS) {
  # Score() recognises the response by the bare name `Surv`.
  Surv <- survival::Surv
  sc <- riskRegression::Score(
    risk, formula = Surv(time_hrs, dead) ~ 1, data = dat,
    times = times, metrics = c("auc", "brier"), summary = "ibs",
    cens.model = "km", se.fit = FALSE, conf.int = FALSE, contrasts = FALSE
  )
  auc <- as.data.frame(sc$AUC$score)
  brier <- as.data.frame(sc$Brier$score)
  last <- max(times)

  rows <- list()
  for (m in unique(as.character(brier$model))) {
    b <- brier[brier$model == m, ]
    rows[[length(rows) + 1]] <- data.frame(model = m, metric = "ibs", horizon = last,
                                           value = b$IBS[b$times == last])
    rows[[length(rows) + 1]] <- data.frame(model = m, metric = "brier", horizon = horizons,
                                           value = b$Brier[match(horizons, b$times)])
    if (m %in% names(risk)) {
      a <- auc[auc$model == m, ]
      rows[[length(rows) + 1]] <- data.frame(model = m, metric = "auc", horizon = horizons,
                                             value = a$AUC[match(horizons, a$times)])
      # Harrell's C, using predicted risk at the middle horizon as the score.
      score <- risk[[m]][, which(times == horizons[2])]
      c_index <- survival::concordance(survival::Surv(time_hrs, dead) ~ score,
                                       data = dat, reverse = TRUE)$concordance
      rows[[length(rows) + 1]] <- data.frame(model = m, metric = "c_index", horizon = NA,
                                             value = c_index)
    }
  }
  do.call(rbind, rows)
}

#' Run the full repeated cross-validation.
#' Returns list(scores = long table per repeat, risk = mean out-of-fold risk).
cross_validate <- function(dat, repeats = 20, nfolds = 5, seed = 2020) {
  scores <- list()
  risk_sum <- NULL
  for (r in seq_len(repeats)) {
    risk <- oof_risk(dat, nfolds, seed + r)
    s <- score_repeat(dat, risk)
    s$rep <- r
    scores[[r]] <- s
    risk_sum <- if (is.null(risk_sum)) risk else Map(`+`, risk_sum, risk)
  }
  list(scores = do.call(rbind, scores),
       risk = lapply(risk_sum, function(m) m / repeats),
       repeats = repeats, nfolds = nfolds)
}

#' Mean and spread (2.5th to 97.5th percentile across repeats) of each metric.
summarise_scores <- function(scores) {
  key <- interaction(scores$model, scores$metric, scores$horizon, drop = TRUE, sep = "|")
  out <- lapply(split(scores, key), function(g) {
    data.frame(model = g$model[1], metric = g$metric[1], horizon = g$horizon[1],
               mean = mean(g$value),
               lo = unname(stats::quantile(g$value, 0.025)),
               hi = unname(stats::quantile(g$value, 0.975)))
  })
  # interaction() drops NA horizons, so add the C-index rows separately.
  cidx <- scores[scores$metric == "c_index", ]
  out_c <- lapply(split(cidx, cidx$model), function(g) {
    data.frame(model = g$model[1], metric = "c_index", horizon = NA,
               mean = mean(g$value),
               lo = unname(stats::quantile(g$value, 0.025)),
               hi = unname(stats::quantile(g$value, 0.975)))
  })
  res <- do.call(rbind, c(out, out_c))
  res <- res[!duplicated(res[c("model", "metric", "horizon")]), ]
  rownames(res) <- NULL
  res[order(res$metric, res$horizon, res$model), ]
}

#' The model that goes on the web page: whichever Cox-family model has the
#' lower integrated Brier score. The forest is a benchmark only, because it
#' cannot be shipped as a handful of coefficients.
select_deployed <- function(summary) {
  ibs <- summary[summary$metric == "ibs" & summary$model %in% c("cox", "enet"), ]
  as.character(ibs$model[which.min(ibs$mean)])
}

#' Calibration at one horizon: characters grouped by predicted risk, compared
#' with the Kaplan-Meier risk actually observed in each group.
calibration_table <- function(dat, risk_matrix, horizon, times = EVAL_TIMES, groups = 5) {
  pred <- risk_matrix[, which(times == horizon)]
  breaks <- unique(stats::quantile(pred, seq(0, 1, length.out = groups + 1)))
  bin <- cut(pred, breaks, include.lowest = TRUE, labels = FALSE)
  rows <- lapply(sort(unique(bin)), function(b) {
    d <- dat[bin == b, ]
    km <- summary(survival::survfit(survival::Surv(time_hrs, dead) ~ 1, data = d),
                  times = horizon, extend = TRUE)
    data.frame(group = b, n = nrow(d), predicted = mean(pred[bin == b]),
               observed = 1 - km$surv, lo = 1 - km$upper, hi = 1 - km$lower)
  })
  do.call(rbind, rows)
}
