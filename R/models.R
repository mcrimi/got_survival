# Model fitting. Every Cox-family model is reduced to the same small object
# (coefficients + Breslow baseline) so that R, the exported JSON and the
# JavaScript on the web page all predict with exactly the same arithmetic.

#' Breslow estimate of the baseline cumulative hazard for a given linear
#' predictor. Returns a data frame with a leading (0, 0) row.
breslow_baseline <- function(time, status, lp) {
  event_times <- sort(unique(time[status == 1]))
  risk <- exp(lp)
  increments <- vapply(event_times, function(t) {
    sum(status == 1 & time == t) / sum(risk[time >= t])
  }, numeric(1))
  data.frame(time = c(0, event_times), cumhaz = c(0, cumsum(increments)))
}

#' A fitted proportional-hazards model in portable form.
linear_model <- function(coef, dat, type, extra = list()) {
  X <- design_matrix(dat)
  full <- stats::setNames(numeric(ncol(X)), colnames(X))
  coef <- coef[!is.na(coef)]
  full[names(coef)] <- coef
  lp <- drop(X %*% full)
  c(list(
    type = type,
    coef = full,
    baseline = breslow_baseline(dat$time_hrs, dat$dead, lp)
  ), extra)
}

linear_predictor <- function(model, newdata) drop(design_matrix(newdata) %*% model$coef)

baseline_at <- function(baseline, times) {
  stats::approx(baseline$time, baseline$cumhaz, xout = times,
                method = "constant", f = 0, rule = 2)$y
}

#' Survival probabilities, one row per character and one column per time.
predict_survival <- function(model, newdata, times) {
  UseMethod("predict_survival")
}

#' @export
predict_survival.default <- function(model, newdata, times) {
  exp(-outer(exp(linear_predictor(model, newdata)), baseline_at(model$baseline, times)))
}

fit_cox <- function(dat) {
  fit <- survival::coxph(baseline_formula(), data = dat, ties = "breslow", x = TRUE)
  linear_model(stats::coef(fit), dat, "cox", extra = list(fit = fit))
}

cox_design_y <- function(dat) survival::Surv(dat$time_hrs, dat$dead)

make_foldid <- function(dat, nfolds, seed) {
  withr::with_seed(seed, {
    id <- integer(nrow(dat))
    for (status in 0:1) {
      idx <- which(dat$dead == status)
      id[idx] <- sample(rep_len(seq_len(nfolds), length(idx)))
    }
    id
  })
}

ENET_ALPHAS <- c(0.05, 0.25, 0.5, 0.75, 1)

#' Elastic-net Cox model. Alpha is tuned over a grid and lambda by
#' cross-validated partial likelihood, with one fixed fold assignment shared by
#' every alpha so the comparison is fair.
fit_enet <- function(dat, alphas = ENET_ALPHAS, nfolds = 10, seed = 2020) {
  X <- design_matrix(dat)
  y <- cox_design_y(dat)
  foldid <- make_foldid(dat, nfolds, seed)
  cvs <- lapply(alphas, function(a) {
    glmnet::cv.glmnet(X, y, family = "cox", alpha = a, foldid = foldid)
  })
  best <- which.min(vapply(cvs, function(cv) min(cv$cvm), numeric(1)))
  cv <- cvs[[best]]
  coef <- stats::setNames(as.numeric(stats::coef(cv, s = "lambda.min")), colnames(X))
  linear_model(coef, dat, "enet", extra = list(
    alpha = alphas[best], lambda = cv$lambda.min,
    tuning = data.frame(alpha = alphas,
                        cv_deviance = vapply(cvs, function(cv) min(cv$cvm), numeric(1)))
  ))
}

#' Refit an elastic net at fixed alpha and lambda (used inside the bootstrap,
#' where re-tuning every replicate would only add noise).
refit_enet <- function(dat, alpha, lambda) {
  X <- design_matrix(dat)
  fit <- glmnet::glmnet(X, cox_design_y(dat), family = "cox", alpha = alpha)
  coef <- stats::setNames(as.numeric(stats::coef(fit, s = lambda)), colnames(X))
  linear_model(coef, dat, "enet")
}

fit_rsf <- function(dat, seed = 2020) {
  fit <- ranger::ranger(
    baseline_formula("survival::Surv(time_hrs, dead)"),
    data = dat[c("time_hrs", "dead", BASELINE_TERMS)],
    num.trees = 500, min.node.size = 15, seed = seed
  )
  structure(list(type = "rsf", fit = fit), class = "rsf_model")
}

#' @export
predict_survival.rsf_model <- function(model, newdata, times) {
  pred <- stats::predict(model$fit, data = newdata[BASELINE_TERMS])
  grid <- c(0, pred$unique.death.times)
  surv <- cbind(1, pred$survival)
  idx <- findInterval(times, grid)
  surv[, idx, drop = FALSE]
}

#' Schoenfeld-residual test of proportional hazards, as a tidy table.
ph_test <- function(cox_model) {
  z <- survival::cox.zph(cox_model$fit)
  data.frame(term = rownames(z$table), chisq = z$table[, "chisq"],
             df = z$table[, "df"], p = z$table[, "p"], row.names = NULL)
}

#' Hazard ratios with Wald intervals from the unpenalised Cox model.
hazard_ratios <- function(cox_model) {
  s <- summary(cox_model$fit)
  ci <- s$conf.int
  data.frame(
    column = rownames(ci), hr = ci[, "exp(coef)"],
    lo = ci[, "lower .95"], hi = ci[, "upper .95"],
    p = s$coefficients[, "Pr(>|z|)"], row.names = NULL
  )
}

#' Sensitivity analysis: the same model on the show clock, with characters
#' entering the risk set when they are introduced (left truncation).
fit_cox_show_clock <- function(dat) {
  survival::coxph(baseline_formula("survival::Surv(intro_hrs, exit_hrs, dead)"),
                  data = dat, ties = "breslow")
}
