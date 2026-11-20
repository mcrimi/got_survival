# Immortal-time bias.
#
# Two columns in the data are summaries of a character's whole run:
#   * switched   - did they ever change allegiance?
#   * prominence - a score built from how many episodes they appeared in.
# A character has to stay alive to switch sides or to rack up episodes, so both
# look protective in a naive model whether or not they are. The data records
# neither *when* a switch happened nor per-episode appearances, so the bias
# cannot be removed on the real data. This file shows its size, shows how far a
# landmark analysis shrinks it, and reproduces the mechanism in a simulation
# where the truth is known.

switch_hr <- function(fit, label) {
  ci <- summary(fit)$conf.int["switchedYes", ]
  data.frame(analysis = label, hr = ci[["exp(coef)"]],
             lo = ci[["lower .95"]], hi = ci[["upper .95"]],
             n = fit$n, deaths = fit$nevent)
}

#' Naive: treat "ever switched" as if it were known on day one.
naive_switch <- function(dat) {
  f <- stats::update(baseline_formula(), . ~ . + switched)
  switch_hr(survival::coxph(f, data = dat, ties = "breslow"), "Naive")
}

#' Landmark: keep only characters still alive `landmark` hours after their
#' introduction and restart the clock there. Deaths before the landmark can no
#' longer be credited to "never switched". Because the flag still includes
#' switches made after the landmark, this is an attenuation, not a correction.
landmark_switch <- function(dat, landmark) {
  alive <- dat[dat$time_hrs > landmark, ]
  alive$time_hrs <- alive$time_hrs - landmark
  # Small landmark sets cannot support the full covariate set; adjust for sex only.
  fit <- survival::coxph(survival::Surv(time_hrs, dead) ~ sex + switched,
                         data = alive, ties = "breslow")
  switch_hr(fit, sprintf("Landmark at %g h", landmark))
}

switch_bias_table <- function(dat, landmarks = c(5, 10, 20)) {
  naive_sex_only <- switch_hr(
    survival::coxph(survival::Surv(time_hrs, dead) ~ sex + switched, data = dat, ties = "breslow"),
    "Naive (adjusted for sex only)"
  )
  rbind(naive_switch(dat), naive_sex_only,
        do.call(rbind, lapply(landmarks, function(l) landmark_switch(dat, l))))
}

#' Simulation with a known answer: switching sides has no effect on the hazard
#' of death (true hazard ratio = 1). Death and switching times are independent
#' exponentials; follow-up ends at `horizon`.
simulate_switching <- function(n = 5000, seed = 2020, mean_death = 20, mean_switch = 30,
                               horizon = 64) {
  withr::with_seed(seed, {
    death <- stats::rexp(n, 1 / mean_death)
    switch_at <- stats::rexp(n, 1 / mean_switch)
  })
  time <- pmin(death, horizon)
  dead <- as.integer(death <= horizon)
  ever <- switch_at < time
  sim <- data.frame(id = seq_len(n), time = time, dead = dead,
                    switched = factor(ifelse(ever, "Yes", "No"), levels = c("No", "Yes")),
                    switch_at = ifelse(ever, switch_at, NA_real_))

  naive <- survival::coxph(survival::Surv(time, dead) ~ switched, data = sim)

  # Correct analysis: each switcher contributes "not yet switched" time before
  # the switch and "switched" time after it.
  td <- survival::tmerge(sim, sim, id = id, death = event(time, dead))
  td <- survival::tmerge(td, sim[ever, ], id = id, switched_now = tdc(switch_at))
  timedep <- survival::coxph(survival::Surv(tstart, tstop, death) ~ switched_now, data = td)

  tidy <- function(fit, label) {
    ci <- summary(fit)$conf.int[1, ]
    data.frame(analysis = label, hr = ci[["exp(coef)"]],
               lo = ci[["lower .95"]], hi = ci[["upper .95"]])
  }
  km <- survival::survfit(survival::Surv(time, dead) ~ switched, data = sim)
  list(
    n = n, true_hr = 1,
    results = rbind(tidy(naive, "Naive: ever switched"),
                    tidy(timedep, "Time-dependent: switched so far")),
    curves = km_by_group(km)
  )
}

#' Split a stratified survfit into one compact curve per group.
km_by_group <- function(km, max_points = 120) {
  s <- summary(km, censored = FALSE)
  groups <- if (is.null(s$strata)) factor(rep("All", length(s$time))) else s$strata
  out <- lapply(split(seq_along(s$time), groups), function(i) {
    keep <- thin_index(length(i), max_points)
    list(time = c(0, s$time[i][keep]), surv = c(1, s$surv[i][keep]))
  })
  names(out) <- sub("^[^=]*=", "", names(out))
  out
}

thin_index <- function(n, max_points) {
  if (n <= max_points) seq_len(n) else unique(round(seq(1, n, length.out = max_points)))
}
