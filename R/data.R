# Loading and cleaning the character data.

#' Covariates known when a character is introduced. These are the only inputs
#' to the predictive models; see R/bias.R for why the others are left out.
BASELINE_TERMS <- c("sex", "social_status", "occupation", "religion", "allegiance")

TERM_LABELS <- c(
  sex = "Sex", social_status = "Social status", occupation = "Occupation",
  religion = "Faith", allegiance = "Allegiance"
)

#' Smallest group that keeps its own factor level; smaller ones join "Other".
MIN_LEVEL_N <- 10

load_raw <- function(path) {
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  # The file carries trailing empty columns (X, X.1, ...) from a spreadsheet export.
  raw <- raw[, !grepl("^X(\\.\\d+)?$", names(raw)), drop = FALSE]
  raw[!is.na(raw$id), , drop = FALSE]
}

# Pool rare levels into `other`, then put the most common level first so it is
# the reference category.
pool_and_order <- function(x, other = "Other", min_n = MIN_LEVEL_N) {
  x <- as.character(x)
  counts <- table(x)
  rare <- names(counts)[counts < min_n]
  x[x %in% rare] <- other
  counts <- sort(table(x), decreasing = TRUE)
  factor(x, levels = names(counts))
}

decode <- function(x, map) unname(map[as.character(x)])

clean_characters <- function(raw) {
  stopifnot(!anyNA(raw$exp_time_sec), all(raw$exp_time_sec > 0), all(raw$dth_flag %in% 0:1))

  out <- data.frame(
    id = raw$id,
    name = trimws(raw$name),
    sex = decode(raw$sex, c("1" = "Male", "2" = "Female")),
    social_status = decode(raw$social_status, c("1" = "Highborn", "2" = "Lowborn")),
    occupation = decode(raw$occupation, c(
      "1" = "Silk collar", "2" = "Boiled leather collar", "9" = "Unknown"
    )),
    religion = decode(raw$religion, c(
      "1" = "Great Stallion", "2" = "Lord of Light", "3" = "Faith of the Seven",
      "4" = "Old Gods", "5" = "Drowned God", "6" = "Many-Faced God",
      "7" = "Other", "9" = "Unknown"
    )),
    allegiance = decode(raw$allegiance_last, c(
      "1" = "Stark", "2" = "Targaryen", "3" = "Night's Watch", "4" = "Lannister",
      "5" = "Greyjoy", "6" = "Bolton", "7" = "Frey", "8" = "Other", "9" = "Unclear"
    )),
    switched = decode(raw$allegiance_switched, c("1" = "No", "2" = "Yes")),
    prominence = raw$prominence,
    intro_season = raw$intro_season,
    # Show clock: hours of running time since the first frame of season 1.
    intro_hrs = raw$intro_time_sec / 3600,
    # Character clock: hours of running time since the character first appeared.
    time_hrs = raw$exp_time_sec / 3600,
    dead = as.integer(raw$dth_flag),
    death_season = raw$dth_season,
    death_episode = raw$dth_episode,
    death_description = raw$dth_description,
    stringsAsFactors = FALSE
  )
  stopifnot(!anyNA(out[BASELINE_TERMS]), !anyNA(out$switched))

  for (term in BASELINE_TERMS) out[[term]] <- pool_and_order(out[[term]])
  out$switched <- factor(out$switched, levels = c("No", "Yes"))
  out$exit_hrs <- out$intro_hrs + out$time_hrs
  out
}

baseline_formula <- function(lhs = "survival::Surv(time_hrs, dead)") {
  stats::as.formula(paste(lhs, "~", paste(BASELINE_TERMS, collapse = " + ")))
}

#' Treatment-coded design matrix for the baseline covariates, no intercept.
#' Column names are "<term><level>", e.g. "sexFemale".
design_matrix <- function(dat) {
  mm <- stats::model.matrix(
    stats::as.formula(paste("~", paste(BASELINE_TERMS, collapse = " + "))),
    data = dat[BASELINE_TERMS]
  )
  mm[, -1, drop = FALSE]
}
