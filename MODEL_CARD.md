# Model card: Game of Thrones survival model

## What it is

An elastic-net Cox proportional-hazards model of time to death for characters
in HBO's *Game of Thrones*. It is exported as 18 coefficients and a Breslow
baseline cumulative hazard (`web/data/model.json`) and evaluated in the browser.

- **Outcome:** death on screen. Time is hours of running time since the
  character's first appearance; characters alive at the end of season 8 are
  right-censored.
- **Inputs:** sex, social status, occupation, faith, allegiance. All are
  categorical, treatment-coded, with the most common level as reference.
  Levels with fewer than 10 characters are pooled into "Other".
- **Tuning:** alpha chosen from {0.05, 0.25, 0.5, 0.75, 1} and lambda by
  10-fold cross-validated partial likelihood with fixed folds. Chosen:
  alpha 0.05, lambda 0.231.
- **Uncertainty:** 200 nonparametric bootstrap refits at the chosen alpha and
  lambda; the page shows 95% percentile intervals.

## Intended use

Entertainment, and teaching survival analysis. The model describes a finished
television script. It should not be used to reason about real people.

## Data

359 characters, 212 deaths, from the dataset by Lystad and Brown (see README).

## Performance

20 repeats of stratified 5-fold cross-validation, tuning inside each fold.

| Measure | Value (range across repeats) |
|---|---|
| C-index | 0.617 (0.602 to 0.632) |
| AUC at 10 / 30 / 50 h | 0.654 / 0.621 / 0.572 |
| Integrated Brier score, 0 to 50 h | 0.216 (0.212 to 0.221), against 0.226 with no covariates |

Calibration at 30 hours is good across five risk groups (see the report).
The model discriminates weakly. That is a finding, not a defect to tune away:
these five traits carry little information about who the writers kill.

## Limitations

- **Allegiance is the last one recorded,** not the one at introduction, so for
  the 55 characters who switched sides the model sees where they ended up.
- **Proportional hazards does not hold** for social status and occupation
  (Schoenfeld test). The random survival forest, which does not assume it,
  scores slightly better.
- **Small groups.** Several levels have 13 or 14 characters.
- **The far end of the curve is thin.** Only characters introduced early in
  season 1 can be followed for the full 64 hours.
- **Intervals are too narrow.** They ignore tuning uncertainty.
- **Excluded on purpose:** `prominence` and `allegiance_switched`. Both are
  computed from a character's whole run and leak the outcome (immortal-time
  bias). The dataset does not record when a switch happened, so their real
  effect cannot be estimated.
