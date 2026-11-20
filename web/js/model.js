// The survival model, as exported by the R pipeline (web/data/model.json).
//
// S(t | x) = exp(-H0(t) * exp(x . beta))
//
// H0 is the Breslow baseline cumulative hazard, a step function. Everything
// here is pure arithmetic on the JSON, and web/test/model.test.js checks it
// against predictions computed in R.

/** Profile with every covariate at its reference (most common) level. */
export function referenceProfile(model) {
  return Object.fromEntries(model.terms.map((term) => [term.name, term.levels[0].level]));
}

function findLevel(model, name, level) {
  const term = model.terms.find((t) => t.name === name);
  if (!term) throw new Error(`Unknown covariate: ${name}`);
  const found = term.levels.find((l) => l.level === level);
  if (!found) throw new Error(`Unknown ${name}: ${level}`);
  return found;
}

/** x . beta for a profile such as { sex: "Female", allegiance: "Stark", ... }. */
export function linearPredictor(model, profile) {
  return model.terms.reduce((sum, term) => sum + findLevel(model, term.name, profile[term.name]).coef, 0);
}

/** Value at t of a right-continuous step function; clamps outside the grid. */
export function stepAt(times, values, t) {
  if (!(t >= times[0])) return values[0];
  let lo = 0;
  let hi = times.length - 1;
  while (lo < hi) {
    const mid = (lo + hi + 1) >> 1;
    if (times[mid] <= t) lo = mid;
    else hi = mid - 1;
  }
  return values[lo];
}

export function survivalAt(model, profile, t) {
  const risk = Math.exp(linearPredictor(model, profile));
  return Math.exp(-stepAt(model.baseline.time, model.baseline.cumhaz, t) * risk);
}

/** The whole curve, one point per step of the baseline hazard. */
export function survivalCurve(model, profile) {
  const risk = Math.exp(linearPredictor(model, profile));
  return {
    time: model.baseline.time,
    surv: model.baseline.cumhaz.map((h) => Math.exp(-h * risk)),
  };
}

/** Sample quantile, R's default (type 7). `sorted` must be ascending. */
export function quantile(sorted, p) {
  const h = (sorted.length - 1) * p;
  const lo = Math.floor(h);
  const hi = Math.min(lo + 1, sorted.length - 1);
  return sorted[lo] + (h - lo) * (sorted[hi] - sorted[lo]);
}

function bootLinearPredictors(model, profile) {
  const { columns, coef } = model.boot;
  const picked = model.terms
    .map((term) => columns.indexOf(term.name + findLevel(model, term.name, profile[term.name]).level))
    .filter((i) => i >= 0);
  return coef.map((row) => picked.reduce((sum, i) => sum + row[i], 0));
}

function bootSurvival(model, lps, gridIndex) {
  return model.boot.cumhaz
    .map((row, b) => Math.exp(-row[gridIndex] * Math.exp(lps[b])))
    .sort((a, b) => a - b);
}

function gridIndexAt(model, t) {
  const grid = model.boot.time;
  let i = 0;
  while (i + 1 < grid.length && grid[i + 1] <= t) i += 1;
  return i;
}

/** Bootstrap percentile interval [lo, hi] for S(t | profile). */
export function interval(model, profile, t) {
  const tail = (1 - model.meta.interval_level) / 2;
  const s = bootSurvival(model, bootLinearPredictors(model, profile), gridIndexAt(model, t));
  return [quantile(s, tail), quantile(s, 1 - tail)];
}

/** Interval along the whole bootstrap time grid, for drawing a band. */
export function band(model, profile) {
  const tail = (1 - model.meta.interval_level) / 2;
  const lps = bootLinearPredictors(model, profile);
  const lo = [];
  const hi = [];
  model.boot.time.forEach((_, i) => {
    const s = bootSurvival(model, lps, i);
    lo.push(quantile(s, tail));
    hi.push(quantile(s, 1 - tail));
  });
  return { time: model.boot.time, lo, hi };
}

/** Real characters whose covariates equal the profile exactly. */
export function matchingCharacters(model, characters, profile) {
  return characters.filter((c) => model.terms.every((term) => c[term.name] === profile[term.name]));
}

export function profileOf(model, character) {
  return Object.fromEntries(model.terms.map((term) => [term.name, character[term.name]]));
}
