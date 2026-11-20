# Game of Thrones survival analysis: revamp and web app

Date: 2026-11-20
Repo: `mcrimi/got_survival` (branch `revamp`, default branch `master`)

## Intent

Turn a 2020 course project (one R notebook, a CSV, a PDF) into a portfolio
piece that impresses two audiences in about two minutes:

- **R data scientists**, who will judge the statistics: is the survival
  analysis correct, are the biases handled, is validation honest?
- **ML engineers**, who will judge the engineering: is it reproducible,
  tested, automated, and is the model actually shipped?

The statistics lead. The web app is the proof that the model ships, and the
hook that gets people to read the analysis.

## Goals

1. A corrected, defensible survival analysis, reproducible from one command.
2. A public page at `marianocrimi.com/got/`, inside Mariano's existing
   personal site and in its house style, where a visitor builds a
   character and sees a predicted survival curve, backed by the fitted model.
3. A repo that reads well on GitHub: clear README, structure, CI badge,
   licence, data attribution.

## Non-goals

- No live model server, database, accounts or analytics.
- No book data, no new data collection, no scraping.
- No change to the Portfolio parent repo.
- No redesign of the existing site; the page adopts its style, not the reverse.
- The original PDF whitepaper is kept as a historical artefact, not rewritten.

## Anti-goals

- Statistics that an expert can puncture (leakage, biased covariates,
  optimistic validation). An honest "the signal is weak" beats an inflated AUC.
- A flashy app whose numbers do not match the R model.
- HBO artwork, logos, fonts or character images.
- A page that looks foreign on marianocrimi.com.
- Over-engineering: nothing that needs a second host or ongoing upkeep.

## Constraints

- Data: `character_data_S01-S08.csv`, 359 characters, 212 deaths. Time is
  screen time since the character's introduction (`exp_time_sec`), right
  censored at the end of season 8.
- The data has **no timestamp for when a character switched allegiance** and
  no per-episode appearance data. `allegiance_switched` and `prominence`
  therefore cannot be modelled as true time-dependent covariates.
- Small sparse groups (e.g. 6 followers of the Many-Faced God, 13 Boltons).
- Local machine: R 4.4.0, Node and npm present; Quarto and wrangler are not
  installed. GitHub CLI is authenticated as `mcrimi`.
- Hosting: the page lives in the existing site, private repo
  `mcrimi/website`: a hand-written static site (`index.html`,
  `assets/app.js`, `assets/style.css`, no build step) served as static
  assets by a Cloudflare Worker (`wrangler.jsonc`). Read that repo's
  `AGENTS.md` before touching it. It is assumed to deploy on push to `main`;
  confirm before relying on it.
- House style of the site, binding on the page: e-ink reader look. Only the
  ink and paper CSS variables (`--ink`, `--ink-2`, `--line`, `--paper`), so
  Night mode works; no hard-coded colours, shadows, gradients, rounded
  corners or eased animation (motion is stepped or a flash); self-hosted
  fonts only; no plain `<img>`; commits go small and focused on `main`.
- Dataset provenance must be confirmed and cited (believed to be Lystad &
  Brown, *Injury Epidemiology* 2018, extended to season 8). If the licence
  cannot be confirmed, the README says so plainly and links to the source.

## Approaches considered

**A. Reproducible R pipeline feeding a static site (chosen).** `renv` +
`targets` go from raw CSV to fitted models to exported JSON. A Quarto report
renders from the same pipeline. A small JS front end computes predictions
from the exported model. CI re-runs everything. Free, instant, nothing to keep alive; no live server to show off.

**B. Live model API** (`plumber`/`vetiver` in a container). Shows serving,
but Cloudflare cannot run R, so it needs a second host, cold starts and
upkeep for a model with about ten coefficients. Reads as over-engineering.

**C. R in the browser** (webR/shinylive). Real `survival` code client-side,
which R people enjoy, but a 20-30 MB first load loses the two-minute visitor.

Mariano picked A. The train-to-serve parity test in A covers the "is the
shipped model the real model" question that B and C answer by brute force.

### Where the page lives

Mariano asked for the page to sit on marianocrimi.com itself rather than a
subdomain, respecting the site's aesthetics.

**A. Own page at `marianocrimi.com/got/` (chosen).** Written and tested in
`got_survival/web/` as plain HTML/CSS/JS, then copied into a `got/` folder
of the `website` repo. Reuses the site's fonts and ink/paper variables.
Costs a sync step between two repos.

**B. Native view inside the site's `app.js`.** Seamless, but puts model,
charts and explorer into a 2,300-line file and splits the parity test
across repos.

**C. Subdomain `got.marianocrimi.com`.** Splits traffic and design.

## Design

### 1. Analysis (the core)

Fix what is wrong in `Paper.Rmd`:

- Death plot labels are swapped.
- "Elastic Net" is lasso (default `alpha = 1`) and is cross-validated twice
  with different folds. Fit once, with alpha tuned over a grid, fixed folds.
- The final Cox model drops `allegiance_last` although the chosen formula
  includes it. One model, one justification.
- Single 75/25 split on 359 rows is too noisy. Replace with repeated
  cross-validation or bootstrap optimism correction.

Handle the survival-dependent covariates honestly:

- `prominence` (a function of episodes survived) and `allegiance_switched`
  (you must live long enough to switch) are **excluded from the primary
  model**.
- A dedicated section demonstrates the immortal-time bias: fit the naive
  model, show the misleading effect, then show a **landmark analysis**
  (condition on surviving to a fixed screen-time landmark, classify
  covariates as known at that point where the data allows, model survival
  after it). Because both covariates are end-of-follow-up summaries, the
  landmark version still uses post-landmark information: it is presented
  as an illustration of the effect shrinking, with the explicit verdict
  "still biased, true effect unknown from this data", never as a
  correction.
- A small **simulation** with a known true effect (switching has no effect
  on hazard) is added as a pipeline target. It reproduces the spurious
  protective effect under the naive analysis and its removal under a
  proper time-dependent analysis. This is what the site's "The trap"
  section draws; the real-data comparison sits beside it.

Primary model uses only covariates known at introduction: sex, social
status, occupation, religion, allegiance. Intro season is examined in the
report (it is tied to how long a character can be followed, since everyone
is censored at the end of season 8) but is **not in the deployed model and
not a visitor input**. Allegiance is
`allegiance_last`; the report notes this is last-known rather than
at-introduction and treats it as a limitation.

Models compared:

- Kaplan-Meier and log-rank for description.
- Cox proportional hazards (with Schoenfeld checks; stratify or add
  time interaction where the assumption fails).
- Penalised Cox (elastic net, alpha and lambda tuned).
- Random survival forest as a non-linear benchmark.

Validation, all resampled: Harrell's C, time-dependent AUC at a few
horizons, integrated Brier score, calibration plot. Reported with
uncertainty. A short model card states intended use (entertainment and
teaching), data, metrics and limitations.

**Deployed model:** the penalised or plain Cox model, whichever validates
better, because it exports as coefficients plus a baseline cumulative hazard
and is trivially reproducible in JS. The forest is a benchmark only.

### 2. Pipeline and repo

```
got_survival/
  README.md  LICENSE  CITATION.cff
  data/raw/character_data_S01-S08.csv   data/data_dictionary.pdf
  R/            functions: load, clean, fit, validate, export
  _targets.R    pipeline definition
  renv.lock
  report/       Quarto source for the analysis write-up
  tests/        testthat: data checks, model sanity, export schema
  web/          front end (see below)
  archive/      original Paper.Rmd and PDF (typo in filename fixed)
  .github/workflows/  ci.yml
```

- `targets::tar_make()` runs clean -> fit -> validate -> export -> render.
- Export is `web/data/model.json` (coefficients, factor levels, baseline
  cumulative hazard on a time grid, validation metrics, KM curves by group)
  and `characters.json` (the real characters for the explorer).
- A fixtures file of R predictions for a set of covariate combinations is
  exported too; a JS test asserts the front end reproduces them to 1e-6.
- CI on push: restore renv, run pipeline, run R tests, run JS tests, build
  page (no deploy).

### 3. Web page

One standalone page at `/got/`, plain HTML/CSS/JS with no framework and no
build step, mobile first, in the site's e-ink style: paper background, ink
text, 1px rules, the site's typefaces. Charts are hand-drawn SVG or canvas
in ink only: step curves, 1px lines, groups told apart by line pattern
(solid, dashed, dotted) and direct labels, never by colour. Updates are
stepped or use the site's flash, not eased. Night mode follows the site's
setting. The top bar links back to the main site.

```
+------------------------------------------------------+
|  <- marianocrimi.com                            Aa   |
|------------------------------------------------------|
|  Would you survive Westeros?                         |
|  A survival model fitted on 359 characters.          |
|                                                      |
|  House [v]  Sex [v]  Status [v]  Trade [v]  Faith [v] |
|                                                      |
|  1.0 |__                                             |
|      |  |___ you ________                            |
|      |      |....  all characters . . .              |
|  0.0 +---------------------------------- screen hrs  |
|  You have a 41% chance of seeing the finale          |
|  (interval 28 to 55%). All characters: 29%.          |
|------------------------------------------------------|
|  What the data says: 4-5 short sections, each with   |
|  one curve the reader can switch between groups      |
|------------------------------------------------------|
|  The trap: why "switching sides saves you" is wrong  |
|  (immortal-time bias, drawn step by step)            |
|------------------------------------------------------|
|  Characters: searchable list; each real character's  |
|  fate marked on their predicted curve                |
|------------------------------------------------------|
|  How it was built: pipeline, metrics, links to the   |
|  report, repo and model card                         |
+------------------------------------------------------+
```

- The headline number has one fixed meaning: the visitor's character is
  introduced in the first episode, and "seeing the finale" is the model's
  survival probability at the full screen time of the series (about 64
  hours). The comparison figure is the overall Kaplan-Meier estimate at
  the same horizon, labelled "all characters" (the sketch's numbers are
  placeholders). The page says this in one plain line under the chart.
- The prediction shows uncertainty, and says so when a chosen combination
  is rare in the data.
- Death descriptions from the dataset appear in the character list (they
  are spoilers; a one-line notice at the top says so).
- The full Quarto report is served at `/got/report/`, with a theme reduced
  to the same paper, ink and typefaces as far as Quarto allows.
- The site's existing `got-survival` project entry (in `assets/app.js` and
  the static fallback in `index.html`) is updated to link to `/got/`, and
  `sitemap.xml` and `llms.txt` gain the page.

### 4. Deployment

`got_survival` holds the source and tests. A script builds the page and
copies it into `got/` in a checkout of `mcrimi/website`; the result is
committed there. CI in `got_survival` runs the pipeline and tests but does
not push to the website repo (automation can come later). Pushing to either
repo, and anything that publishes the page, is confirmed with Mariano
first.

## Where the build ends

Done means: a pull request open on `mcrimi/got_survival` from `revamp`
with CI green, and the page committed locally in a checkout of
`mcrimi/website` and checked in a local server at desktop and phone width,
in light and Night paper. Merging, pushing the website and going live are
Mariano's to trigger.

## Calls made by the designer (not decided by Mariano)

- The visitor is always a season 1 arrival; intro season is not an input
  and not in the deployed model.
- "The trap" is carried by a simulation plus an honest real-data
  comparison. Hand-annotating when each of the 55 switchers changed sides
  would give a real answer, but is new data collection and is left out; it
  is the natural follow-up.

- `prominence` and `allegiance_switched` leave the primary model.
- The deployed model is a Cox-family model; the forest is benchmark only.
- Front end: plain JS modules, no bundler, hand-drawn charts, to match how
  the site is built. JS tests run with Node's built-in test runner.
- Report in Quarto rather than R Markdown.
- Path `/got/`; sync by script rather than automated cross-repo push.
- Fonts and CSS variables are copied or referenced from the site's
  `/assets/` rather than redefined.
- Old notebook and PDF move to `archive/`.
- MIT licence for code; data credited to its authors under their terms.

## Left to the builder

- Exact landmark time(s), alpha grid, resampling scheme, AUC horizons.
- How sparse factor levels are pooled.
- Visual design details within the site's house style.
- File and function naming inside `R/` and `web/`.
- Whether CI caches the pipeline store or rebuilds each time.

## Order of work

1. Repo restructure, renv, targets skeleton, data tests.
2. Corrected analysis and validation; Quarto report.
3. Model export and parity fixtures.
4. Web app.
5. CI, then sync into the website repo (with confirmations).
6. README, model card, citation.
