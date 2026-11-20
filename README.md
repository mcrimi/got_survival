# Who dies in Game of Thrones?

[![CI](https://github.com/mcrimi/got_survival/actions/workflows/ci.yml/badge.svg)](https://github.com/mcrimi/got_survival/actions/workflows/ci.yml)

A survival analysis of all 359 characters in HBO's *Game of Thrones*, built as a
reproducible R pipeline, with the fitted model running in the browser.

- **Try it:** [Would you survive Westeros?](https://marianocrimi.com/got/)
- **Read it:** [the full analysis](https://marianocrimi.com/got/report/)

## What it finds

- 212 of 359 characters die. Half are dead within 29 hours of screen time after
  they first appear; a character introduced in episode one has about a 15%
  chance of reaching the finale.
- From what is known when a character is introduced (sex, social status,
  occupation, faith, allegiance), survival is **only weakly predictable**:
  cross-validated C-index about 0.62. Women, the highborn and Stark followers
  last longer; Boltons and Greyjoys do not.
- The first version of this project (2020) concluded that switching allegiance
  protects you. That was **immortal-time bias**: you have to stay alive to
  switch. A simulation in which switching does nothing reproduces the "effect"
  (hazard ratio 0.31), and a time-dependent analysis removes it (0.95).

## Results

Out-of-sample performance from 20 repeats of stratified 5-fold cross-validation
(mean across repeats):

| Model | C-index | AUC at 30 h | Integrated Brier, 0 to 50 h |
|---|---|---|---|
| No covariates (Kaplan-Meier) | | | 0.226 |
| Cox proportional hazards | 0.615 | 0.624 | 0.222 |
| **Elastic-net Cox (deployed)** | 0.617 | 0.621 | 0.216 |
| Random survival forest | 0.625 | 0.627 | 0.210 |

The forest is slightly better but cannot be shipped as a handful of numbers, so
the elastic net is the model on the web page. See [MODEL_CARD.md](MODEL_CARD.md)
for intended use and limitations.

## How it is built

```
data/raw/character_data_S01-S08.csv
 └─ clean        R/data.R       pool rare groups, set reference levels
     └─ fit      R/models.R     Cox · elastic net · random survival forest
         ├─ validate  R/validate.R   20 × 5-fold cross-validation
         ├─ bias      R/bias.R       immortal-time bias: landmark + simulation
         ├─ report    report/index.qmd   Quarto
         └─ export    R/export.R     web/data/*.json ──→ web page
                                     web/test/fixtures.json ──→ parity test
```

Three things are worth pointing out.

**One prediction path.** Every Cox-family model is reduced to coefficients plus
a Breslow baseline cumulative hazard (`linear_model()` in `R/models.R`). R
predicts from that object, the JSON export is that object, and the JavaScript
in `web/js/model.js` does the same arithmetic.

**Train-to-serve parity is tested.** The pipeline writes `model.json`, reads it
back, and computes predictions for 61 covariate profiles from the JSON alone.
`web/test/model.test.js` asserts that the JavaScript reproduces every one to
1e-7, including the bootstrap intervals.

**No leakage in validation.** Elastic-net tuning happens inside each training
fold. The two covariates that depend on how long a character lived
(`prominence`, `allegiance_switched`) are excluded from the predictive models
and analysed separately.

## Run it

Requires R 4.4 or later, [Quarto](https://quarto.org) and Node 20 or later.

```sh
Rscript -e 'renv::restore()'                    # exact package versions
Rscript -e 'targets::tar_make()'                # whole pipeline, about a minute
Rscript -e 'testthat::test_dir("tests/testthat")'
npm test                                        # JavaScript tests and R-to-JS parity
```

To see the page locally, copy it into a checkout of the website (it borrows the
site's fonts) and serve that:

```sh
scripts/sync_site.sh /path/to/website
```

## Repository layout

| Path | What it holds |
|---|---|
| `R/` | Functions: data cleaning, models, validation, bias analysis, export |
| `_targets.R` | The pipeline |
| `tests/testthat/` | R tests |
| `report/` | Quarto source of the analysis |
| `web/` | The page: plain HTML, CSS and JavaScript modules, no build step |
| `web/data/` | Model, curves and characters exported by the pipeline |
| `web/test/` | JavaScript tests and the R-generated fixtures |
| `data/` | The dataset and its data dictionary |
| `archive/` | The original 2020 notebook and whitepaper |
| `docs/` | Design notes for the 2026 rebuild |

## Data

The data are the *Game of Thrones mortality and survival dataset* by Reidar P.
Lystad and Benjamin T. Brown, published on
[figshare](https://figshare.com/articles/Game_of_Thrones_mortality_and_survival_dataset/8259680/1).
It extends to season 8 the data behind their paper:

> Lystad RP, Brown BT. "Death is certain, the time is not": mortality and
> survival in Game of Thrones. *Injury Epidemiology* 5, 44 (2018).
> https://doi.org/10.1186/s40621-018-0174-7

The dataset is their work and is redistributed here for reproducibility. Check
the figshare record for its licence before reusing it. The MIT licence in this
repository covers the code only.

## History

Version 1 (February 2020) was a course project for a survival analysis class:
one R notebook and a PDF, kept in [`archive/`](archive/). Version 2 (November
2026) is a rebuild. The report lists
[what was wrong with the first version](https://marianocrimi.com/got/report/#what-was-wrong-with-the-2020-version)
and how each problem was fixed.

Not affiliated with HBO.
