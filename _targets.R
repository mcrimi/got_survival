# Pipeline: raw CSV -> cleaned data -> models -> validation -> JSON for the web
# page -> rendered report. Run with `targets::tar_make()`.

library(targets)
library(tarchetypes)

tar_option_set(packages = c("survival", "glmnet", "ranger", "riskRegression"))
tar_source("R")

list(
  tar_target(raw_file, "data/raw/character_data_S01-S08.csv", format = "file"),
  tar_target(characters, clean_characters(load_raw(raw_file))),

  # Models fitted on all characters
  tar_target(cox, fit_cox(characters)),
  tar_target(enet, fit_enet(characters)),
  tar_target(rsf, fit_rsf(characters)),
  tar_target(ph, ph_test(cox)),
  tar_target(cox_show_clock, fit_cox_show_clock(characters)),

  # Validation
  tar_target(cv, cross_validate(characters, repeats = 20, nfolds = 5, seed = 2020)),
  tar_target(cv_summary, summarise_scores(cv$scores)),
  tar_target(deployed_name, select_deployed(cv_summary)),
  tar_target(deployed, list(cox = cox, enet = enet)[[deployed_name]]),
  tar_target(calibration, calibration_table(characters, cv$risk[[deployed_name]], horizon = 30)),
  tar_target(boot, bootstrap_model(characters, deployed, B = 200, seed = 2020)),

  # Immortal-time bias
  tar_target(bias, switch_bias_table(characters)),
  tar_target(simulation, simulate_switching()),

  # Exports for the web page
  tar_target(model_json, export_model(deployed, boot, characters, "web/data/model.json"),
             format = "file"),
  tar_target(characters_json, export_characters(characters, "web/data/characters.json"),
             format = "file"),
  tar_target(story_json,
             export_story(characters, cox, cv_summary, calibration, ph, bias, simulation,
                          deployed_name, "web/data/story.json"),
             format = "file"),
  tar_target(fixtures_json,
             export_fixtures(model_json, characters, "web/test/fixtures.json"),
             format = "file"),

  # Report
  tar_quarto(report, "report/index.qmd", quiet = TRUE)
)
