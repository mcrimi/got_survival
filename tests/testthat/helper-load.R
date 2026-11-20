root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (f in list.files(file.path(root, "R"), full.names = TRUE)) source(f)
raw_path <- file.path(root, "data", "raw", "character_data_S01-S08.csv")
characters <- clean_characters(load_raw(raw_path))
