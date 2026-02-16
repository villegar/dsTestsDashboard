# This script was created to parse the outputs from testthat and covr for
# DataSHIELD packages (e.g., dsBase). The input files are generate via a GHA
# workflow (see URL_TO_GHA_WORKFLOW).
#
# Roberto Villegas-Diaz <r.villegas-diaz@liverpool.ac.uk>
# July 2025

# SETUP ----
args <- commandArgs(trailingOnly = TRUE) # read CL arguments
# 1st argument: INPUT_DIR
if (length(args) >= 1) {
  INPUT_DIR <- args[1]
} else {
  INPUT_DIR <- getwd()
}
# 2nd argument: OUTPUT_DIR
if (length(args) >= 2) {
  OUTPUT_DIR <- args[2]
} else {
  OUTPUT_DIR <- INPUT_DIR
}
# 3rd argument: GH_REPO - GitHub repository URL
if (length(args) >= 3) {
  GH_REPO <- args[3]
} else {
  GH_REPO <- file.path(git2r::remote_url(), "blob/master")
}
# 4th argument: FN_NAME_PATTERN - Function name pattern
if (length(args) >= 4) {
  FN_NAME_PATTERN <- args[4]
} else {
  FN_NAME_PATTERN <- "[^-:.]+"
}
# 5th argument: FN_TEST_CLASS_PATTERN - Function test class pattern
if (length(args) >= 4) {
  FN_TEST_CLASS_PATTERN <- args[5]
} else {
  FN_TEST_CLASS_PATTERN <- "^[a-zA-Z]+-"
}

# HELPER FUNCTIONS ----
valid_url <- function(URL) {
  RCurl::url.exists(URL)
}

# LOAD RESULTS ----
message("Loading results...")
# Load coverage output
covr_csv <- file.path(INPUT_DIR, "coveragelist.csv") |>
  readr::read_csv(
    show_col_types = FALSE,
    skip = 1,
    col_names = c("name", "file_coverage", "total_coverage")
  )

# Load test results
tests_xml <- file.path(INPUT_DIR, "test_results.xml") |>
  xml2::read_xml()

# Parse XML results into tibble
tests_tbl <- tests_xml |>
  xml2::xml_find_all("testsuite") |>
  purrr::map(function(ts) {
    tmp <- tempfile()
    on.exit(unlink(tmp))
    ts |>
      xml2::xml_attrs() |>
      tibble::as_tibble_row() |>
      readr::write_csv(tmp)
    readr::read_csv(tmp, show_col_types = FALSE)
  }) |>
  purrr::list_c()

# AGGREGATE RESULTS ----
message("Aggregating results...")
tests_tbl_v2 <- tests_tbl |>
  # drop rows without actual tests
  dplyr::filter(tests > 0) |>
  # extract details from testsuite
  dplyr::mutate(
    # extract function name
    fn_name = name |>
      stringr::str_remove("(?=::test).*") |>
      stringr::str_remove(FN_TEST_CLASS_PATTERN) |>
      stringr::str_extract(FN_NAME_PATTERN) |>
      stringr::str_remove_all("[\\(\\)]"),
    # extract any additional components to the test file
    fn_name_sub = name |>
      stringr::str_remove("(?=::test).*") |>
      stringr::str_remove(FN_TEST_CLASS_PATTERN) |>
      stringr::str_remove(fn_name) |>
      stringr::str_extract("^[^:-]+") |>
      stringr::str_remove_all("[\\(\\)]") |>
      stringr::str_replace_na(""),
    # fn_name_sub = ifelse(nchar(fn_name_sub) == 0, NA, fn_name_sub),
    # detect if current record has a test class
    has_test_class = name |>
      stringr::str_detect(FN_TEST_CLASS_PATTERN),
    # extract test class (e.g., arg)
    test_class = name |>
      stringr::str_remove(fn_name) |>
      stringr::str_match(FN_TEST_CLASS_PATTERN) |>
      # (\(x) x[,2])()
      (\(.) .[, 1])(),
    test_class = ifelse(has_test_class, test_class, NA)
  ) |>
  # fill in sub-tests with test class
  tidyr::fill(test_class, .direction = "down") |>
  dplyr::mutate(
    # create links to function script and test file
    github_script_link = file.path(
      GH_REPO,
      "R",
      paste0(fn_name, fn_name_sub, ".R")
    ),
    github_test_link = file.path(
      GH_REPO,
      "tests/testthat",
      paste0("test-", test_class, "-", fn_name, fn_name_sub, ".R")
    )
  )

# aggregate results by function name and test class
tests_tbl_v3 <- tests_tbl_v2 |>
  dplyr::group_by(fn_name, fn_name_sub, test_class) |>
  dplyr::mutate(
    tests = sum(tests, na.rm = TRUE),
    skipped = sum(skipped, na.rm = TRUE),
    failures = sum(failures, na.rm = TRUE),
    errors = sum(errors, na.rm = TRUE),
    time = sum(time, na.rm = TRUE),
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    fn_name,
    fn_name_sub,
    test_class,
    timestamp,
    tests:github_test_link,
    -has_test_class
  ) |>
  dplyr::distinct(fn_name, fn_name_sub, test_class, .keep_all = TRUE)

# check/validate URLs
message("Validating URLs...")
tests_tbl_v4 <- tests_tbl_v3 |>
  dplyr::mutate(
    valid_github_script_link = github_script_link |>
      purrr::map(valid_url, .progress = TRUE),
    valid_github_test_link = github_test_link |>
      purrr::map(valid_url, .progress = TRUE),
    github_script_link = ifelse(
      valid_github_script_link,
      github_script_link,
      NA
    ),
    github_test_link = ifelse(valid_github_test_link, github_test_link, NA)
  )

# ADD TEST COVERAGE ----
message("Adding test coverage...")
covr_csv_2 <- covr_csv |>
  dplyr::mutate(
    # extract function name
    fn_name = name |>
      basename() |>
      stringr::str_remove("\\.R") |>
      stringr::str_extract(FN_NAME_PATTERN),
    # review if `fn_name` is missing
    fn_name = ifelse(
      is.na(fn_name),
      name |>
        stringr::str_remove("R\\/*") |>
        stringr::str_remove("\\.R"),
      fn_name
    ),
    # extract any additional components to the test file
    fn_name_sub = name |>
      basename() |>
      stringr::str_remove("\\.R") |>
      stringr::str_extract("^[^:]+") |>
      stringr::str_remove(fn_name),
    # fn_name_sub = ifelse(nchar(fn_name_sub) == 0, NA, fn_name_sub),
  )

# Combine results
covr_and_test_results <- tests_tbl_v4 |>
  dplyr::left_join(covr_csv_2, by = dplyr::join_by(fn_name, fn_name_sub))

# Tidy up results and combine into a wide table
covr_and_test_results_v2 <- covr_and_test_results |>
  dplyr::filter(!is.na(fn_name)) |>
  # subset columns
  dplyr::select(
    fn_name,
    fn_name_sub,
    github_script_link,
    file_coverage,
    test_class,
    github_test_link,
    tests:time
  ) |>
  dplyr::arrange(fn_name) |>
  tidyr::pivot_longer(
    cols = -c(
      fn_name,
      fn_name_sub,
      github_script_link,
      test_class,
      file_coverage
    ),
    values_transform = as.character
  ) |>
  dplyr::group_by(test_class) |>
  dplyr::group_split()

covr_and_test_results_v3 <- covr_and_test_results_v2 |>
  purrr::map(function(x) {
    x |>
      dplyr::distinct() |>
      tidyr::pivot_wider(
        id_cols = c(
          fn_name,
          fn_name_sub,
          github_script_link,
          test_class,
          file_coverage
        )
      ) |>
      dplyr::rename(
        script_url = github_script_link,
        test_url = github_test_link
      )
  })

covr_and_test_results_v3 |>
  readr::write_rds(file.path(
    OUTPUT_DIR,
    paste0(Sys.Date(), "_covr_and_test_results.Rds")
  ))

covr_and_test_results_v3 |>
  purrr::reduce(dplyr::bind_rows) |>
  readr::write_excel_csv(
    file.path(OUTPUT_DIR, paste0(Sys.Date(), "_covr_and_test_results.csv")),
    na = ""
  )
