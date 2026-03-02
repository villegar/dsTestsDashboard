xml7.0 <- "logs/dsBase/7.0.0/22173973834-1-v7.0-dev/test_results.xml"
xml6.3 <- "logs/dsBase/6.3.3/22173973834-1-6.3.3/test_results.xml"

dsBaseClient_1 <- "~/Downloads/test_results.xml"
dsBaseClient_2 <- "~/Downloads/test_results-2.xml"
dsBaseClient_3 <- "~/Downloads/test_results-3.xml"

xml2tbl <- function(xml) {
  xml2::read_xml(xml) |>
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
}

tblagg <- function(tbl) {
  tbl |>
    # drop rows without actual tests
    dplyr::filter(tests > 0) |>
    # extract details from testsuite
    dplyr::mutate(
      # detect if current record has a test class
      has_test_class = stringr::str_detect(name, FN_TEST_CLASS_PATTERN),
      # detect naming style
      is_hyphen_style = stringr::str_detect(name, "^[^-:]+-[^-:]+"),
      # extract function name (handles both `class-fn` and `fn::class::...`)
      fn_name = name |>
        stringr::str_extract(FN_NAME_PATTERN) |>
        stringr::str_remove_all("[\\(\\)]"),
      # extract test class (e.g., arg, smk, etc.)
      test_class = name |>
        stringr::str_extract(FN_TEST_CLASS_PATTERN),
      test_class = dplyr::if_else(has_test_class, test_class, NA_character_),
      # fn_name_sub ONLY for hyphen-style: class-function-sub
      fn_name_sub = dplyr::if_else(
        is_hyphen_style,
        name |>
          # remove "class-function-" prefix (optional trailing dash)
          stringr::str_remove("^[^-:]+-[^-:]+-?") |>
          # keep only the first condition token if present
          stringr::str_extract("^[^:-]+") |>
          stringr::str_remove_all("[\\(\\)]") |>
          tidyr::replace_na(""),
        ""
      )
    ) |>
    dplyr::select(-is_hyphen_style) |>
    dplyr::select(
      name,
      fn_name,
      fn_name_sub,
      test_class,
      tests,
      skipped,
      failures,
      errors
    )
}

# Parse XML results into tibble
(xml7.0_tbl <- xml7.0 |>
  xml2tbl() |>
  tblagg())

(xml6.3_tbl <- xml6.3 |>
  xml2tbl() |>
  tblagg())

(dsBaseClient_1_tbl <- dsBaseClient_1 |>
  xml2tbl() |>
  tblagg())

(dsBaseClient_2_tbl <- dsBaseClient_2 |>
  xml2tbl() |>
  tblagg())

(dsBaseClient_3_tbl <- dsBaseClient_3 |>
  xml2tbl() |>
  tblagg())

"~/Downloads/logs-ref_6/test_results.xml" |>
  xml2tbl() |>
  tblagg() |>
  View()

# - - - - - - - - -
xml6.3_tbl |>
  tblagg()

xml7.0_tbl |>
  tblagg()

dsBaseClient_1_tbl |>
  tblagg()

dsBaseClient_2_tbl |>
  tblagg()

list.files(
  pattern = "results\\.(csv|Rds)$",
  recursive = TRUE,
  full.names = TRUE
) |>
  unlink()
