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

# utilitarian functions ----
# identify directories containing both:
# - coveragelist.csv
# - test_results.xml
has_covr_tests <- function(d) {
  sub_files <- list.files(d)
  idx <- c("test_results.xml", "coveragelist.csv") %in% sub_files
  # idx_2 <- "index.html" %in% sub_files
  all(idx) #, !idx_2)
}

# validate XML file (i.e., is it readable?)
validate_xml <- function(xml_path) {
  tryCatch(
    {
      xml_path |> xml2::read_xml()
      return(TRUE)
    },
    error = function(e) {
      return(FALSE)
    }
  )
}

# dependencies
# install.packages(c("glue", "kableExtra", "knitr", "purrr", "quarto"))

# remove invalid XMLs
list.files(
  INPUT_DIR,
  ".xml",
  ignore.case = TRUE,
  full.names = TRUE,
  recursive = TRUE
) |>
  purrr::walk(function(xml_path) {
    if (!validate_xml(xml_path)) {
      message("Deleting: ", xml_path)
      unlink(xml_path)
    }
  })

# list directories inside the INPUT_DIR directory
logs_dirs_packages <- list.dirs(INPUT_DIR, recursive = FALSE)

# list sub-directories
logs_dirs_versions <- list.dirs(logs_dirs_packages, recursive = FALSE)

find_latest_version <- function(d) {
  if (!has_covr_tests(d)) {
    # list sub-directories: multiple reports for the same version
    sub_dirs <- list.dirs(d, recursive = FALSE)
    if (length(sub_dirs) == 0) {
      return(NULL)
    }
    # check which of the reports have coverage and test results
    idx <- purrr::map_lgl(sub_dirs, has_covr_tests)
    sub_dirs_2 <- sub_dirs[idx]
    # return path to latest report
    return(tibble::tibble(path = d, latest = sub_dirs_2[length(sub_dirs_2)]))
  } else {
    return(tibble::tibble(path = d, latest = d))
  }
}

logs_dirs_versions |>
  purrr::map(find_latest_version) |>
  purrr::list_c() |>
  # dplyr::slice(7) |>
  purrr::pwalk(
    function(path, latest) {
      # setup
      LOGS_INPUT_DIR <- latest
      LOGS_OUTPUT_DIR <- LOGS_INPUT_DIR
      HTML_DIR <- stringr::str_replace(path, INPUT_DIR, OUTPUT_DIR)
      repo <- stringr::str_extract(path, "(?<=logs\\/)(.*)(?=\\/)")
      version <- stringr::str_remove_all(latest, "[^-]*-[^-]*-")
      GH_REPO <- file.path(
        "https://github.com/datashield",
        repo,
        "blob",
        version
      )

      # common patterns for all packages
      ## captures "fn::class::" and "class-function-"
      FN_NAME_PATTERN <- "^[^:]+(?=::)|(?<=-)[^-:]+"
      FN_TEST_CLASS_PATTERN <- "^[^-:]+(?=-)|(?<=::)[^:]+"

      message("==== Processing: ", file.path(repo, version), " ====")
      tryCatch(
        {
          suppressWarnings(suppressMessages({
            RDS_OUTPUT <- file.path(
              LOGS_OUTPUT_DIR,
              paste0(Sys.Date(), "_covr_and_test_results.Rds")
            )
            # parse test report results
            if (!file.exists(RDS_OUTPUT)) {
              glue::glue(
                "Rscript source/parse_test_report.R {LOGS_INPUT_DIR} {LOGS_OUTPUT_DIR} {GH_REPO} {glue::single_quote(FN_NAME_PATTERN)} {glue::single_quote(FN_TEST_CLASS_PATTERN)}"
              ) |>
                system()
            }

            # generate report with Quarto template
            title <- paste0(
              "DataSHIELD tests\\' overview: ",
              basename(dirname(path)),
              "/",
              basename(path)
            )
            glue::glue(
              "R -s -e \"quarto::quarto_render('source/test_report.qmd', execute_params = list(input_dir = '../{LOGS_OUTPUT_DIR}', title = \'{title}\'))\""
            ) |>
              system()

            # delete old version of output
            unlink(HTML_DIR, recursive = TRUE)

            message("Creating output directory: ", HTML_DIR)
            # create output dir in the HTML_DIR directory
            dir.create(HTML_DIR, recursive = TRUE)

            message("Moving report into: ", HTML_DIR)
            # relocate HTML output
            glue::glue("mv source/test_report.html {HTML_DIR}/index.html") |>
              system()

            message("Report saved to: ", HTML_DIR)
          }))
        },
        error = function(e) {
          message("[ERROR] ", e)
        }
      )
    },
    .progress = TRUE
  )
