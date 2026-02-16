
<!-- README.md is generated from README.Rmd. Please edit that file -->

<a href='https://datashield.org'><img src='https://i0.wp.com/datashield.org/wp-content/uploads/2024/07/DS-logo-A4.png' alt='DataSHIELD logo' align='right' height=70px/></a>

# DataSHIELD packages tests’ status

<!-- badges: start -->

<!-- [![`dsbase` test suite](https://github.com/villegar/dsBase/actions/workflows/dsBase_test_suite.yaml/badge.svg)](https://github.com/villegar/dsBase/actions/workflows/dsBase_test_suite.yaml) -->

<!-- badges: end -->

This repository contains scripts to aggregate
([source/parse_test_report.R](source/parse_test_report.R)) results from
[`{testthat}`](https://cran.r-project.org/package=testthat) and
[`{covr}`](https://cran.r-project.org/package=covr) packages (see the
workflow: <https://github.com/datashield/.github>). There is a script to
render ([source/render_docs.R](source/render_docs.R)) the results
committed by the pipeline to the [logs/](logs/) directory. Also, a
template for a Quarto report
([source/test_report.qmd](source/test_report.qmd)) to present the
results of the tests in a dashboard.

The workflow follows the following steps:

`Repository with unit tests` \>\>\> `Repository with results` \>\>\>
`GitHub pages`

<br />

#### Render locally

1.  Parse `./logs/` and identify directories with a valid `.xml` file
    (`testthat` output).

    ``` r
    source("source/parse_logs.R")
    ```

2.  (Optional) To clear the repo from old intermediate outputs (`Rds`
    files), run the following before step 1:

    ``` r
    # delete old .Rds files in logs
    list.files("logs", ".Rds", ignore.case = TRUE, full.names = TRUE, recursive = TRUE) |>
      unlink()

    # delete old .csv files in logs
    list.files("logs", "results.csv", ignore.case = TRUE, full.names = TRUE, recursive = TRUE) |>
      unlink()
    ```

3.  Render a website with all the test results:

    ``` sh
    Rscript --verbose --vanilla source/render_docs.R docs docs TRUE
    ```

### Notes

- This a new implementation of the `testStatus` repository, see for
  details: <https://github.com/datashield/testStatus/>.
