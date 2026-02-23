# reads the configuration files under .github to keep only current logs
conf_files <- list.files(".github", pattern = "txt", full.names = TRUE)

conf_files |>
  sapply(function(f) {
    # identify package
    package <- NULL
    if (grepl("dsbaseclient", tolower(basename(f)))) {
      package <- "dsBaseClient"
    } else if (grepl("dsbase", tolower(basename(f)))) {
      package <- "dsBase"
    }

    # read contents
    conf_contents <- read.delim(
      f,
      header = FALSE,
      sep = "=",
      comment.char = "#"
    )

    # create valid paths (still been tested)
    valid_versions <- file.path(package, conf_contents[, 1])

    # list directories in logs for package
    log_versions <- list.dirs(file.path("logs", package), recursive = FALSE)

    # identify old versions
    idx <- !(log_versions %in% file.path("logs", valid_versions))

    if (sum(idx)) {
      message(
        "Deleting the following versions for ",
        package,
        ": \n",
        paste0(" - ", log_versions[idx], "\n")
      )

      unlink(log_versions[idx], recursive = TRUE, force = TRUE)
    }
  })
