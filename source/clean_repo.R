# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# DELETE OLD LOGS - CONFIG BASED ------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
# commit changes
system('git add -A')
system(
  'git commit -m "Delete old directories not in the config files!"'
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# DELETE OLD LOGS ------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
repo_path <- "logs" # Change to your repo path
days_old <- 5 # Delete directories older than 7 days

# Get full list of directories in the repo (non-recursive)
## level 1: R packages
lvl1_dirs <- list.dirs(path = repo_path, full.names = TRUE, recursive = FALSE)
## level 2: versions
lvl2_dirs <- list.dirs(path = lvl1_dirs, full.names = TRUE, recursive = FALSE)
## logs / results
dirs <- list.dirs(path = lvl2_dirs, full.names = TRUE, recursive = TRUE)
## exclude lvl2_dirs
dirs <- dirs[!dirs %in% lvl2_dirs]

# Get current time
now <- Sys.time()

# Loop through directories and delete if older than X days
for (dir in dirs) {
  # Get last modification time of the directory
  dir_info <- file.info(dir)
  mod_time <- dir_info$mtime

  # Calculate age in days
  age_days <- as.numeric(difftime(now, mod_time, units = "days"))

  # Delete if older than specified days
  if (age_days > days_old) {
    message("Deleting: ", dir, " (", round(age_days, 1), " days old)")
    unlink(dir, recursive = TRUE, force = TRUE)
  }
}

# commit changes
system('git add -A')
system(sprintf(
  'git commit -m "Delete old directories older than %s days"',
  days_old
))
