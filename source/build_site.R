library(fs)
library(glue)
library(quarto)
library(purrr)
library(stringr)

INPUT_DIR <- Sys.getenv("INPUT_DIR", "logs")
SITE_DIR <- "site"

dir_create(file.path(SITE_DIR, "reports"))

# Find all RDS files
rds_files <- dir_ls(
  INPUT_DIR,
  recurse = TRUE,
  glob = "*covr_and_test_results.Rds"
)

if (length(rds_files) == 0) {
  stop("No RDS files found.")
}

# Extract package + version
meta <- tibble::tibble(
  rds = rds_files,
  pkg = path_file(path_dir(path_dir(rds_files))),
  version = path_file(path_dir(rds_files))
)

# Generate report pages
walk2(meta$rds, seq_len(nrow(meta)), function(rds_path, i) {
  pkg <- meta$pkg[i]
  version <- meta$version[i]

  qmd_name <- glue("reports/{pkg}-{version}.qmd")
  qmd_path <- file.path(SITE_DIR, qmd_name)

  file_copy(
    path = file.path(SITE_DIR, "report_template.qmd"),
    new_path = qmd_path,
    overwrite = TRUE
  )

  quarto_render(
    input = qmd_path,
    execute_params = list(
      title = glue("{pkg} {version}"),
      rds_path = normalizePath(rds_path)
    ),
    quiet = TRUE
  )
})

# Build dynamic sidebar grouped by package
sidebar_yaml <- c(
  "website:",
  "  sidebar:",
  "    style: docked",
  "    search: true",
  "    contents:"
)

for (pkg in unique(meta$pkg)) {
  sidebar_yaml <- c(
    sidebar_yaml,
    glue("      - section: \"{pkg}\""),
    "        contents:"
  )
  versions <- meta$version[meta$pkg == pkg]

  for (v in versions) {
    sidebar_yaml <- c(
      sidebar_yaml,
      glue("          - reports/{pkg}-{v}.qmd")
    )
  }
}

writeLines(
  sidebar_yaml,
  file.path(SITE_DIR, "_sidebar.yml")
)

# Render full site
quarto_render(SITE_DIR)
