# CONFIG ----
INPUT_DIR <- Sys.getenv("INPUT_DIR", "logs") # where RDS files live
SITE_DIR <- "site" # Quarto project folder
OUTPUT_DIR <- "docs" # rendered website

fs::dir_create(file.path(SITE_DIR, "reports"))

# Find all RDS files
rds_files <- fs::dir_ls(
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
  pkg = fs::path_file(fs::path_dir(fs::path_dir(fs::path_dir(rds_files)))),
  version = fs::path_file(fs::path_dir(fs::path_dir(rds_files)))
) |>
  # keep only latest
  dplyr::group_by(pkg, version) |>
  dplyr::slice_tail(n = 1) |>
  dplyr::ungroup()

# Load report template
template <- readLines(file.path(SITE_DIR, "report_template.qmd"))

# Generate report pages
purrr::walk(seq_len(nrow(meta)), function(i) {
  rds_path <- meta$rds[i]
  pkg <- meta$pkg[i]
  version <- meta$version[i]

  qmd_name <- glue::glue("reports/{pkg}-{version}.qmd")
  qmd_path <- file.path(SITE_DIR, qmd_name)

  # YAML header with embedded params
  header <- c(
    "---",
    glue::glue('title: "{pkg}/{version}"'),
    "format:",
    "  html:",
    "    toc: false",
    "execute:",
    "  echo: false",
    "engine: knitr",
    "params:",
    glue::glue('  rds_path: "{normalizePath(rds_path, winslash = "/")}"'),
    glue::glue('  title: "{pkg} {version}"'),
    "---",
    ""
  )

  # Remove original YAML from template
  template_body <- template
  if (template_body[1] == "---") {
    end_yaml <- which(template_body[-1] == "---")[1] + 1
    template_body <- template_body[(end_yaml + 1):length(template_body)]
  }

  # Write updated version of the template
  writeLines(c(header, template_body), qmd_path)
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
    glue::glue("      - section: \"{pkg}\""),
    "        contents:"
  )
  versions <- meta$version[meta$pkg == pkg]

  for (v in versions) {
    sidebar_yaml <- c(
      sidebar_yaml,
      glue::glue("          - reports/{pkg}-{v}.qmd")
    )
  }
}

writeLines(
  sidebar_yaml,
  file.path(SITE_DIR, "_sidebar.yml")
)

# delete old OUTPUT_DIR
if (fs::dir_exists(OUTPUT_DIR)) {
  fs::dir_delete(OUTPUT_DIR)
}

old_wd <- setwd(SITE_DIR)
on.exit(setwd(old_wd), add = TRUE)

system("quarto render", intern = FALSE)

message("Website build complete! Output at: ", OUTPUT_DIR)
