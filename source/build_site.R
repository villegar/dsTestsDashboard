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
  "  title: 'DataSHIELD Test Reports'",
  "  sidebar:",
  "    logo: https://i0.wp.com/datashield.org/wp-content/uploads/2024/07/DS-logo-A4.png",
  # "    logo-href: https://datashield.org",
  "    style: docked",
  "    search: false",
  "    contents:"
)

for (pkg in unique(meta$pkg)) {
  sidebar_yaml <- c(
    sidebar_yaml,
    glue::glue("      - section: \"{pkg}\""),
    "        contents:"
  )
  versions <- meta$version[meta$pkg == pkg] |> sort(decreasing = TRUE)
  is_dev <- grepl("dev|rc|beta|alpha", versions, ignore.case = TRUE)
  release_versions <- versions[!is_dev]
  dev_versions <- versions[is_dev]

  # ---- RELEASE SECTION ----
  if (length(release_versions) > 0) {
    sidebar_yaml <- c(
      sidebar_yaml,
      "          - section: \"Release\"",
      "            contents:"
    )

    for (v in sort(release_versions, decreasing = TRUE)) {
      sidebar_yaml <- c(
        sidebar_yaml,
        glue::glue("              - text: \"{v}\""),
        glue::glue("                href: reports/{pkg}-{v}.qmd")
      )
    }
  }

  # ---- DEVELOPMENT SECTION ----
  if (length(dev_versions) > 0) {
    sidebar_yaml <- c(
      sidebar_yaml,
      "          - section: \"Development\"",
      "            contents:"
    )

    for (v in sort(dev_versions, decreasing = TRUE)) {
      sidebar_yaml <- c(
        sidebar_yaml,
        glue::glue("              - text: \"{v}\""),
        glue::glue("                href: reports/{pkg}-{v}.qmd")
      )
    }
  }
}

# load contents of base _quarto.yml
quarto_yml <- readLines(file.path(SITE_DIR, "_quarto_template.yml"))
writeLines(
  c(quarto_yml, sidebar_yaml),
  con = file.path(SITE_DIR, "_quarto.yml")
)

# delete old OUTPUT_DIR
if (fs::dir_exists(OUTPUT_DIR)) {
  fs::dir_delete(OUTPUT_DIR)
}

old_wd <- setwd(SITE_DIR)
on.exit(setwd(old_wd), add = TRUE)

system("quarto render --no-cache", intern = FALSE)

message("Website build complete! Output at: ", OUTPUT_DIR)
