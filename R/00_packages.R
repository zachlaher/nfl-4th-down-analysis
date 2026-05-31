# =============================================================================
# 00_packages.R
# =============================================================================

required_packages <- c(
  "tidyverse",
  "glue",
  "scales",
  "broom",
  "pROC",
  "ggridges",
  "patchwork",
  "janitor",
  "lubridate",
  "ggrepel"   # added for team label plots
)

missing <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing) > 0) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}

invisible(lapply(required_packages, library, character.only = TRUE))
message("[00] Packages loaded.")
