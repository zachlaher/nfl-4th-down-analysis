# =============================================================================
# analysis.R — Run full pipeline
# =============================================================================

message("=== NFL 4th Down Efficiency Analysis (v2) ===\n")

source("R/00_packages.R")
source("R/01_load_and_parse.R")
source("R/02_feature_engineering.R")
source("R/03_eda.R")
source("R/04_model.R")
source("R/05_coach_analysis.R")

message("\n=== Done. All outputs in output/ ===")
