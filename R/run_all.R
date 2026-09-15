#### Convenience runner: regenerate every figure and table --------------------
#
# Run from the repo root: Rscript R/run_all.R
#
# Runs each script fresh in its own process (Rscript) rather than sourcing
# them into one session, so a failure in one figure doesn't leave stale
# objects behind for the next. Requires DATA_DIR to already contain the
# downloaded GEE exports - see README.md.
#
# Author: Zander Venter

scripts <- c(
  "R/01_figure1_overview.R",
  "R/02_table1_accounting.R",
  "R/03_figure2_country_maps.R",
  "R/04_figure3_distance_gradients.R",
  "R/05_figure4_biome_panels.R",
  "R/06_figure5_l3_drivers.R",
  "R/07_figureS2_bivariate_maps.R"
)

for (s in scripts) {
  message("==> Running ", s)
  status <- system2("Rscript", shQuote(s))
  if (status != 0) stop("Failed: ", s, call. = FALSE)
}

message("All figures and tables written to OUTPUTS_DIR (see .env).")
