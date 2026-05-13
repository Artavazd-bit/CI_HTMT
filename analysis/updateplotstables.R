# Master script: regenerate every plot and table under outputs/ from the
# RDS result files in RESULTS_DIR. Run from the repo root so the relative
# source() / file.path() calls in the analysis scripts resolve correctly.
#
# Each prep / plot / table script remains runnable standalone -- the
# exists()-guarded source() calls in those scripts no-op when this master
# has already populated the global environment.

if (!file.exists("CI_HTMT.Rproj")) {
  stop("updateplotstables.R must be run from the repo root ",
       "(working directory should contain CI_HTMT.Rproj). Current wd: ",
       getwd())
}

RESULTS_DIR <- "results/results_2026_05_13"

scripts <- c(
  "analysis/prep_ci.R",
  "analysis/plots_popcov.R",
  "analysis/plots_rejrate.R",
  "analysis/tables_time.R",
  "analysis/prep_problems.R",
  "analysis/plots_problems.R",
  "analysis/tables_problems.R",
  "analysis/prep_boot_validity.R",
  "analysis/tables_boot_validity.R"
)

t0 <- Sys.time()
for (s in scripts) {
  message("\n==> ", s)
  source(s)
}

message(sprintf("\nDone in %.1fs. Outputs under outputs/plots and outputs/tables.",
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
