
# master file for updating plots and tables, prep files prepare the data, 
# plot files produce png stored in output subfolder
# table files produce LaTeX tables stored in output subfolder
if (!file.exists("CI_HTMT.Rproj")) {
  stop("updateplotstables.R must be run from the repo root ",
       "(working directory should contain CI_HTMT.Rproj). Current wd: ",
       getwd())
}

RESULTS_DIR <- "results/results_2026_05_14"

scripts <- c(
  "analysis/prep_ci.R",
  "analysis/plots_popcov.R",
  "analysis/plots_rejrate.R",
  "analysis/tables_time.R",
  "analysis/prep_problems.R",
  "analysis/plots_problems.R",
  "analysis/prep_boot_validity.R",
  "analysis/tables_boot_validity.R",
  "analysis/prep_jack_validity.R",
  "analysis/tables_jack_validity.R",
  "analysis/rep_HTMT.R"
)

t0 <- Sys.time()
for (s in scripts) {
  message("\n==> ", s)
  source(s)
}

message(sprintf("\nDone in %.1fs. Outputs under outputs/plots and outputs/tables.",
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
