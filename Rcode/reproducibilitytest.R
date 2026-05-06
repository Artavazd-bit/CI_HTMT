# Reproducibility test.
# Run from the project root (working directory containing conditions.rds).
# Picks one saved (task_id, rep_in_batch) row from the cluster output and
# re-runs the same data generation + CI battery using the seed stored with
# that row. The replayed estimates should match the saved estimates exactly.

source("Rcode/replay.R")

# --- Environment audit ----------------------------------------------------
# Reproducibility requires the same R version, the same package versions,
# and L'Ecuyer-CMRG to be the active RNG kind at the moment .Random.seed
# is restored. Print everything that can drift.
locked <- list(R = "4.5.2", lavaan = "0.6-21", covsim = "1.1.0",
               dplyr = "1.2.1", mvtnorm = "1.3-7", MASS = "7.3-65")
loaded <- list(R      = paste(R.version$major, R.version$minor, sep = "."),
               lavaan = as.character(packageVersion("lavaan")),
               covsim = as.character(packageVersion("covsim")),
               dplyr  = as.character(packageVersion("dplyr")),
               mvtnorm = as.character(packageVersion("mvtnorm")),
               MASS    = as.character(packageVersion("MASS")))
cat("Library paths:\n"); cat(paste0("  ", .libPaths()), sep = "\n"); cat("\n")
cat("RNGkind() before restore: ", paste(RNGkind(), collapse = " / "), "\n")
cat("Package versions (loaded vs renv.lock):\n")
for (nm in names(locked)) {
  ok <- identical(loaded[[nm]], locked[[nm]])
  cat(sprintf("  %-7s  loaded=%-8s  locked=%-8s  %s\n",
              nm, loaded[[nm]], locked[[nm]], if (ok) "OK" else "MISMATCH"))
}
cat("\n")

results_dir   <- "results/results_2026_05_01"
task_id       <- 1L
rep_in_batch  <- 1L
nboot         <- 1000L

ci_file  <- file.path(results_dir, "ci", sprintf("ci_task_%05d.rds", task_id))
ci_saved <- readRDS(ci_file)
saved    <- ci_saved[ci_saved$task_id == task_id &
                     ci_saved$rep_in_batch == rep_in_batch, ]
# Anchor the comparison to the 95% rows (the only level that existed in
# pre-multi-conf-level result drops). Old result files without a conf_level
# column are treated as 95% rows.
if ("conf_level" %in% names(saved)) {
  saved <- saved[saved$conf_level == 0.95, ]
}
saved    <- saved[order(match(paste(saved$estimator, saved$method),
                              c("cfa wald_cfa", "htmt delta",
                                "htmt perc", "htmt bc", "htmt bca"))), ]

rep <- replay_rep(task_id        = task_id,
                  rep_in_batch   = rep_in_batch,
                  conditions_file = "conditions.rds",
                  results_dir    = results_dir,
                  nboot          = nboot,
                  run_battery    = TRUE)

replayed <- rep$battery$ci
if ("conf_level" %in% names(replayed)) {
  replayed <- replayed[replayed$conf_level == 0.95, ]
}
replayed <- replayed[order(match(paste(replayed$estimator, replayed$method),
                                 c("cfa wald_cfa", "htmt delta",
                                   "htmt perc", "htmt bc", "htmt bca"))), ]

compare <- data.frame(
  estimator      = saved$estimator,
  method         = saved$method,
  est_saved      = saved$estimate,
  est_replay     = replayed$estimate,
  est_diff       = replayed$estimate   - saved$estimate,
  lower_saved    = saved$lowerbound,
  lower_replay   = replayed$lowerbound,
  lower_diff     = replayed$lowerbound - saved$lowerbound,
  upper_saved    = saved$upperbound,
  upper_replay   = replayed$upperbound,
  upper_diff     = replayed$upperbound - saved$upperbound,
  stringsAsFactors = FALSE
)

cat("\n--- Saved vs replayed CIs (task_id =", task_id,
    ", rep_in_batch =", rep_in_batch, ") ---\n")
print(compare, row.names = FALSE, digits = 8)

num_saved   <- as.matrix(saved[,    c("estimate", "lowerbound", "upperbound")])
num_replay  <- as.matrix(replayed[, c("estimate", "lowerbound", "upperbound")])
cat("\nall.equal(saved, replayed): ",
    isTRUE(all.equal(num_saved, num_replay, check.attributes = FALSE)), "\n")
cat("max |diff|: ", max(abs(num_saved - num_replay), na.rm = TRUE), "\n")
