library(dplyr)

# Builds two overview tables of problem cases from the CI result files:
#   problem_estimates: reps where the point estimate (CFA or HTMT) is NA,
#                      grouped per (condition x estimator).
#   problem_bounds:    reps where the estimate is present but lower/upper
#                      bound is NA/NaN, grouped per
#                      (condition x estimator x method x conf_level).
#
# is.na() in R is TRUE for both NA and NaN, so BCa NaN bounds count here.

CI_DIR <- "results/results_2026_05_06_time_ci/ci"

ci_files <- list.files(CI_DIR, pattern = "\\.rds$", full.names = TRUE)
if (length(ci_files) == 0L) stop("No .rds files found in CI_DIR: ", CI_DIR)

dfall <- do.call(rbind, lapply(ci_files, readRDS))

message(sprintf("Loaded %d CI rows from %d task files (%d unique task_ids).",
                nrow(dfall), length(ci_files), length(unique(dfall$task_id))))

# --- Problem 1: estimator failures -------------------------------------------
# Within a rep, the estimate is repeated across (method, conf_level), so
# collapse to one row per (rep x estimator) before counting.
est_per_rep <- dfall %>%
  group_by(task_id, condition_id, rep_in_batch,
           correlation, n, dtype, estimator) %>%
  summarize(estimate = first(estimate), .groups = "drop")

problem_estimates <- est_per_rep %>%
  group_by(correlation, n, dtype, estimator) %>%
  summarize(n_reps     = n(),
            n_failed   = sum(is.na(estimate)),
            pct_failed = 100 * n_failed / n_reps,
            .groups    = "drop") %>%
  arrange(desc(pct_failed), correlation, n, dtype, estimator)

# --- Problem 2: bound failures (conditional on estimate being present) -------
problem_bounds <- dfall %>%
  group_by(correlation, n, dtype, estimator, method, conf_level) %>%
  summarize(n_reps           = n(),
            n_estimate_ok    = sum(!is.na(estimate)),
            n_bounds_failed  = sum(!is.na(estimate) &
                                   (is.na(lowerbound) | is.na(upperbound))),
            pct_bounds_failed = 100 * n_bounds_failed /
                                pmax(n_estimate_ok, 1L),
            .groups = "drop") %>%
  arrange(desc(pct_bounds_failed),
          correlation, n, dtype, estimator, method, conf_level)

message(sprintf(
  "problem_estimates: %d (condition x estimator) groups, %d with any failure.",
  nrow(problem_estimates), sum(problem_estimates$n_failed > 0)))
message(sprintf(
  "problem_bounds:    %d (condition x estimator x method x conf_level) groups, %d with any failure.",
  nrow(problem_bounds), sum(problem_bounds$n_bounds_failed > 0)))
