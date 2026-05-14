library(dplyr)

# Builds `problems`, consumed by analysis/plots_problems.R.
#
# Per (condition x method x conf_level), counts the fraction of replications
# that have *any* problem: a missing point estimate or a missing/NaN lower or
# upper bound. For CFA / CFA-MLR a lavaan warning or error is also treated as
# a failed estimate, mirroring the clean-rep filter in prep_ci.R. HTMT keeps
# its raw NA status (the benign jackknife "Negative mean inside sqrt()"
# warning is not promoted to a failure).

if (!exists("RESULTS_DIR", inherits = TRUE)) RESULTS_DIR <- "results/results_2026_05_14"
CI_DIR  <- file.path(RESULTS_DIR, "ci")
ERR_DIR <- file.path(RESULTS_DIR, "errors")

ci_files <- list.files(CI_DIR, pattern = "\\.rds$", full.names = TRUE)
if (length(ci_files) == 0L) stop("No .rds files found in CI_DIR: ", CI_DIR)

err_files <- list.files(ERR_DIR, pattern = "\\.rds$", full.names = TRUE)
if (length(err_files) == 0L) stop("No .rds files found in ERR_DIR: ", ERR_DIR)

dfall  <- do.call(rbind, lapply(ci_files,  readRDS))
errall <- do.call(rbind, lapply(err_files, readRDS))

message(sprintf("Loaded %d CI rows from %d task files (%d unique task_ids).",
                nrow(dfall), length(ci_files), length(unique(dfall$task_id))))

# Map CI method -> estimator group so the join key matches errall$estimator.
# One HTMT error row fans out to 12 CI rows (4 methods x 3 conf_levels) for
# that rep; one CFA-flavour error row fans out to 3 CI rows.
ci_estimator_grp <- c(wald_cfa = "cfa", wald_cfa_robust = "cfa_robust",
                      delta = "htmt", perc = "htmt", bc = "htmt", bca = "htmt")
dfall$estimator_grp <- ci_estimator_grp[dfall$method]

err_join <- errall[errall$scope == "uncon" | is.na(errall$scope),
                   c("task_id", "rep_in_batch", "estimator",
                     "error_message", "warning_message")]
err_join <- dplyr::rename(err_join, estimator_grp = estimator)

dfall <- merge(dfall, err_join,
               by = c("task_id", "rep_in_batch", "estimator_grp"),
               all.x = TRUE, sort = FALSE)

# CFA / CFA-MLR: promote any lavaan warning or error to an estimate failure.
# HTMT: leave estimate as-is (NA only when the computation truly returned NA).
is_cfa <- dfall$estimator_grp %in% c("cfa", "cfa_robust")
has_problem <- !is.na(dfall$error_message) | !is.na(dfall$warning_message)
dfall$estimate[is_cfa & has_problem] <- NA

dfall$is_problem <- is.na(dfall$estimate) |
                    is.na(dfall$lowerbound) |
                    is.na(dfall$upperbound)

problems <- dfall %>%
  group_by(correlation, n, dtype, estimator, method, conf_level) %>%
  summarize(n_reps      = n(),
            n_problem   = sum(is_problem),
            pct_problem = 100 * n_problem / n_reps,
            .groups = "drop")

method_labels <- c(wald_cfa = "CFA", wald_cfa_robust = "CFA-MLR",
                   delta = "Asymptotic", perc = "Percentile",
                   bc = "BC", bca = "BCa")
problems$method2 <- factor(method_labels[problems$method], levels = method_labels)

problems$correlation_lbl <- paste("Phi ==", format(problems$correlation, nsmall = 2))

message(sprintf(
  "problems: %d (condition x method x conf_level) groups, %d with any failure.",
  nrow(problems), sum(problems$n_problem > 0)))
