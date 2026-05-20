library(dplyr)

# Builds `resag`, consumed by analysis/plots_popcov.R and analysis/plots_rejrate.R.

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
ci_estimator_grp <- c(wald_cfa = "cfa", wald_cfa_robust = "cfa_robust",
                      delta = "htmt", perc = "htmt", bc = "htmt", bca = "htmt")
dfall$estimator_grp <- ci_estimator_grp[dfall$method]

# Left-join errors onto CI on (task_id, rep_in_batch, estimator_grp). One-to-many.
err_join <- errall[errall$scope == "uncon" | is.na(errall$scope), c("task_id", "rep_in_batch", "estimator",
                       "error_message", "warning_message")]
err_join <- dplyr::rename(err_join, estimator_grp = estimator)
# left join
dfall <- merge(dfall, err_join,
               by = c("task_id", "rep_in_batch", "estimator_grp"),
               all.x = TRUE, sort = FALSE)

dfall$upperwithin <- dfall$correlation < dfall$upperbound
dfall$lowerwithin <- dfall$correlation > dfall$lowerbound
dfall$coverageone <- (1 > dfall$lowerbound) & (1 < dfall$upperbound)

# Clean-rep filter: require finite estimate + bounds; for CFA/CFA-MLR drop on
# any error OR warning; for HTMT drop only on hard errors 
err_clean <- ifelse(dfall$estimator_grp == "htmt",
                    is.na(dfall$error_message),
                    is.na(dfall$error_message) & is.na(dfall$warning_message))
finite_ok <- is.finite(dfall$estimate) &
             is.finite(dfall$lowerbound) &
             is.finite(dfall$upperbound)
dfall2 <- dfall[err_clean & finite_ok, ]

message(sprintf(
  "Clean rows: %d (dropped %d on error/warning; %d further on non-finite bounds).",
  nrow(dfall2), sum(!err_clean), sum(err_clean & !finite_ok)))

resag <- dfall2 %>%
  group_by(correlation, n, dtype, method, conf_level) %>%
  summarize(upperwithin = mean(upperwithin) * 100,
            lowerwithin = mean(lowerwithin) * 100,
            covagoneag  = mean(coverageone) * 100,
            time_mean = mean(time),
            .groups = "drop")

method_labels <- c(perc = "Percentile", delta = "Asymptotic",
                   bca = "BCa", bc = "BC",
                   wald_cfa = "CFA-ML", wald_cfa_robust = "CFA-MLR")
resag$method2 <- factor(method_labels[resag$method], levels = method_labels)

resag$correlation <- format(resag$correlation, nsmall = 2)
resag$correlation <- paste("Phi ==", resag$correlation)

