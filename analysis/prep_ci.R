library(dplyr)

# Builds `resag`, consumed by analysis/plots_popcov.R and analysis/plots_rejrate.R.
# Change CI_DIR when a fresher HPC result drop arrives.

CI_DIR <- "results/results_2026_05_02/ci"

ci_files <- list.files(CI_DIR, pattern = "\\.rds$", full.names = TRUE)
if (length(ci_files) == 0L) stop("No .rds files found in CI_DIR: ", CI_DIR)

dfall <- do.call(rbind, lapply(ci_files, readRDS))

message(sprintf("Loaded %d CI rows from %d task files (%d unique task_ids).",
                nrow(dfall), length(ci_files), length(unique(dfall$task_id))))

# Legacy result drops (pre-multi-conf-level) have no conf_level column; treat
# them as 95% so old plots still render.
if (!"conf_level" %in% names(dfall)) dfall$conf_level <- 0.95

dfall$upperwithin <- dfall$correlation < dfall$upperbound
dfall$lowerwithin <- dfall$correlation > dfall$lowerbound
dfall$coverageone <- (1 > dfall$lowerbound) & (1 < dfall$upperbound)

dfall2 <- dfall[!is.na(dfall$lowerbound), ]

resag <- dfall2 %>%
  group_by(correlation, n, dtype, estimator, method, conf_level) %>%
  summarize(upperwithin = mean(upperwithin) * 100,
            lowerwithin = mean(lowerwithin) * 100,
            covagoneag  = mean(coverageone) * 100,
            .groups = "drop")

method_labels <- c(perc = "Percentile", delta = "Asymptotic",
                   bca = "BCa", bc = "BC", wald_cfa = "cfa")
resag$method2 <- factor(method_labels[resag$method], levels = method_labels)

resag$correlation <- format(resag$correlation, nsmall = 2)
resag$correlation <- paste("Phi ==", resag$correlation)
