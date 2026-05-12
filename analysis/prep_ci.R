library(dplyr)

# Builds `resag`, consumed by analysis/plots_popcov.R and analysis/plots_rejrate.R.

CI_DIR <- "results/results_2026_05_11/ci"

ci_files <- list.files(CI_DIR, pattern = "\\.rds$", full.names = TRUE)
if (length(ci_files) == 0L) stop("No .rds files found in CI_DIR: ", CI_DIR)

dfall <- do.call(rbind, lapply(ci_files, readRDS))

message(sprintf("Loaded %d CI rows from %d task files (%d unique task_ids).",
                nrow(dfall), length(ci_files), length(unique(dfall$task_id))))


dfall$upperwithin <- dfall$correlation < dfall$upperbound
dfall$lowerwithin <- dfall$correlation > dfall$lowerbound
dfall$coverageone <- (1 > dfall$lowerbound) & (1 < dfall$upperbound)

dfall2 <- dfall[is.finite(dfall$estimate) &
                is.finite(dfall$lowerbound) &
                is.finite(dfall$upperbound), ]

resag <- dfall2 %>%
  group_by(correlation, n, dtype, method, conf_level) %>%
  summarize(upperwithin = mean(upperwithin) * 100,
            lowerwithin = mean(lowerwithin) * 100,
            covagoneag  = mean(coverageone) * 100,
            time_mean = mean(time),
            .groups = "drop")

method_labels <- c(perc = "Percentile", delta = "Asymptotic",
                   bca = "BCa", bc = "BC",
                   wald_cfa = "CFA", wald_cfa_robust = "CFA-MLR")
resag$method2 <- factor(method_labels[resag$method], levels = method_labels)

resag$correlation <- format(resag$correlation, nsmall = 2)
resag$correlation <- paste("Phi ==", resag$correlation)
