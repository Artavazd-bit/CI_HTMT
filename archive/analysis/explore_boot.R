library(dplyr)
options(width = 200)

ERR_DIR <- "results/results_2026_05_06_time_ci/errors"
err_files <- list.files(ERR_DIR, pattern = "\\.rds$", full.names = TRUE)
errall <- do.call(rbind, lapply(err_files, readRDS))

cat(sprintf("Loaded %d error rows from %d files.\n", nrow(errall), length(err_files)))
cat("Columns: ", paste(names(errall), collapse = ", "), "\n\n")

htmt <- errall[errall$estimator == "htmt", ]
cat(sprintf("HTMT rows (one per rep): %d\n\n", nrow(htmt)))

NBOOT <- 1000L
htmt$n_boot_missing <- NBOOT - htmt$n_boot_valid

# --- 1. How many reps have NA n_boot_valid (HTMT block never ran)? ----------
cat("1. reps with n_boot_valid = NA (HTMT block failed entirely):\n")
cat(sprintf("   total: %d of %d (%.2f%%)\n",
            sum(is.na(htmt$n_boot_valid)),
            nrow(htmt),
            100 * mean(is.na(htmt$n_boot_valid))))

cat("\n   by condition (only conditions with any failure):\n")
na_by_cond <- htmt %>%
  group_by(correlation, n, dtype) %>%
  summarize(n_reps    = n(),
            n_na_boot = sum(is.na(n_boot_valid)),
            pct_na    = 100 * n_na_boot / n_reps,
            .groups   = "drop") %>%
  filter(n_na_boot > 0) %>%
  arrange(desc(pct_na))
print(as.data.frame(na_by_cond))

# --- 2. Distribution of n_boot_missing among reps where bootstrap ran -------
cat("\n2. distribution of n_boot_missing (excl. NA reps):\n")
miss <- htmt$n_boot_missing[!is.na(htmt$n_boot_missing)]
cat(sprintf("   reps with bootstrap run: %d\n", length(miss)))
cat(sprintf("   summary:\n"))
print(summary(miss))
cat(sprintf("\n   quantiles 0.50, 0.75, 0.90, 0.95, 0.99, 1.00:\n"))
print(quantile(miss, c(0.50, 0.75, 0.90, 0.95, 0.99, 1.00)))

cat("\n   shares above thresholds (overall, all conditions pooled):\n")
for (thr in c(0, 10, 25, 50, 100, 250, 500, 999)) {
  cat(sprintf("   > %4d missings: %6.2f%%  (%d reps)\n",
              thr, 100 * mean(miss > thr), sum(miss > thr)))
}

# --- 3. Per-condition: max missing seen, to gauge whether 50 is enough -----
cat("\n3. per-condition max(n_boot_missing) -- top 25 worst conditions:\n")
worst <- htmt %>%
  filter(!is.na(n_boot_missing)) %>%
  group_by(correlation, n, dtype) %>%
  summarize(n_reps      = n(),
            max_missing = max(n_boot_missing),
            p95_missing = quantile(n_boot_missing, 0.95),
            p99_missing = quantile(n_boot_missing, 0.99),
            mean_missing = mean(n_boot_missing),
            .groups = "drop") %>%
  arrange(desc(max_missing))
print(as.data.frame(head(worst, 25)))
