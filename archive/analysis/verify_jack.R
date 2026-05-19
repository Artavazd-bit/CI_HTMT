library(dplyr)
if (!exists("RESULTS_DIR", inherits = TRUE)) RESULTS_DIR <- "results/results_2026_05_13"
ERR_DIR <- file.path(RESULTS_DIR, "errors")
ef <- list.files(ERR_DIR, pattern = "\\.rds$", full.names = TRUE)
errall <- do.call(rbind, lapply(ef, readRDS))
htmt <- errall[errall[["estimator"]] == "htmt", ]
htmt[["n_jack_missing"]] <- htmt[["n"]] - htmt[["n_jack_valid"]]

cat("Total HTMT reps:", nrow(htmt), "\n")
cat("Reps with NA n_jack_valid:", sum(is.na(htmt[["n_jack_valid"]])), "\n\n")

cat("Distribution of n_jack_missing across conditions (n x dtype):\n")
overview <- htmt %>%
  group_by(n, dtype, correlation) %>%
  summarise(n_reps   = n(),
            zero_miss = mean(n_jack_missing == 0) * 100,
            any_miss  = sum(n_jack_missing > 0),
            mean_miss = mean(n_jack_missing),
            max_miss  = max(n_jack_missing),
            .groups   = "drop")
print(overview, n = Inf)

cat("\n--- Per (correlation, n, dtype) cell with any missings ---\n")
percell <- htmt %>%
  group_by(correlation, n, dtype) %>%
  summarise(n_reps    = n(),
            zero_miss = mean(n_jack_missing == 0) * 100,
            mean_miss = mean(n_jack_missing),
            max_miss  = max(n_jack_missing),
            .groups   = "drop") %>%
  arrange(correlation, n, dtype)
print(percell, n = Inf)

cat("\n--- Distribution of jack_missing values where >0, by n x dtype ---\n")
nonzero <- htmt %>% filter(n_jack_missing > 0)
if (nrow(nonzero) > 0) {
  print(nonzero %>% group_by(n, dtype) %>%
          summarise(min = min(n_jack_missing),
                    q25 = quantile(n_jack_missing, 0.25),
                    median = median(n_jack_missing),
                    q75 = quantile(n_jack_missing, 0.75),
                    max = max(n_jack_missing),
                    n_affected = n(),
                    .groups = "drop"), n = Inf)
}

cat("\n--- For n=25, full table of n_jack_missing values ---\n")
print(table(htmt$n_jack_missing[htmt$n == 25], htmt$dtype[htmt$n == 25]))
