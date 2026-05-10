library(dplyr)
options(width = 200)

CI_DIR <- "results/results_2026_05_06_time_ci/ci"
ci_files <- list.files(CI_DIR, pattern = "\\.rds$", full.names = TRUE)
dfall <- do.call(rbind, lapply(ci_files, readRDS))

cat(sprintf("Loaded %d rows from %d files.\n\n", nrow(dfall), length(ci_files)))

# --- 1. All task_ids present? ------------------------------------------------
conds <- readRDS("conditions.rds")
expected_tasks <- sort(unique(conds$task_id))
got_tasks      <- sort(unique(dfall$task_id))
missing_tasks  <- setdiff(expected_tasks, got_tasks)
cat("1. expected task_ids:", length(expected_tasks),
    " got:", length(got_tasks),
    " missing:", length(missing_tasks), "\n")
if (length(missing_tasks) > 0L) {
  cat("   missing task_ids: ", paste(head(missing_tasks, 20), collapse = ","),
      if (length(missing_tasks) > 20) " ..." else "", "\n", sep = "")
}

# --- 2. Each task has exactly 1500 rows (100 reps x 5 methods x 3 conf)? ----
rows_per_task <- dfall %>% count(task_id, name = "rows")
bad_tasks <- rows_per_task %>% filter(rows != 1500)
cat(sprintf("\n2. tasks with != 1500 rows: %d\n", nrow(bad_tasks)))
if (nrow(bad_tasks) > 0L) print(as.data.frame(head(bad_tasks, 20)))

# --- 3. Each (task, rep) has exactly 15 rows? -------------------------------
rows_per_rep <- dfall %>% count(task_id, rep_in_batch, name = "rows")
bad_reps <- rows_per_rep %>% filter(rows != 15)
cat(sprintf("\n3. (task, rep) with != 15 rows: %d\n", nrow(bad_reps)))
if (nrow(bad_reps) > 0L) print(as.data.frame(head(bad_reps, 20)))

# --- 4. Each (task, rep) has the expected method/conf combos? ---------------
expected_combo <- expand.grid(
  estimator  = c("cfa", "htmt"),
  method     = c("wald_cfa", "delta", "perc", "bc", "bca"),
  conf_level = c(0.90, 0.95, 0.99),
  stringsAsFactors = FALSE
)
expected_combo <- expected_combo[
  (expected_combo$estimator == "cfa"  & expected_combo$method == "wald_cfa") |
  (expected_combo$estimator == "htmt" & expected_combo$method != "wald_cfa"), ]
cat(sprintf("\n4. expected method/conf combos per rep: %d\n", nrow(expected_combo)))
combo_counts <- dfall %>%
  count(task_id, rep_in_batch, estimator, method, conf_level)
bad_combos <- combo_counts %>% filter(n != 1)
cat(sprintf("   rep-rows with duplicated combos: %d\n", nrow(bad_combos)))

# --- 5. Unexpected estimator/method combos? ---------------------------------
unexpected <- dfall %>%
  distinct(estimator, method) %>%
  anti_join(expected_combo %>% distinct(estimator, method),
            by = c("estimator", "method"))
cat(sprintf("\n5. unexpected estimator/method combos: %d\n", nrow(unexpected)))
if (nrow(unexpected) > 0L) print(unexpected)

# --- 6. Sanity: all conf_levels present per (task, rep, estimator, method) --
miss_conf <- dfall %>%
  count(task_id, rep_in_batch, estimator, method) %>%
  filter(n != 3)
cat(sprintf("\n6. (task, rep, est, method) with != 3 conf_level rows: %d\n",
            nrow(miss_conf)))

# --- 7. NaN vs NA: are there NaN estimates that is.na catches? --------------
n_na_estimate  <- sum(is.na(dfall$estimate))
n_nan_estimate <- sum(is.nan(dfall$estimate))
cat(sprintf("\n7. estimate: NA-or-NaN = %d, of which NaN = %d (rest are pure NA)\n",
            n_na_estimate, n_nan_estimate))

# --- 8. Inf estimates / bounds? (is.na is FALSE for Inf!) -------------------
n_inf_estimate <- sum(is.infinite(dfall$estimate))
n_inf_lower    <- sum(is.infinite(dfall$lowerbound))
n_inf_upper    <- sum(is.infinite(dfall$upperbound))
cat(sprintf("\n8. Inf values (NOT caught by is.na): estimate=%d, lower=%d, upper=%d\n",
            n_inf_estimate, n_inf_lower, n_inf_upper))
if (n_inf_lower > 0 || n_inf_upper > 0) {
  cat("   sample rows with Inf bounds:\n")
  print(as.data.frame(head(dfall[is.infinite(dfall$lowerbound) |
                                 is.infinite(dfall$upperbound),
                                 c("task_id","rep_in_batch","correlation","n",
                                   "dtype","estimator","method","conf_level",
                                   "estimate","lowerbound","upperbound")], 10)))
}

# --- 9. Cross-check: total NA estimates vs problem_estimates -----------------
cat("\n9. cross-check problem_estimates totals:\n")
src_count <- dfall %>%
  group_by(task_id, condition_id, rep_in_batch, estimator) %>%
  summarize(estimate = first(estimate), .groups = "drop") %>%
  summarize(n_failed = sum(is.na(estimate)),
            n_total  = n())
cat(sprintf("   reps total: %d, failed estimates: %d\n",
            src_count$n_total, src_count$n_failed))

source("analysis/prep_problems.R")
cat(sprintf("   problem_estimates n_failed sum: %d (rows: %d, total reps: %d)\n",
            sum(problem_estimates$n_failed),
            nrow(problem_estimates),
            sum(problem_estimates$n_reps)))

# --- 10. Cross-check: total bound failures (estimate ok & bound NA) --------
cat("\n10. cross-check problem_bounds totals:\n")
direct_bf <- sum(!is.na(dfall$estimate) &
                 (is.na(dfall$lowerbound) | is.na(dfall$upperbound)))
cat(sprintf("    direct count of (estimate ok & bound NA/NaN) rows: %d\n",
            direct_bf))
cat(sprintf("    problem_bounds n_bounds_failed sum: %d\n",
            sum(problem_bounds$n_bounds_failed)))

# --- 11. Are bound counts truly identical across conf_levels? ---------------
pb_check <- problem_bounds %>%
  group_by(correlation, n, dtype, estimator, method) %>%
  summarize(distinct_counts = n_distinct(n_bounds_failed),
            counts = paste(sort(unique(n_bounds_failed)), collapse = ","),
            .groups = "drop") %>%
  filter(distinct_counts > 1)
cat(sprintf("\n11. groups where n_bounds_failed differs across conf_levels: %d\n",
            nrow(pb_check)))
if (nrow(pb_check) > 0L) print(as.data.frame(pb_check))

# --- 12. Are there rows where estimate present, only ONE bound NA? ----------
one_sided <- dfall %>%
  filter(!is.na(estimate),
         xor(is.na(lowerbound), is.na(upperbound)))
cat(sprintf("\n12. rows with one bound NA but the other finite: %d\n",
            nrow(one_sided)))
if (nrow(one_sided) > 0L) {
  print(as.data.frame(head(one_sided[, c("task_id","rep_in_batch","estimator",
                                         "method","conf_level","estimate",
                                         "lowerbound","upperbound")], 10)))
}
