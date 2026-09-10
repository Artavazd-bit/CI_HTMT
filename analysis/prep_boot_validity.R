library(dplyr)

# Builds `boot_validity`: per (correlation, n, dtype), the share of reps in
# which the bootstrap distribution had a given number of non-computable HTMT
# resamples (n_boot_missing = 1000 - n_boot_valid). Bins are coarse-on-the-
# tail because the missing-count distribution is highly skewed (median 0,
# but max up to ~983 in n=25, severe conditions).

if (!exists("RESULTS_DIR", inherits = TRUE)) RESULTS_DIR <- "results/results_2026_05_13"
ERR_DIR <- file.path(RESULTS_DIR, "errors")
NBOOT <- 1000L

CI_DIR <- file.path(RESULTS_DIR, "ci")

ci_files <- list.files(CI_DIR, pattern = "\\.rds$", full.names = TRUE)
if (length(ci_files) == 0L) stop("No .rds files found in CI_DIR: ", CI_DIR)

dfall3  <- do.call(rbind, lapply(ci_files,  readRDS))

dfall3 <- dfall3[dfall3$estimator == "htmt" & dfall3$method == "bca" & dfall3$conf_level == 0.9,]


# Single source of truth for the bin definitions (label + predicate).
#BIN_SPECS <- list(
#  list(label = "0",        test = function(x) x == 0),
#  list(label = "1-10",     test = function(x) x >=   1 & x <=   10),
#  list(label = "11-25",    test = function(x) x >=  11 & x <=   25),
#  list(label = "26-50",    test = function(x) x >=  26 & x <=   50),
#  list(label = "51-100",   test = function(x) x >=  51 & x <=  100),
#  list(label = "101-250",  test = function(x) x >= 101 & x <=  250),
#  list(label = "251-1000", test = function(x) x >= 251 & x <= 1000)
#)
BIN_SPECS <- list(
  list(label = "0",        test = function(x) x == 0),
  list(label = "1-25",     test = function(x) x >=   1 & x <=   25),
  list(label = "26-50",    test = function(x) x >=  26 & x <=   50),
  list(label = "51-100",    test = function(x) x >=  51 & x <=  100),
  list(label = "101-500",   test = function(x) x >=  101 & x <=  500),
  list(label = "501-750",  test = function(x) x >= 501 & x <=  750),
  list(label = "751-1000", test = function(x) x >= 751 & x <= 1000)
)
BIN_LABELS <- vapply(BIN_SPECS, `[[`, character(1), "label")

err_files <- list.files(ERR_DIR, pattern = "\\.rds$", full.names = TRUE)
if (length(err_files) == 0L) stop("No .rds files found in ERR_DIR: ", ERR_DIR)

errall <- do.call(rbind, lapply(err_files, readRDS))
htmt <- errall[errall$estimator == "htmt", ]
htmt$n_boot_missing <- NBOOT - htmt$n_boot_valid

htmt_val <- merge(htmt, dfall3,
                  by = c("task_id", "rep_in_batch"))

htmt_val <- htmt_val[!is.na(htmt_val$estimate) & !is.na(htmt_val$lowerbound), ]

# All HTMT reps in this dataset have a recorded n_boot_valid (no NAs);
# fail loudly if a future result set violates that, since it would change
# how the shares should be interpreted.
if (anyNA(htmt_val$n_boot_missing)) {
  stop(sprintf("Unexpected NA n_boot_valid in %d HTMT rows; revisit the ",
               "binning scheme to add an explicit 'HTMT failed' bucket.",
               sum(is.na(htmt_val$n_boot_missing))))
}

boot_validity <- htmt_val %>%
  mutate(dtype.x = factor(dtype.x, levels = c("normal", "moderate", "severe"))) %>%
  group_by(correlation.x, dtype.x, n.x) %>%
  group_modify(~ {
    shares <- vapply(BIN_SPECS,
                     function(b) 100 * mean(b$test(.x$n_boot_missing)),
                     numeric(1))
    out <- data.frame(n_reps = nrow(.x), check.names = FALSE)
    for (i in seq_along(BIN_LABELS)) out[[BIN_LABELS[i]]] <- shares[i]
    out
  }) %>%
  ungroup() %>%
  arrange(correlation.x, n.x, dtype.x)

# Sanity: rows must sum to 100% (within float tolerance), confirming the
# bins are exhaustive and non-overlapping.
row_sums <- rowSums(as.matrix(boot_validity[, BIN_LABELS]))
if (any(abs(row_sums - 100) > 1e-9)) {
  stop("boot_validity row sums deviate from 100%: ",
       paste(round(row_sums - 100, 6), collapse = ", "))
}

message(sprintf("boot_validity: %d (correlation x n x dtype) rows, %d reps total.",
                nrow(boot_validity), sum(boot_validity$n_reps)))
