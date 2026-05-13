library(dplyr)

# Builds `boot_validity`: per (correlation, n, dtype), the share of reps in
# which the bootstrap distribution had a given number of non-computable HTMT
# resamples (n_boot_missing = 1000 - n_boot_valid). Bins are coarse-on-the-
# tail because the missing-count distribution is highly skewed (median 0,
# but max up to ~983 in n=25, severe conditions).

ERR_DIR <- "results/results_2026_05_11/errors"
NBOOT <- 1000L

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

# All HTMT reps in this dataset have a recorded n_boot_valid (no NAs);
# fail loudly if a future result set violates that, since it would change
# how the shares should be interpreted.
if (anyNA(htmt$n_boot_missing)) {
  stop(sprintf("Unexpected NA n_boot_valid in %d HTMT rows; revisit the ",
               "binning scheme to add an explicit 'HTMT failed' bucket.",
               sum(is.na(htmt$n_boot_missing))))
}

boot_validity <- htmt %>%
  group_by(correlation, n, dtype) %>%
  group_modify(~ {
    shares <- vapply(BIN_SPECS,
                     function(b) 100 * mean(b$test(.x$n_boot_missing)),
                     numeric(1))
    out <- data.frame(n_reps = nrow(.x), check.names = FALSE)
    for (i in seq_along(BIN_LABELS)) out[[BIN_LABELS[i]]] <- shares[i]
    out
  }) %>%
  ungroup() %>%
  arrange(correlation, n, dtype)

# Sanity: rows must sum to 100% (within float tolerance), confirming the
# bins are exhaustive and non-overlapping.
row_sums <- rowSums(as.matrix(boot_validity[, BIN_LABELS]))
if (any(abs(row_sums - 100) > 1e-9)) {
  stop("boot_validity row sums deviate from 100%: ",
       paste(round(row_sums - 100, 6), collapse = ", "))
}

message(sprintf("boot_validity: %d (correlation x n x dtype) rows, %d reps total.",
                nrow(boot_validity), sum(boot_validity$n_reps)))
