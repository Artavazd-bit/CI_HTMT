library(dplyr)


if (!exists("RESULTS_DIR", inherits = TRUE)) RESULTS_DIR <- "results/results_2026_05_13"
ERR_DIR <- file.path(RESULTS_DIR, "errors")
NBOOT <- 1000L

CI_DIR <- file.path(RESULTS_DIR, "ci")

ci_files <- list.files(CI_DIR, pattern = "\\.rds$", full.names = TRUE)
if (length(ci_files) == 0L) stop("No .rds files found in CI_DIR: ", CI_DIR)

dfall3  <- do.call(rbind, lapply(ci_files,  readRDS))

dfall3 <- dfall3[dfall3$estimator == "htmt" & dfall3$method == "bca" & dfall3$conf_level == 0.9,]

BIN_SPECS <- list(
  list(label = "0",        test = function(x) x == 0),
  list(label = "0-1",     test = function(x) x >   0 & x <=   1),
  list(label = "1-100",    test = function(x) x > 1  & x <=   100)
)
BIN_LABELS <- vapply(BIN_SPECS, `[[`, character(1), "label")

err_files <- list.files(ERR_DIR, pattern = "\\.rds$", full.names = TRUE)
if (length(err_files) == 0L) stop("No .rds files found in ERR_DIR: ", ERR_DIR)

errall <- do.call(rbind, lapply(err_files, readRDS))
htmt <- errall[errall$estimator == "htmt", ]
htmt$n_jack_missing <- htmt$n - htmt$n_jack_valid 
htmt$n_jack_missing_perc <- htmt$n_jack_missing / htmt$n 

htmt_val <- merge(htmt, dfall3,
                  by = c("task_id", "rep_in_batch"))

htmt_val <- htmt_val[!is.na(htmt_val$estimate) & !is.na(htmt_val$lowerbound), ]

# All HTMT reps in this dataset have a recorded n_boot_valid (no NAs);
# fail loudly if a future result set violates that, since it would change
# how the shares should be interpreted.
if (anyNA(htmt_val$n_jack_missing)) {
  stop(sprintf("Unexpected NA n_boot_valid in %d HTMT rows; revisit the ",
               "binning scheme to add an explicit 'HTMT failed' bucket.",
               sum(is.na(htmt_val$n_boot_missing))))
}

jack_validity <- htmt_val %>%
  group_by(correlation.x, n.x, dtype.x) %>%
  group_modify(~ {
    shares <- vapply(BIN_SPECS,
                     function(b) 100 * mean(b$test(.x$n_jack_missing_perc)),
                     numeric(1))
    out <- data.frame(n_reps = nrow(.x), check.names = FALSE)
    for (i in seq_along(BIN_LABELS)) out[[BIN_LABELS[i]]] <- shares[i]
    out
  }) %>%
  ungroup() %>%
  arrange(correlation.x, n.x, dtype.x)

# Sanity: rows must sum to 100% (within float tolerance), confirming the
# bins are exhaustive and non-overlapping.
row_sums <- rowSums(as.matrix(jack_validity[, BIN_LABELS]))
if (any(abs(row_sums - 100) > 1e-9)) {
  stop("jack_validity row sums deviate from 100%: ",
       paste(round(row_sums - 100, 6), collapse = ", "))
}

message(sprintf("jack_validity: %d (correlation x n x dtype) rows, %d reps total.",
                nrow(jack_validity), sum(jack_validity$n_reps)))
