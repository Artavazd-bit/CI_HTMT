source("renv/activate.R")

suppressPackageStartupMessages({
  library(lavaan)
  library(covsim)
})

source("Rcode/cfa.R")
source("Rcode/HTMT.R")
source("Rcode/boot.R")
source("Rcode/analyse.R")
source("Rcode/generate_data.R")

MASTER_SEED   <- 20260501L
REPS_PER_TASK <- 100L
NBOOT         <- 1000L
CONF_LEVELS   <- c(0.90, 0.95, 0.99)

task_id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", "1"))
if (is.na(task_id) || task_id < 1L) stop("Invalid SLURM_ARRAY_TASK_ID: ", task_id)

conditions <- readRDS("conditions.rds")
cond <- conditions[conditions$task_id == task_id, , drop = FALSE]
if (nrow(cond) != 1L) {
  stop(sprintf("Expected 1 row for task_id=%d, got %d.", task_id, nrow(cond)))
}

dt_arg <- if (cond$dtype == "normal") "normal" else "nonnormal"
sk_arg <- cond$skewness[[1]]
kt_arg <- cond$kurtosis[[1]]

message(sprintf(
  "Task %d: condition_id=%d batch=%d | n=%d rho=%.2f dtype=%s | %d reps",
  task_id, cond$condition_id, cond$rep_batch, cond$n, cond$correlation,
  cond$dtype, REPS_PER_TASK
))

RNGkind("L'Ecuyer-CMRG")
set.seed(MASTER_SEED)
stream <- .Random.seed
first  <- (task_id - 1L) * REPS_PER_TASK
for (i in seq_len(first)) stream <- parallel::nextRNGStream(stream)
streams <- vector("list", REPS_PER_TASK)
for (k in seq_len(REPS_PER_TASK)) {
  stream <- parallel::nextRNGStream(stream)
  streams[[k]] <- stream
}

ci_list   <- vector("list", REPS_PER_TASK)
lrt_list  <- vector("list", REPS_PER_TASK)
err_list  <- vector("list", REPS_PER_TASK)
data_list <- vector("list", REPS_PER_TASK)

cond_cols <- cond[, c("task_id", "condition_id", "rep_batch",
                      "correlation", "n", "datatype", "dtype")]

for (k in seq_len(REPS_PER_TASK)) {
  seed_k <- streams[[k]]
  assign(".Random.seed", seed_k, envir = globalenv())

  gen_err   <- NA_character_
  gen_warns <- character()
  d <- withCallingHandlers(
    tryCatch(
      generate_data(n = cond$n, population_model = cond$model,
                    datatype = dt_arg, skewness = sk_arg, kurtosis = kt_arg),
      error = function(e) { gen_err <<- conditionMessage(e); NULL }
    ),
    warning = function(w) {
      gen_warns <<- c(gen_warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  data_list[[k]] <- list(
    rep_in_batch = k,
    task_id      = task_id,
    condition_id = cond$condition_id,
    rep_batch    = cond$rep_batch,
    seed         = seed_k,
    data         = d
  )

  if (is.null(d)) {
    msg <- paste0("generate_data: ", gen_err)
    res <- list(
      ci = data.frame(
        conf_level = rep(CONF_LEVELS, each = 5),
        estimator  = rep(c("cfa", "htmt", "htmt", "htmt", "htmt"),
                         times = length(CONF_LEVELS)),
        method     = rep(c("wald_cfa", "delta", "perc", "bc", "bca"),
                         times = length(CONF_LEVELS)),
        estimate   = NA_real_, lowerbound = NA_real_, upperbound = NA_real_,
        time       = NA_real_,
        stringsAsFactors = FALSE
      ),
      lrt = data.frame(chisq_diff = NA_real_, df_diff = NA_real_,
                       p_diff = NA_real_, time = NA_real_),
      errors = data.frame(
        estimator       = c("cfa", "htmt"),
        error_message   = msg,
        warning_message = NA_character_,
        stringsAsFactors = FALSE
      )
    )
  } else {
    res <- ci_battery(d, nboot = NBOOT, conf_levels = CONF_LEVELS)
  }

  if (length(gen_warns) > 0L) {
    gen_msg <- paste0("generate_data: ",
                      paste(unique(gen_warns), collapse = " || "))
    res$errors$warning_message <- ifelse(
      is.na(res$errors$warning_message),
      gen_msg,
      paste(gen_msg, res$errors$warning_message, sep = " || ")
    )
  }

  prefix <- cbind(cond_cols, rep_in_batch = k, row.names = NULL)
  prefix$seed <- I(list(seed_k))

  expand <- function(p, n) {
    out <- p[rep(1L, n), , drop = FALSE]
    rownames(out) <- NULL
    out
  }

  ci_list[[k]]  <- cbind(expand(prefix, nrow(res$ci)),     res$ci,     row.names = NULL)
  lrt_list[[k]] <- cbind(expand(prefix, nrow(res$lrt)),    res$lrt,    row.names = NULL)
  err_list[[k]] <- cbind(expand(prefix, nrow(res$errors)), res$errors, row.names = NULL)
}

results_ci  <- do.call(rbind, ci_list)
results_lrt <- do.call(rbind, lrt_list)
results_err <- do.call(rbind, err_list)

dir.create("results/ci",       recursive = TRUE, showWarnings = FALSE)
dir.create("results/lrt",      recursive = TRUE, showWarnings = FALSE)
dir.create("results/errors",   recursive = TRUE, showWarnings = FALSE)
dir.create("results/datasets", recursive = TRUE, showWarnings = FALSE)

saveRDS(results_ci,
        file = file.path("results/ci",       sprintf("ci_task_%05d.rds",     task_id)))
saveRDS(results_lrt,
        file = file.path("results/lrt",      sprintf("lrt_task_%05d.rds",    task_id)))
saveRDS(results_err,
        file = file.path("results/errors",   sprintf("errors_task_%05d.rds", task_id)))
saveRDS(data_list,
        file = file.path("results/datasets", sprintf("df_task_%05d.rds",     task_id)))

message(sprintf("Task %d done: ci=%d, lrt=%d, errors=%d rows, datasets=%d.",
                task_id, nrow(results_ci), nrow(results_lrt), nrow(results_err),
                length(data_list)))

writeLines(capture.output(sessionInfo()), con = stderr())
