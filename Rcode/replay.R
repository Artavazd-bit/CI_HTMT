suppressPackageStartupMessages({
  library(lavaan)
  library(covsim)
})

source("Rcode/cfa.R")
source("Rcode/HTMT.R")
source("Rcode/boot.R")
source("Rcode/analyse.R")
source("Rcode/generate_data.R")

replay_rep <- function(task_id, rep_in_batch,
                       conditions_file = "conditions.rds",
                       results_dir     = "results",
                       nboot           = 1000L,
                       run_battery     = TRUE) {

  conditions <- readRDS(conditions_file)
  cond <- conditions[conditions$task_id == task_id, , drop = FALSE]
  if (nrow(cond) != 1L) {
    stop(sprintf("Expected 1 row for task_id=%d, got %d.", task_id, nrow(cond)))
  }

  ci_file <- file.path(results_dir, "ci", sprintf("ci_task_%05d.rds", task_id))
  if (!file.exists(ci_file)) stop("Result file not found: ", ci_file)
  ci_tbl <- readRDS(ci_file)
  row    <- ci_tbl[ci_tbl$rep_in_batch == rep_in_batch, ][1L, ]
  if (nrow(row) == 0L) stop("rep_in_batch not found in result file.")
  seed_k <- row$seed[[1]]

  dt_arg <- if (cond$dtype == "normal") "normal" else "nonnormal"
  sk_arg <- cond$skewness[[1]]
  kt_arg <- cond$kurtosis[[1]]

  RNGkind("L'Ecuyer-CMRG")
  assign(".Random.seed", seed_k, envir = globalenv())
  data <- generate_data(n = cond$n, population_model = cond$model,
                        datatype = dt_arg, skewness = sk_arg, kurtosis = kt_arg)

  out <- list(condition = cond, seed = seed_k, data = data)

  if (run_battery) {
    assign(".Random.seed", seed_k, envir = globalenv())
    data2 <- generate_data(n = cond$n, population_model = cond$model,
                           datatype = dt_arg, skewness = sk_arg, kurtosis = kt_arg)
    out$battery <- ci_battery(data2, nboot = nboot)
  }
  out
}
