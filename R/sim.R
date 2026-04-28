suppressPackageStartupMessages({
  library(future)
  library(future.batchtools)
  library(doFuture)
  libraray(foreach)
  library(covsim)
  library(lavaan)
  library(dplyr)
})

source("R/setup.R")

conditions <- expand.grid(
  n    = c(50, 100, 250, 500),
  beta = c(0.0, 0.2, 0.5),
  rep  = seq_len(1000)
)

plan(
  batchtools_slurm, 
  template = "slurm.tmp1",
  resources = list(
    walltime = "02:00:00",
    ncpus = 1L, 
    memory = 2000L,
    partition = "standard"
  )
)

results_list <- foreach(
  i = seq_len(nrow(conditions)),
  .options.future = list(
    seed       = TRUE,         # L'Ecuyer-CMRG streams; fully reproducible
    chunk.size = 100
  )
) %dofuture% {
  cond <- conditions[i, , drop = FALSE]
  analyse_one(n = cond$n, beta = cond$beta)
}

results <- do.call(rbind, results_list)
results$rep <- conditions$rep

dir.create("results", showWarnings = FALSE)
saveRDS(results, file = file.path("results", "sim_results.rds"))
message("Done. Saved to results/sim_results.rds")
