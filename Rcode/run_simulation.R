# Rcode/run_simulation.R
# ---------------------------------------------------------------------------
# Simulation harness: compare CI methods for the inter-latent correlation.
#
# Runs as a SLURM array job; each task processes a contiguous block of
# replications and writes its own .rds files into results/. Aggregation
# (rbind across task files) is a separate downstream step.
#
# Reproducibility model:
#   master_seed -> L'Ecuyer-CMRG streams -> stream k drives replication k.
# Stream assignment is by global rep_id, NOT by within-task index, so the
# same rep_id always sees the same data and the same bootstrap draws,
# regardless of how the array is chunked.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(lavaan)
})

source("Rcode/cfa.R")            # models + cfa_one()
source("Rcode/HTMT.R")           # HTMT(), HTMTDM()
source("Rcode/boot.R")           # boot(), jackknife(), bootbc(), bootbca()
source("Rcode/analyse.R")        # ci_battery()
source("Rcode/generate_data.R")  # generate_data(n, rho)

# ---- 1. Design grid -------------------------------------------------------
# Each row of `grid` is one replication. `rep_id` = global stream index.
n_reps <- 1000L
grid <- expand.grid(
  rep      = seq_len(n_reps),
  n        = c(50L, 100L, 250L, 500L),
  rho_true = c(0.0, 0.3, 0.6, 0.9),
  KEEP.OUT.ATTRS = FALSE
)
grid$rep_id <- seq_len(nrow(grid))
total_reps <- nrow(grid)

# ---- 2. SLURM array task -> rep_id range ---------------------------------
reps_per_task <- 120L
task_id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", "1"))
first   <- (task_id - 1L) * reps_per_task + 1L
last    <- min(task_id * reps_per_task, total_reps)

if (first > total_reps) {
  message(sprintf("Task %d: nothing to do (first=%d > total=%d).",
                  task_id, first, total_reps))
  quit(save = "no")
}

my_reps <- first:last
message(sprintf("Task %d: rep_ids %d..%d (%d reps).",
                task_id, first, last, length(my_reps)))

# ---- 3. RNG: master seed + per-rep L'Ecuyer streams ----------------------
master_seed <- 20260429L
RNGkind("L'Ecuyer-CMRG")
set.seed(master_seed)
stream <- .Random.seed

# advance to just before this chunk's first rep_id
for (i in seq_len(first - 1L)) stream <- parallel::nextRNGStream(stream)

# precompute one stream per rep in this chunk (avoids O(rep_id) advance per iter)
streams <- vector("list", length(my_reps))
for (k in seq_along(my_reps)) {
  stream <- parallel::nextRNGStream(stream)
  streams[[k]] <- stream
}

# ---- 4. Replication loop -------------------------------------------------
ci_list  <- vector("list", length(my_reps))
lrt_list <- vector("list", length(my_reps))

for (k in seq_along(my_reps)) {
  rep_id <- my_reps[k]
  cond   <- grid[rep_id, , drop = FALSE]

  # set the active L'Ecuyer state for this replication
  assign(".Random.seed", streams[[k]], envir = globalenv())

  d   <- generate_data(n = cond$n, rho = cond$rho_true)
  res <- ci_battery(d)

  # harness owns labelling: stamp condition + rep_id, then derive coverage
  cond_cols <- cond[, c("n", "rho_true"), drop = FALSE]
  res$ci  <- cbind(cond_cols, rep_id = rep_id, res$ci,  row.names = NULL)
  res$lrt <- cbind(cond_cols, rep_id = rep_id, res$lrt, row.names = NULL)
  res$ci$covered <- res$ci$lowerbound <= cond$rho_true &
                    cond$rho_true <= res$ci$upperbound

  ci_list[[k]]  <- res$ci
  lrt_list[[k]] <- res$lrt
}

results_ci  <- do.call(rbind, ci_list)
results_lrt <- do.call(rbind, lrt_list)

# ---- 5. Save per-task output ---------------------------------------------
dir.create("results", showWarnings = FALSE, recursive = TRUE)
saveRDS(results_ci,
        file = file.path("results", sprintf("ci_task_%05d.rds",  task_id)))
saveRDS(results_lrt,
        file = file.path("results", sprintf("lrt_task_%05d.rds", task_id)))
message(sprintf("Task %d done: ci=%d rows, lrt=%d rows.",
                task_id, nrow(results_ci), nrow(results_lrt)))
