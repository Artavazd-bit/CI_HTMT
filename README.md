# CI_HTMT — Comparison of Confidence Intervals for the HTMT

Monte Carlo simulation comparing five 95% confidence-interval methods for the
**Heterotrait–Monotrait Ratio of Correlations (HTMT)** and the inter-factor
correlation it estimates. The study varies sample size, true latent
correlation, and the (non-)normality of the indicator distributions, and is
built to run on an HPC cluster as a **SLURM array job**.

The simulated population is a two-factor CFA with three indicators per factor
(`x11..x13`, `x21..x23`); the quantity of interest is the correlation
`xi_1 ~~ xi_2`.

## CI methods compared

All five are assembled per dataset by `ci_battery()` in [Rcode/analyse.R](Rcode/analyse.R):

| `estimator / method` | What it is                                         | Implemented in |
| -------------------- | -------------------------------------------------- | -------------- |
| `cfa / wald_cfa`     | Wald CI on the CFA factor correlation              | [Rcode/cfa.R](Rcode/cfa.R) |
| `htmt / delta`       | Analytical delta-method CI for the HTMT            | [Rcode/HTMT.R](Rcode/HTMT.R) (`HTMTDM`) |
| `htmt / perc`        | Percentile bootstrap CI                            | [Rcode/boot.R](Rcode/boot.R) (`myboot` + `quantile`) |
| `htmt / bc`          | Bias-corrected bootstrap CI                        | [Rcode/boot.R](Rcode/boot.R) (`bootbc`) |
| `htmt / bca`         | BCa bootstrap CI (uses jackknife acceleration)     | [Rcode/boot.R](Rcode/boot.R) (`bootbca`) |

Likelihood-ratio test of `H0: xi_1 ~~ xi_2 = 1` (perfect-correlation /
discriminant-validity null) is also recorded alongside the CFA fit.

## Simulation design

Built by [Rcode/design.R](Rcode/design.R) and saved to `conditions.rds`:

- `correlation ∈ {0.7, 0.8, 0.9, 0.95, 1}`              — 5 levels
- `n ∈ {25, 50, 100, 200, 400, 800, 1600, 3200, 6400}` — 9 levels
- `datatype ∈ {normal, moderate non-normal, severe non-normal}` — 3 levels
  - non-normal data is simulated with `covsim::rPLSIM` using fixed
    skewness/kurtosis targets (moderate: skew = 2, ex. kurt = 7;
    severe: skew = 3, ex. kurt = 21)

→ **5 × 9 × 3 = 135 unique conditions**, split into **10 rep_batches** each
→ **1350 array tasks**, **100 reps per task**, **1000 bootstrap draws per rep**
→ 1000 reps per condition in total.

## Running on an HPC cluster (SLURM array job)

The simulation is designed to run on an HPC as a SLURM array job, one
condition-batch per array task. The submit script is
[submit_master.sh](submit_master.sh):

```bash
#SBATCH --array=1-1350
#SBATCH --time=06:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G
#SBATCH -p small_cpu

Rscript Rcode/sim.R
```

Submit the whole experiment with:

```bash
sbatch submit_master.sh
```

[Rcode/sim.R](Rcode/sim.R) reads `SLURM_ARRAY_TASK_ID`, looks up the matching
row of `conditions.rds`, runs `REPS_PER_TASK = 100` replications, and writes
per-task RDS files into `results/{ci,lrt,errors,datasets}/`. The `results/`
tree is gitignored and created on the fly by the submit script.

A single task can be reproduced locally without SLURM:

```bash
SLURM_ARRAY_TASK_ID=1 Rscript Rcode/sim.R
```

## Folder structure

```
CI_HTMT/
├── submit_master.sh           # SLURM submit script: 1350-task array, runs Rcode/sim.R per task
├── conditions.rds             # design matrix (1350 rows: task_id, n, correlation, datatype, ...)
├── CI_HTMT.Rproj              # RStudio project file
├── renv.lock                  # pinned package versions for renv
├── .Rprofile                  # bootstraps renv on R startup
├── README.md                  # this file
└── Rcode/
    ├── sim.R                  # entry point per array task: reads SLURM_ARRAY_TASK_ID,
    │                          #   runs 100 reps, writes per-task RDS into results/
    ├── design.R               # builds conditions.rds (run once before submitting the array)
    ├── generate_data.R        # generate_data(): normal data via lavaan, non-normal via covsim::rPLSIM
    ├── cfa.R                  # lavaan CFA models + cfa_one() for the Wald-CFA CI and LRT
    ├── HTMT.R                 # HTMT point estimate, analytical jacobian, delta-method CI (HTMTDM)
    ├── boot.R                 # myboot(), jackknife(), bootbc(), bootbca() — bootstrap CIs
    ├── analyse.R              # ci_battery(): runs all five CI methods on one dataset
    ├── helper.R               # make_population_model() — alternative population-model builder
    ├── replay.R               # replay_rep(): re-run one (task_id, rep_in_batch) from its stored seed
    └── reproducibilitytest.R  # audit script: replays a saved rep and checks CIs match exactly
```

`results/` (gitignored, created at runtime):

```
results/
├── ci/        ci_task_<id>.rds       # 5 CI rows per rep × 100 reps per task
├── lrt/       lrt_task_<id>.rds      # LRT chi-square diff / df / p per rep
├── errors/    errors_task_<id>.rds   # captured errors and warnings per rep
├── datasets/  df_task_<id>.rds       # the simulated data frames + per-rep seeds
└── logs/
    ├── out/   sim-<jobid>_<arrayid>.out
    └── err/   sim-<jobid>_<arrayid>.err
```

Each row in `results/ci/*.rds` carries the full condition prefix
(`task_id, condition_id, rep_batch, correlation, n, datatype, dtype,
rep_in_batch, seed`) plus `estimator, method, estimate, lowerbound,
upperbound`, so the CI table is self-describing and can be `rbind`-ed across
all tasks for analysis.

## Reproducibility

- **RNG.** `Rcode/sim.R` uses `RNGkind("L'Ecuyer-CMRG")` with
  `MASTER_SEED = 20260501` and advances independent streams per `(task, rep)`,
  so every replication is statistically independent of every other and the
  schedule does not depend on which tasks ran in which order.
- **Stored seeds.** The exact `.Random.seed` used for each rep is stored in
  every CI / LRT / error / dataset row of `results/`. Any single rep can be
  re-run from its seed with `replay_rep()` in [Rcode/replay.R](Rcode/replay.R).
- **Audit script.** [Rcode/reproducibilitytest.R](Rcode/reproducibilitytest.R)
  picks one stored `(task_id, rep_in_batch)`, replays it, and checks that the
  replayed CIs match the saved ones exactly. It also prints a comparison of
  loaded vs locked package versions.
- **Package versions** are pinned via `renv` ([renv.lock](renv.lock));
  `.Rprofile` activates renv on R startup.

## Local quick-start (non-HPC)

```bash
# 1. Restore the locked package environment
Rscript -e 'renv::restore()'

# 2. Build conditions.rds (only needed once, or after editing design.R)
Rscript Rcode/design.R

# 3. Run a single array task locally
SLURM_ARRAY_TASK_ID=1 Rscript Rcode/sim.R
```

This produces `results/ci/ci_task_00001.rds` and the matching `lrt`,
`errors`, and `datasets` files.
