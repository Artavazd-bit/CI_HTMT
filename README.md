# CI_HTMT — Comparison of Confidence Intervals for the HTMT

Monte Carlo simulation comparing six 95% confidence-interval methods for the
**Heterotrait–Monotrait Ratio of Correlations (HTMT)** and the inter-factor
correlation it estimates. The study varies sample size, true latent
correlation, and the (non-)normality of the indicator distributions, and is
built to run on an HPC cluster as a **SLURM array job**.

The simulated population is a two-factor CFA with three indicators per factor
(`x11..x13`, `x21..x23`); the quantity of interest is the correlation
`xi_1 ~~ xi_2`. Each replication produces CI bounds at three confidence
levels (90%, 95%, 99%) for every method, plus per-method wall-clock time.

## CI methods compared

All six are assembled per dataset by `ci_battery()` in [Rcode/analyse.R](Rcode/analyse.R):

| `estimator / method`      | What it is                                            | Implemented in |
| ------------------------- | ----------------------------------------------------- | -------------- |
| `cfa / wald_cfa`          | Wald CI on the CFA factor correlation, ML estimator   | [Rcode/cfa.R](Rcode/cfa.R) |
| `cfa / wald_cfa_robust`   | Wald CI on the CFA factor correlation, MLR estimator  | [Rcode/cfa.R](Rcode/cfa.R) |
| `htmt / delta`            | Analytical delta-method CI for the HTMT               | [Rcode/HTMT.R](Rcode/HTMT.R) (`HTMTDM` / `HTMT_delta_se`) |
| `htmt / perc`             | Percentile bootstrap CI                               | [Rcode/boot.R](Rcode/boot.R) (`myboot` + `quantile`) |
| `htmt / bc`               | Bias-corrected bootstrap CI                           | [Rcode/boot.R](Rcode/boot.R) (`bootbc`) |
| `htmt / bca`              | BCa bootstrap CI (uses jackknife acceleration)        | [Rcode/boot.R](Rcode/boot.R) (`bootbca`) |

The bootstrap draw and the jackknife are computed **once per dataset** and
reused across all four HTMT methods and all three confidence levels, so
`ci_battery()` returns 6 methods × 3 confidence levels = **18 CI rows per
replication**.

Likelihood-ratio tests of `H0: xi_1 ~~ xi_2 = 1` (perfect-correlation /
discriminant-validity null) are also recorded for both ML and MLR fits.

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
├── Rcode/                     # simulation source (one rep = generate → fit → CIs)
│   ├── sim.R                  #   entry point per array task: reads SLURM_ARRAY_TASK_ID,
│   │                          #     runs 100 reps, writes per-task RDS into results/
│   ├── design.R               #   builds conditions.rds (run once before submitting the array)
│   ├── generate_data.R        #   generate_data(): normal data via lavaan, non-normal via covsim::rPLSIM
│   ├── cfa.R                  #   lavaan CFA models + cfa_one() for ML / MLR fits and the LRT
│   ├── HTMT.R                 #   HTMT point estimate, analytical jacobian, delta-method SE/CI
│   ├── boot.R                 #   myboot(), jackknife(), bootbc(), bootbca() — bootstrap CIs
│   └── analyse.R              #   ci_battery(): runs all six CI methods at three conf_levels
├── analysis/                  # downstream analysis pipeline (run after results/ is populated)
│   ├── updateplotstables.R    #   master: regenerates everything under outputs/
│   ├── prep_ci.R              #   loads results, joins errors, builds `resag` summary table
│   ├── prep_problems.R        #   per-condition rates of failed estimates / missing bounds
│   ├── prep_boot_validity.R   #   per-condition distribution of non-computable bootstrap reps
│   ├── plots_popcov.R         #   coverage of the population correlation (one-sided)
│   ├── plots_rejrate.R        #   rejection rate of H0: phi = 1 (discriminant validity)
│   ├── plots_problems.R       #   rate of failed estimates or missing CI bounds
│   ├── tables_time.R          #   LaTeX: mean compute time per method vs. wald_cfa baseline
│   ├── tables_boot_validity.R #   LaTeX: bootstrap-failure share by condition
│   └── verify_jack.R          #   sanity check on jackknife missingness across conditions
├── archive/                   # superseded code kept for reference (replay, reproducibility test, …)
├── outputs/                   # gitignored: plots/ and tables/ produced by analysis/
└── results/                   # gitignored: one results_YYYY_MM_DD/ directory per simulation run
```

`results/results_<date>/` (gitignored, created at runtime by `sim.R`):

```
results_2026_05_14/
├── ci/        ci_task_<id>.rds       # 18 CI rows per rep × 100 reps per task
├── lrt/       lrt_task_<id>.rds      # 2 LRT rows per rep (standard + robust)
├── errors/    errors_task_<id>.rds   # captured errors / warnings per rep × estimator
├── datasets/  df_task_<id>.rds       # the simulated data frames + per-rep seeds
└── logs/
    ├── out/   sim-<jobid>_<arrayid>.out
    └── err/   sim-<jobid>_<arrayid>.err
```

Each row in `results/.../ci/*.rds` carries the full condition prefix
(`task_id, condition_id, rep_batch, correlation, n, datatype, dtype,
rep_in_batch, seed`) plus `conf_level, estimator, method, estimate,
lowerbound, upperbound, time`, so the CI table is self-describing and can be
`rbind`-ed across all tasks for analysis.

The `errors/*.rds` table is long-format with one row per `(rep, estimator,
scope)`: scope is `uncon` / `con` / `lrt` for `cfa` and `cfa_robust`, and
`NA` for `htmt`. Columns `n_boot_valid` and `n_jack_valid` record how many
HTMT bootstrap / jackknife resamples produced a finite value (out of `NBOOT`
and `n` respectively), which is used downstream by `prep_boot_validity.R`.

## Downstream analysis

After a simulation run completes, regenerate all plots and tables under
`outputs/` from a results directory:

```bash
# Edit RESULTS_DIR at the top of analysis/updateplotstables.R if needed
Rscript analysis/updateplotstables.R
```

The master script sources the `prep_*.R`, `plots_*.R`, and `tables_*.R`
files in order; each is also runnable standalone (the `exists()`-guarded
`source()` calls inside them no-op when the master has already populated
the global environment).

Outputs:

- `outputs/plots/popcov_<dtype>_cl<NN>.png` — one-sided coverage of the
  population correlation, by sample size, faceted by phi; one PNG per
  data type × confidence level
- `outputs/plots/rejrate_<dtype>_cl<NN>.png` — rejection rate of
  `H0: phi = 1` by sample size, faceted by phi
- `outputs/plots/problems_cl<NN>.png` — per-condition rate of failed
  point estimates or missing CI bounds
- `outputs/tables/relative_time_vs_wald.tex` — mean per-rep compute time
  of every method as a % difference from the ML Wald-CFA baseline
- `outputs/tables/boot_validity.tex` — share of replications with a given
  number of non-computable bootstrap HTMT values, by condition

## Reproducibility

- **RNG.** `Rcode/sim.R` uses `RNGkind("L'Ecuyer-CMRG")` with
  `MASTER_SEED = 20260501` and advances independent streams per `(task, rep)`,
  so every replication is statistically independent of every other and the
  schedule does not depend on which tasks ran in which order.
- **Stored seeds.** The exact `.Random.seed` used for each rep is stored in
  every CI / LRT / error / dataset row of `results/`, so any single rep can
  be replayed from its seed. The replay and reproducibility-audit scripts
  used in earlier drops have been moved to [archive/Rcode/](archive/Rcode).
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

# 4. (After many tasks have run) regenerate plots and tables
Rscript analysis/updateplotstables.R
```

Step 3 produces `results/ci/ci_task_00001.rds` and the matching `lrt`,
`errors`, and `datasets` files. For a full run, move these into a dated
`results/results_YYYY_MM_DD/` directory and point
`analysis/updateplotstables.R` at it via `RESULTS_DIR`.
