#!/bin/bash
#SBATCH --job-name=ci-htmt-sim
#SBATCH --array=1-1350
#SBATCH -p small_cpu
#SBATCH --output=results/logs/out/sim-%A_%a.out
#SBATCH --error=results/logs/err/sim-%A_%a.err
#SBATCH --time=06:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jason.berger@uni-wuerzburg.de
#SBATCH --qos=cpu

set -euo pipefail

cd "${SLURM_SUBMIT_DIR:-$(pwd)}"

mkdir -p results/ci results/lrt results/errors results/logs/out results/logs/err

export RENV_PATHS_CACHE="${SCRATCH:-$HOME}/renv-cache"

Rscript Rcode/sim.R
