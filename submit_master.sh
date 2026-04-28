#!/bin/bash
#SBATCH --job-name=mysim-master
#SBATCH --output=logs/master-%j.out
#SBATCH --error=logs/master-%j.err
#SBATCH --time=12:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=2G
#SBATCH --partition=standard

mkdir -p logs results

module load R/4.5.0
export RENV_PATHS_CACHE="${SCRATCH:-$HOME}/renv-cache"

Rscript sim.R