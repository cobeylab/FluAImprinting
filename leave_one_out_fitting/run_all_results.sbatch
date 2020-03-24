#!/bin/bash
#SBATCH -n 1
#SBATCH -N 1
#SBATCH --mem=4gb

module load R/3.4.3
Rscript process_results.R ${SLURM_ARRAY_TASK_ID}