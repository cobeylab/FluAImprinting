#!/bin/bash
#SBATCH -p broadwl
#SBATCH -n 1
#SBATCH -N 1
#SBATCH --mem=4gb

module load R/3.4.3
Rscript run_optimx.R ${SLURM_ARRAY_TASK_ID} ${SLURM_ARRAY_JOB_ID}