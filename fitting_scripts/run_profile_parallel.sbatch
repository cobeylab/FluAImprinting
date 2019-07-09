#!/bin/bash
#SBATCH -p cobey
#SBATCH -n 1
#SBATCH -N 1

module load R/3.4.3
Rscript profile_parallel.R ${SLURM_ARRAY_TASK_ID} ${param} ${profile_min} ${profile_max} ${num_points} ${model_index} ${SLURM_ARRAY_JOB_ID} ${fh}