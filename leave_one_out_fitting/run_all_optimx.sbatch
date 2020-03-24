#!/bin/bash
#SBATCH -p cobey
#SBATCH -n 1
#SBATCH -N 1

sbatch -p cobey --array=16 --export=exclude_year=${SLURM_ARRAY_TASK_ID} run_optimx.sbatch
