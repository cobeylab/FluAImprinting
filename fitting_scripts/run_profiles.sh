#!/bin/bash
#SBATCH -p cobey
#SBATCH -n 1
#SBATCH -N 1

num_points=14
model_index=2
fh=0.1

# DEBUG

sbatch --array=1-${num_points} -p cobey --export=param=N2m,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index},fh=${fh} run_profile_parallel.sbatch

#sbatch --array=1-${num_points} --export=param=H1m,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index},fh=${fh} run_profile_parallel.sbatch
#sbatch --array=1-${num_points} --export=param=H3m,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index},fh=${fh} run_profile_parallel.sbatch

#sbatch --array=1-${num_points} --export=param=VE_0-4_h1,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
#sbatch --array=1-${num_points} --export=param=VE_5-9_h1,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
#sbatch --array=1-${num_points} --export=param=VE_10-14_h1,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
#sbatch --array=1-${num_points} --export=param=VE_15-19_h1,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
#sbatch --array=1-${num_points} --export=param=VE_20-29_h1,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
#sbatch --array=1-${num_points} --export=param=VE_30-39_h1,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
#sbatch --array=1-${num_points} --export=param=VE_40-49_h1,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
#sbatch --array=1-${num_points} --export=param=VE_50-64_h1,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
#sbatch --array=1-${num_points} --export=param=VE_65-100_h1,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch

#sbatch --array=1-${num_points} --export=param=VE_0-4_h3,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
#sbatch --array=1-${num_points} --export=param=VE_5-9_h3,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
#sbatch --array=1-${num_points} --export=param=VE_10-14_h3,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
#sbatch --array=1-${num_points} --export=param=VE_15-19_h3,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
#sbatch --array=1-${num_points} --export=param=VE_20-29_h3,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
#sbatch --array=1-${num_points} --export=param=VE_30-39_h3,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
#sbatch --array=1-${num_points} --export=param=VE_40-49_h3,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
#sbatch --array=1-${num_points} --export=param=VE_50-64_h3,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
#sbatch --array=1-${num_points} --export=param=VE_65-100_h3,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch

#sbatch --array=1-${num_points} --export=param=A0-4,profile_min=1.0,profile_max=4.0,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
#sbatch --array=1-${num_points} --export=param=A5-9,profile_min=2,profile_max=5,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
#sbatch --array=1-${num_points} --export=param=A10-14,profile_min=1.0,profile_max=3,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
#sbatch --array=1-${num_points} --export=param=A15-19,profile_min=0.5,profile_max=2,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
#sbatch --array=1-${num_points} --export=param=A30-39,profile_min=0.5,profile_max=2,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
#sbatch --array=1-${num_points} --export=param=A40-49,profile_min=0.3,profile_max=1.5,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
#sbatch --array=1-${num_points} --export=param=A50-64,profile_min=0.3,profile_max=2.0,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
#sbatch --array=1-${num_points} --export=param=A65-100,profile_min=0.3,profile_max=3.0,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch