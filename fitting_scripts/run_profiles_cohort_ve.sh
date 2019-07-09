#!/bin/bash
#SBATCH -p cobey
#SBATCH -n 1
#SBATCH -N 1

num_points=15
model_index=12
fh=0.1

#sbatch --array=1-${num_points} -p cobey --export=param=N2m,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index},fh=${fh} run_profile_parallel.sbatch

sbatch --array=1-${num_points} --export=param=H1m,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index},fh=${fh} run_profile_parallel.sbatch
sbatch --array=1-${num_points} --export=param=H3m,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index},fh=${fh} run_profile_parallel.sbatch

sbatch --array=1-${num_points} --export=param=VE_2013-2017_h1,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
sbatch --array=1-${num_points} --export=param=VE_2008-2012_h1,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
sbatch --array=1-${num_points} --export=param=VE_2003-2007_h1,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
sbatch --array=1-${num_points} --export=param=VE_1998-2002_h1,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
sbatch --array=1-${num_points} --export=param=VE_1988-1997_h1,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
sbatch --array=1-${num_points} --export=param=VE_1978-1987_h1,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
sbatch --array=1-${num_points} --export=param=VE_1968-1977_h1,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
sbatch --array=1-${num_points} --export=param=VE_1953-1967_h1,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
sbatch --array=1-${num_points} --export=param=VE_1917-1952_h1,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch

sbatch --array=1-${num_points} --export=param=VE_2013-2017_h3,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
sbatch --array=1-${num_points} --export=param=VE_2008-2012_h3,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
sbatch --array=1-${num_points} --export=param=VE_2003-2007_h3,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
sbatch --array=1-${num_points} --export=param=VE_1998-2002_h3,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
sbatch --array=1-${num_points} --export=param=VE_1988-1997_h3,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
sbatch --array=1-${num_points} --export=param=VE_1978-1987_h3,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
sbatch --array=1-${num_points} --export=param=VE_1968-1977_h3,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
sbatch --array=1-${num_points} --export=param=VE_1953-1967_h3,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
sbatch --array=1-${num_points} --export=param=VE_1917-1952_h3,profile_min=0,profile_max=0.99,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch

sbatch --array=1-${num_points} --export=param=A0-4,profile_min=1.0,profile_max=4.0,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
sbatch --array=1-${num_points} --export=param=A5-9,profile_min=2,profile_max=5,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
sbatch --array=1-${num_points} --export=param=A10-14,profile_min=1.0,profile_max=3,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
sbatch --array=1-${num_points} --export=param=A15-19,profile_min=0.5,profile_max=2,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
sbatch --array=1-${num_points} --export=param=A30-39,profile_min=0.5,profile_max=2,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
sbatch --array=1-${num_points} --export=param=A40-49,profile_min=0.3,profile_max=1.5,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
sbatch --array=1-${num_points} --export=param=A50-64,profile_min=0.3,profile_max=2.0,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch
sbatch --array=1-${num_points} --export=param=A65-100,profile_min=0.3,profile_max=3.0,num_points=${num_points},model_index=${model_index} run_profile_parallel.sbatch