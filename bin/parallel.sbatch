#!/bin/sh

#SBATCH --time=05:00:00
#SBATCH --ntasks=1
#SBATCH --exclusive
#SBATCH --mem-per-cpu=51200
#SBATCH --partition=bigmem

module load parallel

srun="srun --exclusive -N1 -n1"
parallel="parallel -j $SLURM_NTASKS --joblog runtask.log --resume"
$parallel "$srun runtask {1} > runtask.{1}" ::: 1
