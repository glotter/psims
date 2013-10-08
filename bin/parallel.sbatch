#!/bin/sh

#SBATCH --time=02:00:00
#SBATCH --ntasks=1
#SBATCH --exclusive
#SBATCH --mem-per-cpu=16384
#SBATCH --partition=westmere

module load parallel

srun="srun --exclusive -N1 -n1"
parallel="parallel -j $SLURM_NTASKS --joblog runtask.log --resume"
$parallel "$srun runtask {1} > runtask.{1}" ::: 1
