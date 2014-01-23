#!/bin/sh
#SBATCH --time=01:00:00
#SBATCH --ntasks=128
#SBATCH --mem-per-cpu=2048
#SBATCH --partition=sandyb
module load parallel
srun="srun -N1 -n1"
parallel="parallel -j $SLURM_NTASKS --joblog runtask.make.aghadisd.log --resume"
$parallel "$srun ./runtask.make.aghadisd {1} > runtask.make.aghadisd.{1}" ::: {1..128}
