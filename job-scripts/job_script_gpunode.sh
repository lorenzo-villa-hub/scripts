#!/usr/bin/bash

#SBATCH --output err.%j
#SBATCH --error out.%j
#SBATCH --job-name=

#SBATCH --nodes 1
#SBATCH --time=10080
#SBATCH --mem-per-cpu=10000
#SBATCH --get-user-env=L
#SBATCH --ntasks=2
#SBATCH --gres=gpu:2
#SBATCH --cpus-per-task=1
#SBATCH --no-requeue

### #SBATCH --chdir=

module purge
module load nvhpc-hpcx-cuda12/24.9

srun -n $SLURM_NTASKS /nfshome/leimeroth/lammps_builds/gpunode2/lmp -k on g $SLURM_NTASKS -sf kk -pk kokkos newton on neigh half cuda/aware on -in input.in

