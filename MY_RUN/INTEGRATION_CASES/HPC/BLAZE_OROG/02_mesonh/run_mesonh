#!/bin/sh
#SBATCH -J Blaze
#SBATCH -N 1            # nodes number
#SBATCH -n 128          # CPUs number (on all nodes)
#SBATCH -o zzout.eo%j   #
#SBATCH -e zzout.eo%j   #
#SBATCH -t 05:20:00    # time limit
#SBATCH -p normal256

# Echo des commandes
ulimit -c 0
ulimit -s unlimited
# Arrete du job des la premiere erreur
set -e
# Nom de la machine
hostname

export MPIRUN="Mpirun -np 128"

set -x
set -e
#module purge
#module load python/3.7.6

#python3 fuelmap.py
#module purge

. ~rodierq/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2

ln -sf ../01_prep_ideal_case/PGDFireTest.* .
ln -sf ../01_prep_ideal_case/MNHFireTest.* .

time ${MPIRUN} MESONH${XYZ}

# -----------------------------------------------
# post processing of output files with pyrolib CLI
# ------------------------------------------------
## delete unprocessed files
#pyrolib-post rearrange-netcdf -r BLAZE.1.TEST4.OUT.*.nc

## keep unprocessed files
# pyrolib-post rearrange-netcdf BLAZE.*.OUT.*.nc
cd ../03_python
sbatch run_python
