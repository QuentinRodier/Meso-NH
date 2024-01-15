#!/bin/sh
#SBATCH -J prep_d2_megan
#SBATCH -N 1            # nodes number (=NBP)
#SBATCH -n 1            # CPUs number (on all nodes) (=NBP*TPN)
#SBATCH -o run_megan.eo%j   #
#SBATCH -e run_megan.eo%j   #
#SBATCH -t 00:30:00    # time limit

# Echo des commandes
ulimit -c 0
ulimit -s unlimited
# Arrete du job des la premiere erreur
set -e
# Nom de la machine
hostname


ln -sf ../003_prep_nest/BIOM* .
ln -sf ../004_prep_real_case_d1/ECMWF* .
. ~/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2-RELACS

export MPIRUN="Mpirun -np 1"

set -x
set -e

time ${MPIRUN} SPAWNING${XYZ}
time ${MPIRUN} PREP_REAL_CASE${XYZ}
cd ../006_mesonh_283TO3003
sbatch run_mesonh.sh
