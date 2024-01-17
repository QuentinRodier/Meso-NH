#!/bin/sh
#SBATCH -J run_megan1
#SBATCH -N 1            # nodes number (=NBP)
#SBATCH -n 1            # CPUs number (on all nodes) (=NBP*TPN)
#SBATCH -o run_megan.eo%j   #
#SBATCH -e run_megan.eo%j   #
#SBATCH -t 03:00:00    # time limit
# Echo des commandes
ulimit -c 0
ulimit -s unlimited
# Arrete du job des la premiere erreur
set -e
# Nom de la machine
hostname 


. ~/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2-RELACS
export MPIRUN="Mpirun -np 1"

set -x
set -e

ln -sf ../001_prep_pgd1/CHARMEX0714_PGD_50km* .
ln -sf ../004_prep_real1/CPLCHE20140630* .

ln -sf ~rodierq/SAVE/GRIB_KTEST/CHARMEX/CHIMIE_FILES/tuv531/DATA* .

time ${MPIRUN} MESONH${XYZ}
