#!/bin/sh
#SBATCH -J run_megan1
#SBATCH -N 1            # nodes number (=NBP)
#SBATCH -n 64           # CPUs number (on all nodes) (=NBP*TPN)
#SBATCH -o run_megan.eo%j   #
#SBATCH -e run_megan.eo%j   #
#SBATCH -t 04:30:00    # time limit

# Echo des commandes
ulimit -c 0
ulimit -s unlimited
# Arrete du job des la premiere erreur
set -e
# Nom de la machine
hostname 


. ~/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2-RELACS

ln -sf ../003_prep_nest/BIOM* .
ln -sf ../004_prep_real_case_d1/ECMWF* .
ln -sf ../005_prep_real_case_d2/ECMWF_D2_20190328_06* .
export MPIRUN="Mpirun -np 64"

set -x
set -e

ln -sf ~rodierq/SAVE/GRIB_KTEST/CHARMEX/CHIMIE_FILES/tuv531/DATA* .

time ${MPIRUN} MESONH${XYZ}
