#!/bin/sh
#SBATCH -J prep_ifs
#SBATCH -N 1            # nodes number (=NBP)
#SBATCH -n 2            # CPUs number (on all nodes) (=NBP*TPN)
#SBATCH -o prep_ifs.eo%j   #
#SBATCH -e prep_ifs.eo%j   #
#SBATCH -t 00:30:00    # time limit
# Echo des commandes
ulimit -c 0
ulimit -s unlimited
# Arrete du job des la premiere erreur
set -e
# Nom de la machine
hostname 

export MPIRUN="Mpirun -np 2"
. ~rodierq/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2

ln -sf ../PGD/PGD_FRANCE* .
ln -sf ~rodierq/SAVE/GRIB_KTEST/ecmwf.OD.20180513.00 .
set -x
set -e
 


time ${MPIRUN} PREP_REAL_CASE${XYZ}
sbatch run_python
