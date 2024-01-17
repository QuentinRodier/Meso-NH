#!/bin/bash
#SBATCH -J ALM2_024
#SBATCH -N 32            # nodes number (=NBP)
#SBATCH -n 2048            # CPUs number (on all nodes) (=NBP*TPN)
#SBATCH -o ideal_fire.eo%j   #
#SBATCH -e ideal_fire.eo%j   #
#SBATCH -t 05:00:00    # time limit

# Echo des commandes
ulimit -c 0
ulimit -s unlimited
# Arrete du job des la premiere erreur
set -e
# Nom de la machine
hostname

. ~rodierq/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2

export MPIRUN="Mpirun -np 2048"

set -x
set -e

cp EXSEG1.nam_SEG4 EXSEG1.nam

time ${MPIRUN} MESONH${XYZ}

rm -f file_for_xtransfer pipe_name
mv OUTPUT_LISTING0 OUTPUT_LISTING0_SEG4
mv OUTPUT_LISTING1 OUTPUT_LISTING1_SEG4
sbatch run_python
