#!/bin/sh
#SBATCH -J run2_fog
#SBATCH -N 20            # nodes number (=NBP)
#SBATCH -n 200            # CPUs number (on all nodes) (=NBP*TPN)
#SBATCH -o run2_fog.eo%j   #
#SBATCH -e run2_fog.eo%j   #
#SBATCH -t 04:00:00    # time limit

# Echo des commandes
ulimit -c 0
ulimit -s unlimited
# Arrete du job des la premiere erreur
set -e
# Nom de la machine
hostname 


. ~rodierq/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2

ln -sf ~rodierq/DEV_57/MNH-PHYEX070-b95d84d7/src/LIB/RAD/ecrad-1.4.0/data/* .


export MPIRUN="Mpirun -np 200"

set -x
set -e
ls -lrt


rm -f FOG3D.?.CE_S2.*
cp EXSEG1.nam_SEG02 EXSEG1.nam
time ${MPIRUN} MESONH${XYZ}
mv OUTPUT_LISTING0  OUTPUT_LISTING0_run2
mv OUTPUT_LISTING1  OUTPUT_LISTING1_run2

ls -lrt

rm -f EXSEG1.nam
rm -f file_for_xtransfer pipe_name PRESSURE REMAP*
ls -lrt
rm -Rf OUTPUT
mkdir OUTPUT
mv OUTPUT_L* OUTPUT
ja
