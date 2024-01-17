#!/bin/sh
#SBATCH -J run1_fanny
#SBATCH -N 20            # nodes number
#SBATCH -n 200            # CPUs number (on all nodes) 
#SBATCH -o run1_fanny.eo%j   #
#SBATCH -e run1_fanny.eo%j   #
#SBATCH -t 00:20:00    # time limit

# Echo des commandes
ulimit -c 0
ulimit -s unlimited
# Arrete du job des la premiere erreur
set -e
# Nom de la machine
hostname 

. ~rodierq/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-DEBUG

ln -sf ../ICE3/PGD_2.5km_AR.* .
ln -sf ../ICE3/A_2008090* .
export MPIRUN="Mpirun -np 200"

set -x
set -e


ls -lrt

cp -f EXSEG1.nam_EXPE1 EXSEG1.nam
time ${MPIRUN} MESONH${XYZ}
mv OUTPUT_LISTING1 OUTPUT_LISTING1_EXPE1

rm -f file_for_xtransfer pipe_name PRESSURE REMAP*

ja
