#!/bin/sh
#SBATCH -J run_armles
#SBATCH -N 2            # nodes number (=NBP)
#SBATCH -n 128            # CPUs number (on all nodes) (=NBP*TPN)
#SBATCH -o run_armles.eo%j   #
#SBATCH -e run_armles.eo%j   #
#SBATCH -t 02:00:00    # time limit

# Echo des commandes
ulimit -c 0
ulimit -s unlimited
# Arrete du job des la premiere erreur
set -e
# Nom de la machine
hostname 


. ~rodierq/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2
#. ~rodierq/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2-ARMSV
export MPIRUN="Mpirun -np 128"

set -x
set -e


ls -lrt

rm -f ARM__.1.CEN4T.*
time ${MPIRUN} MESONH${XYZ}
mv OUTPUT_LISTING0  OUTPUT_LISTING0_run
mv OUTPUT_LISTING1  OUTPUT_LISTING1_run
 
ls -lrt 

rm -f file_for_xtransfer pipe_name PRESSURE REMAP*

ls -lrt 
mkdir OUTPUT
mv OUTPUT_L* OUTPUT
mkdir LFI
mv *.lfi LFI/.
mv *.des LFI/.
mkdir NETCDF
mv *.nc NETCDF/.
ja

cd ../PYTHON
sbatch run_python
