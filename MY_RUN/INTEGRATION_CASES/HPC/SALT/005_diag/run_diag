#!/bin/sh
#SBATCH -J salt_diag
#SBATCH -N 1            # nodes number (=NBP)
#SBATCH -n 1            # CPUs number (on all nodes) (=NBP*TPN)
#SBATCH -o run1_16jan.eo%j   #
#SBATCH -e run1_16jan.eo%j   #
#SBATCH -t 00:05:00    # time limit
#
## Echo des commandes
ulimit -c 0
ulimit -s unlimited
## Arrete du job des la premiere erreur
set -e
## Nom de la machine
hostname
#
. ~rodierq/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2

export MPIRUN="Mpirun -np 1"

ln -sf ../004_run_mesonh/SAL* .
ln -sf ../004_run_mesonh/Aro* .

time ${MPIRUN} DIAG${XYZ}
cd ../006_python
sbatch run_python
