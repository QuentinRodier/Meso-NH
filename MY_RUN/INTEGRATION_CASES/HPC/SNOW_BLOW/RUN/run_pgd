#!/bin/sh
#SBATCH -J pgd_kclb
#SBATCH -N 1            # nodes number
#SBATCH -n 1            # CPUs number (on all nodes) 
#SBATCH -o pgd_kclb.eo%j   #
#SBATCH -e pgd_kclb.eo%j   #
#SBATCH -t 01:00:00    # time limit
# Echo des commandes
ulimit -c 0
ulimit -s unlimited
# Arrete du job des la premiere erreur
set -e
# Nom de la machine
hostname 


. ~rodierq/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2

export MPIRUN="Mpirun -np 1"

set -x
set -e

#Fichier supplementaire en entree pour ce KTEST
ln -sf $HOME/SAVE/GRIB_KTEST/mntclb_llv.txt . 


ls -lrt

rm -f  CLB_PGD_50m* OUTPUT_* pipe* *.tex

time ${MPIRUN} PREP_PGD${XYZ}
mv OUTPUT_LISTING0  OUTPUT_LISTING0_pgd

rm -f file_for_xtransfer pipe_name

sbatch run_prep_ideal_case

ja
