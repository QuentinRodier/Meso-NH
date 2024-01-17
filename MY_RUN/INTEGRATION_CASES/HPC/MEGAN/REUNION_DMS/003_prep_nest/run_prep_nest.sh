#!/bin/sh
#SBATCH -J pgd_megan1
#SBATCH -N 1            # nodes number (=NBP)
#SBATCH -n 1            # CPUs number (on all nodes) (=NBP*TPN)
#SBATCH -o run_megan.eo%j   #
#SBATCH -e run_megan.eo%j   #
#SBATCH -t 00:10:00    # time limit

# Echo des commandes
ulimit -c 0
ulimit -s unlimited
# Arrete du job des la premiere erreur
set -e
# Nom de la machine
hostname

ln -sf ../001_prep_pgd1/BIOM* .
ln -sf ../002_prep_pgd2/BIOM* .
. ~/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2
#. ~/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2-PIERRE
export MPIRUN="Mpirun -np 1"

set -x
set -e


time ${MPIRUN} PREP_NEST_PGD${XYZ}
cd ../004_prep_real_case_d1
sbatch run_prep_real_case.sh
