#!/bin/sh
#SBATCH -J pgd_megan1
#SBATCH -N 1            # nodes number (=NBP)
#SBATCH -n 8            # CPUs number (on all nodes) (=NBP*TPN)
#SBATCH -o run_megan.eo%j   #
#SBATCH -e run_megan.eo%j   #
#SBATCH -t 00:30:00    # time limit

# Echo des commandes
ulimit -c 0
ulimit -s unlimited
# Arrete du job des la premiere erreur
set -e
# Nom de la machine
hostname


#. ~/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2-PIERRE
. ~/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2
ln -sf ~/SAVE/mesonh/PGD/* .
ln -sf ~/SAVE/EMISSIONS_MEGAN/DATA/DATA_EMIS/* .
ln -sf ~/SAVE/CHIMIE_FILES/EMISSIONS/* .
ln -sf ~/SAVE/EMISSIONS_MEGAN/* .
ln -sf ~/SAVE/EMISSIONS_DMS/* .

export MPIRUN="Mpirun -np 8"

set -x
set -e


time ${MPIRUN} PREP_PGD${XYZ}
cd ../002_prep_pgd2
sbatch run_pgd.sh
