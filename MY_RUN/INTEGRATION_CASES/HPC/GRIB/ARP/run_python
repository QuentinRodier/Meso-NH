#!/bin/sh
#SBATCH -J Ltools
#SBATCH -N 1            # nodes number (=NBP)
#SBATCH -n 1            # CPUs number (on all nodes) (=NBP*TPN)
#SBATCH -o python.eo%j   #
#SBATCH -e python.eo%j   #
#SBATCH -t 00:10:00    # time limit

# Echo des commandes
ulimit -c 0
ulimit -s unlimited
# Arrete du job des la premiere erreur
set -e
# Nom de la machine
hostname 
. ~rodierq/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2

#ln -sf ${SRC_MESONH}/src/LIB/Python/* .
ln -sf ~rodierq/MNHPy/src/* .
module purge
module load python/3.7.6

python3 plot_MNH_PREP.py
convert *.png MNH_PREP_ARP.pdf
