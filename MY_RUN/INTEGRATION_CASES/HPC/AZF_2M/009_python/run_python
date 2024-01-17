#!/bin/sh
#SBATCH -J tools
#SBATCH -N 1            # nodes number
#SBATCH -n 1            # CPUs number (on all nodes) 
#SBATCH -o tools.eo%j   #
#SBATCH -e tools.eo%j   #
#SBATCH -t 02:00:00    # time limit
# Echo des commandes
ulimit -c 0
ulimit -s unlimited
# Arrete du job des la premiere erreur
set -e

. ~rodierq/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2
ln -sf ${SRC_MESONH}/src/LIB/Python/* .
ln -sf ~rodierq/SAVE/OUTILS/PYTHON/departements-20180101.* .
ln -sf ../007_run/AZF02.*.CEN4T.*.nc .
module purge
module load python/3.7.6

python3 plot_AZF2M.py
convert *.png AZF_2M.pdf
