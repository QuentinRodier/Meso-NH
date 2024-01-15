#!/bin/sh
#SBATCH -J LES_OCEAN
#SBATCH -N 1            # nodes number (=NBP)
#SBATCH -n 128            # CPUs number (on all nodes) (=NBP*TPN)
#SBATCH -o les_ocean.eo%j   #
#SBATCH -e les_ocean.eo%j   #
#SBATCH -t 02:00:00    # time limit

# Echo des commandes
ulimit -c 0
ulimit -s unlimited
# Arrete du job des la premiere erreur
set -e
# Nom de la machine
hostname 
. ~rodierq//DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2
export MPIRUN="Mpirun -np 128"

cd 001_prep_ideal_case 
rm -f OUT* OCE_IN_T0*
echo PREP_IDEAL_CASE${XYZ}
time $MPIRUN PREP_IDEAL_CASE${XYZ}

cd ../002_run1
rm -f OCE_IN_T0* OUT* pipe* PRESSURE GN_01*.*
ln -sf ../001_prep_ideal_case/OCE_IN_T0* .
echo MESONH${XYZ}
date
time $MPIRUN MESONH${XYZ}

cd ../003_spawn1
rm -f OUT* pipe* PRESSURE GN_01*.* 
ln -sf ../002_run1/GN_01*.* .
time $MPIRUN SPAWNING${XYZ}

cd ../004_run2
rm -f GN_01* OUT* pipe* PRESSURE SPWAN* 
ln -sf ../003_spawn1/GN_01.1.OC_01.002.spa00*.* .
ln -sf ../002_run1/GN_01* .
time $MPIRUN MESONH${XYZ}

cd ../005_python
ln -sf ../004_run2/SPWAN.*003.nc .

module purge
module load python/3.7.6

python3 plot_OCEAN.py
convert *.png OCEAN.pdf

