#!/bin/sh
#SBATCH -J prep_megan
#SBATCH -N 1            # nodes number (=NBP)
#SBATCH -n 1            # CPUs number (on all nodes) (=NBP*TPN)
#SBATCH -o run_megan.eo%j   #
#SBATCH -e run_megan.eo%j   #
#SBATCH -t 01:30:00    # time limit

# Echo des commandes
ulimit -c 0
ulimit -s unlimited
# Arrete du job des la premiere erreur
set -e
# Nom de la machine
hostname


ln -sf ../003_prep_nest/BIOM* .
ln -sf ~/SAVE/GRIB_KTEST/CAMS_20190328_* .
ln -sf ~/SAVE/GRIB_KTEST/ecmwf.OD.20190328.* .
. ~/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-DEBUG
#. ~/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2-PIERRE
export MPIRUN="Mpirun -np 1"

set -x
set -e

cp PRE_REAL1.nam_06 PRE_REAL1.nam
time ${MPIRUN} PREP_REAL_CASE${XYZ}
cp PRE_REAL1.nam_12 PRE_REAL1.nam
time ${MPIRUN} PREP_REAL_CASE${XYZ}

cd ../005_prep_real_case_d2
sbatch run_prep_real_case.sh
