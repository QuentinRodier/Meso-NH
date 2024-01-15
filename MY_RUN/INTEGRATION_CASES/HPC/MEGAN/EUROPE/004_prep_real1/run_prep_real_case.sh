#!/bin/sh
#SBATCH -J prep_megan
#SBATCH -N 1            # nodes number (=NBP)
#SBATCH -n 4            # CPUs number (on all nodes) (=NBP*TPN)
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


. ~/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2-RELACS
export MPIRUN="Mpirun -np 4"

set -x
set -e


# ---------------------------------------------------------
# ---------------------------------------------------------
# ECMWF
# ---------------------------------------------------------
ln -sf ~rodierq/SAVE/GRIB_KTEST/ecmwf.OD.2014* .

# ---------------------------------------------------------
# MOZART
# ---------------------------------------------------------
ln -sf ~/SAVE/EMISSIONS_MEGAN/DATA/MOZART/mozart4geos5-201406* .
# ---------------------------------------------------------
# prep nest pgd files
# ---------------------------------------------------------
ln -sf ../001_prep_pgd1/CHARMEX0714_PGD_50km.* .

#export GRIB_DEFINITION_PATH=${SRC_MESONH}/src/LIB/grib_api-1.26.0-Source/definitions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#--
rm -f CPLCHE20140630*
cp PRE_REAL1.nam_20140630_00 PRE_REAL1.nam
time ${MPIRUN} PREP_REAL_CASE${XYZ}
cp PRE_REAL1.nam_20140630_06 PRE_REAL1.nam
time ${MPIRUN} PREP_REAL_CASE${XYZ}
cp PRE_REAL1.nam_20140630_12 PRE_REAL1.nam
time ${MPIRUN} PREP_REAL_CASE${XYZ}
#--
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#rm *.hdr
#rm *.dir
#rm etopo2.nc
cd ../005_mesonh_tst1
sbatch run_mesonh.sh
cd ../005_mesonh_tst2
sbatch run_mesonh.sh
cd ../005_mesonh_tst3
sbatch run_mesonh.sh
cd ../005_mesonh_tst4
sbatch run_mesonh.sh

