#!/bin/sh
#SBATCH -J prep_fanny
#SBATCH -N 1            # nodes number (=NBP)
#SBATCH -n 10            # CPUs number (on all nodes) (=NBP*TPN)
#SBATCH -o prep_fanny.eo%j   #
#SBATCH -e prep_fanny.eo%j   #
#SBATCH -t 00:20:00    # time limit

. /home/cnrm_other/ge/mrmh/rodierq/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2

# Echo des commandes
ulimit -c 0
ulimit -s unlimited
# Arrete du job des la premiere erreur
set -e
# Nom de la machine
hostname

ln -sf $HOME/SAVE/GRIB_KTEST/CHARMEX/CHIMIE_FILES/tuv531/DATAE1 .
ln -sf $HOME/SAVE/GRIB_KTEST/CHARMEX/CHIMIE_FILES/tuv531/DATAJ1 .

ln -sf  ../009_mesonh2/ICART.1.SEG02.003.{des,nc} .
ln -sf  ../009_mesonh2/ICART.2.SEG02.003.{des,nc} .
ln -sf ../003_nest/ICARTT1008_PGD_15km.neste1.{des,nc} .
ln -sf ../003_nest/ICARTT1008_PGD_2km5.neste1.{des,nc} .

export MPIRUN="Mpirun -np 10"

cp  DIAG1.nam1 DIAG1.nam
time ${MPIRUN} DIAG${XYZ}

cp  DIAG1.nam2 DIAG1.nam
time ${MPIRUN} DIAG${XYZ}

cd ../011_ncl
sbatch run_ncl
