#!/bin/sh
#SBATCH -J prep_fanny
#SBATCH -N 1            # nodes number (=NBP)
#SBATCH -n 16            # CPUs number (on all nodes) (=NBP*TPN)
#SBATCH -o prep_fanny.eo%j   #
#SBATCH -e prep_fanny.eo%j   #
#SBATCH -t 04:00:00    # time limit

# Echo des commandes
ulimit -c 0
ulimit -s unlimited
# Nom de la machine
hostname 

. ~rodierq/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2

export MPIRUN="Mpirun -np 16"

ln -sf ~rodierq/SAVE/GRIB_KTEST/arome.AN.200809* .
ln -sf ~rodierq/SAVE/GRIB_KTEST/CAMS_200809* .

ls -lrt

for DATE in '20080903.06' '20080903.09'  '20080903.12' '20080903.15' '20080903.18' '20080903.21' '20080904.00' '20080904.03'  '20080904.06'
do   
rm -f A_${DATE}.???
cp PRE_REAL1.nam.${DATE} PRE_REAL1.nam
time ${MPIRUN} PREP_REAL_CASE${XYZ}
mv OUTPUT_LISTING0  OUTPUT_LISTING0_prep.${DATE}
 
ls -lrt

done


rm -f arome*
rm -f file_for_xtransfer pipe_name


ls -lrt

sbatch run_mesonh
ja
