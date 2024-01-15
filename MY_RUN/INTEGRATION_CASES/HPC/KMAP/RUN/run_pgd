#!/bin/sh
#SBATCH -J pgd_kmap
#SBATCH -N 1            # nodes number
#SBATCH -n 4            # CPUs number (on all nodes) 
#SBATCH -o pgd_kmap.eo%j   #
#SBATCH -e pgd_kmap.eo%j   #
#SBATCH -t 00:20:00    # time limit
# Echo des commandes
ulimit -c 0
ulimit -s unlimited
# Arrete du job des la premiere erreur
set -e
# Nom de la machine
hostname 


. ~rodierq//DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2

export MPIRUN="Mpirun -np 4"

set -x
set -e
ln -sf $MESONH/PGD/ECOCLIMAP_v2.0.* .
ln -sf $MESONH/PGD/gtopo30.* .
ln -sf $MESONH/PGD/CLAY_HWSD_MOY.* .
ln -sf $MESONH/PGD/SAND_HWSD_MOY.* .


ls -lrt

rm -f KMAP_PGD_32km.???
cp PRE_PGD1.nam_1 PRE_PGD1.nam
time ${MPIRUN} PREP_PGD${XYZ}
mv OUTPUT_LISTING0  OUTPUT_LISTING0_pgd32km
touch KMAP_PGD_32km.des

ls -lrt


rm -f KMAP_PGD_8km.???
cp PRE_PGD1.nam_2 PRE_PGD1.nam
time ${MPIRUN} PREP_PGD${XYZ}
mv OUTPUT_LISTING0  OUTPUT_LISTING0_pgd8km
touch KMAP_PGD_8km.des

ls -lrt

rm -f KMAP_PGD_2km.???
cp PRE_PGD1.nam_3 PRE_PGD1.nam
time ${MPIRUN} PREP_PGD${XYZ}
mv OUTPUT_LISTING0  OUTPUT_LISTING0_pgd2km
touch KMAP_PGD_2km.des

ls -lrt

rm -f *.neste1.*
time ${MPIRUN} PREP_NEST_PGD${XYZ}
mv OUTPUT_LISTING0  OUTPUT_LISTING0_nest
touch KMAP_PGD_32km.neste1.des
touch KMAP_PGD_8km.neste1.des
touch KMAP_PGD_2km.neste1.des

ls -lrt
rm -f *.dir *.hdr
rm -f PRE_PGD1.nam
rm -f file_for_xtransfer pipe_name

sbatch run_prep_real_case

ja
