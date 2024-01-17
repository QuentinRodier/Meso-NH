#!/bin/sh
#SBATCH -J pgd_fanny
#SBATCH -N 1            # nodes number
#SBATCH -n 128            # CPUs number (on all nodes) 
#SBATCH -o pgd_fanny.eo%j   #
#SBATCH -e pgd_fanny.eo%j   #
#SBATCH -t 02:00:00    # time limit

# Echo des commandes
ulimit -c 0
ulimit -s unlimited
# Arrete du job des la premiere erreur
set -e
# Nom de la machine
hostname 


. ~rodierq/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2
export MPIRUN="Mpirun -np 128"

set -x
set -e
ln -sf $MESONH/PGD/ECOCLIMAP_v2.0.* .
ln -sf $MESONH/PGD/gtopo30.* .
ln -sf $MESONH/PGD/CLAY_HWSD_MOY.* .
ln -sf $MESONH/PGD/SAND_HWSD_MOY.* .

ls -lrt

rm -f PGD_2.5km_AR.???
time ${MPIRUN} PREP_PGD${XYZ}
mv OUTPUT_LISTING0  OUTPUT_LISTING0_pgd
touch PGD_2.5km_AR.des

ls -lrt
rm -f *.dir *.hdr 
rm -f file_for_xtransfer pipe_name

sbatch run_prep_real_case

ja
