#!/bin/sh
#SBATCH -J pgd_dust
#SBATCH -N 1            # nodes number
#SBATCH -n 1            # CPUs number (on all nodes) 
#SBATCH -o pgd_dust.eo%j   #
#SBATCH -e pgd_dust.eo%j   #
#SBATCH -t 01:00:00    # time limit
# Echo des commandes
ulimit -c 0
ulimit -s unlimited
# Arrete du job des la premiere erreur
set -e
# Nom de la machine
hostname 



. ~rodierq/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2

export MPIRUN="Mpirun -np 1"

set -x
set -e


ln -sf $MESONH/PGD/ECOCLIMAP_v2.0.* .
ln -sf $MESONH/PGD/gtopo30.* .
ln -sf $MESONH/PGD/CLAY_HWSD_MOY.* .
ln -sf $MESONH/PGD/SAND_HWSD_MOY.* .

ls -lrt

rm -f pgd_shade.???
time ${MPIRUN} PREP_PGD${XYZ}
mv OUTPUT_LISTING0  OUTPUT_LISTING0_pgd
touch pgd_shade.des

ls -lrt
rm -f *.dir *.hdr 
rm -f file_for_xtransfer pipe_name

sbatch run_prep_real_case
ja
