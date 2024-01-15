#!/bin/sh
#SBATCH -J diagB_cyc
#SBATCH -N 4            # nodes number (=NBP)
#SBATCH -n 256            # CPUs number (on all nodes) (=NBP*TPN)
#SBATCH -o diagB_cyc.eo%j   #
#SBATCH -e diagB_cyc.eo%j   #
#SBATCH -t 01:00:00    # time limit

# Echo des commandes
ulimit -c 0
ulimit -s unlimited
# Arrete du job des la premiere erreur
set -e
# Nom de la machine
hostname 


. ~rodierq/DEV_57/MNH-PHYEX070-b95d84d7/conf/profile_mesonh-LXifort-R8I4-MNH-V5-6-2-ECRAD140-MPIAUTO-O2

export MPIRUN="Mpirun -np 256"

set -x
set -e

ls -lrt

for ECH in '001' '002' '003' '004' '005' '006' '007' '008'  
do

cat > DIAG1.nam << EOF
&NAM_CONFIO LCDF4=T LLFIOUT=T LLFIREAD=T / 
&NAM_DIAG 
  CISO='PREVTK',
  LVAR_RS=T,
  LMSLP=T, LRADAR=T,
  LDIAG(:)=.FALSE. /
&NAM_DIAG_FILE  YINIFILE(1)= "REFid.1.D70Cb.${ECH}",
                YINIFILEPGD(1)="PGD_mer04km",
                YSUFFIX = "type" /
&NAM_DIAG_SURFn N2M=2, LSURF_BUDGET=T N2M=2,
                LSURF_BUDGET=T/
&NAM_SEAFLUXn CSEA_FLUX='DIRECT',
              CSEA_ALB='TA96'/
EOF
rm -f REFid.1.D70Cb.${ECH}type.??? 
time ${MPIRUN} DIAG${XYZ}
mv OUTPUT_LISTING0  OUTPUT_LISTING0_diagB.${ECH}
mv OUTPUT_LISTING1  OUTPUT_LISTING1_diagB.${ECH}
ls -lrt

done


rm -f DIAG1.nam
rm -f file_for_xtransfer pipe_name


ls -lrt
mkdir OUTPUT
mv OUTPUT_L* OUTPUT
mkdir LFI
mv *.lfi LFI/.
mv *.des LFI/.
ja

cd ../PYTHON
sbatch run_python
