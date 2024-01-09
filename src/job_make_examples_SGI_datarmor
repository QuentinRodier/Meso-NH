#!/bin/bash
#MNH_LIC Copyright 1994-2024 CNRS, Meteo-France and Universite Paul Sabatier
#MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
#MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
#MNH_LIC for details. version 1.
#PBS -S /bin/bash
#PBS -N Examples
##PBS -e Examples_errors.txt
##PBS -o Examples_output.txt
##PBS -m e
#PBS -q mpi_1
#PBS -l walltime=00:30:00

ulimit -c 0
ulimit -s unlimited
# Arret du job des la premiere erreur
set -e
# Nom de la machine
hostname 
# Echo des commandes

unset MAKEFLAGS

cd $PBS_O_WORKDIR

. ../conf/profile_mesonh-LXifort-R8I4-MNH-V5-7-0-MPIINTEL-O2
export MONORUN="Mpirun -np 1 "
export MPIRUN="Mpirun -np 4 "
export POSTRUN="time "
export MNH_PYTHON="NO"

cd $SRC_MESONH/MY_RUN/KTEST/003_KW78 
make -k
#
echo "#################################################################################"
echo "##CAS SUIVANT####################################################################"
echo "#################################################################################"
cd $SRC_MESONH/MY_RUN/KTEST/001_2Drelief 
make -k
#
echo "#################################################################################"
echo "##CAS SUIVANT####################################################################"
echo "#################################################################################"
cd $SRC_MESONH/MY_RUN/KTEST/002_3Drelief 
make -k
#
echo "#################################################################################"
echo "##CAS SUIVANT####################################################################"
echo "#################################################################################"

cd $SRC_MESONH/MY_RUN/KTEST/004_Reunion
make -k << EOF 


EOF
#
echo "#################################################################################"
echo "##CAS SUIVANT####################################################################"
echo "#################################################################################"
cd $SRC_MESONH/MY_RUN/KTEST/007_16janvier
make -k << EOF 


EOF
#

