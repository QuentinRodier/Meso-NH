#!/bin/sh
#SBATCH -J tools
#SBATCH -N 1            # nodes number
#SBATCH -n 1            # CPUs number (on all nodes) 
#SBATCH -o tools.eo%j   #
#SBATCH -e tools.eo%j   #
#SBATCH -t 01:00:00    # time limit
# Echo des commandes
ulimit -c 0
ulimit -s unlimited
# Arrete du job des la premiere erreur
set -e

module purge
module load python/3.10.12

ln -sf ${SRC_MESONH}/src/LIB/Python/*.py .

python3 plot_flash.py IC3E4
python3 plot_CV_elec_MNH.py IC3E4 001
python3 plot_CV_elec_MNH.py IC3E4 002
python3 plot_CV_elec_MNH.py IC3E4 003
python3 plot_CV_elec_MNH.py IC3E4 004
python3 plot_CV_elec_MNH.py IC3E4 005
python3 plot_CV_elec_MNH.py IC3E4 006
convert Fig_*IC3E4*.png ice3.pdf

python3 plot_flash.py LI1E4
python3 plot_CV_elec_MNH.py LI1E4 001
python3 plot_CV_elec_MNH.py LI1E4 002
python3 plot_CV_elec_MNH.py LI1E4 003
python3 plot_CV_elec_MNH.py LI1E4 004
python3 plot_CV_elec_MNH.py LI1E4 005
python3 plot_CV_elec_MNH.py LI1E4 006
convert Fig_*LI1E4*.png lima.pdf

python3 plot_flash.py LI2E4
python3 plot_CV_elec_MNH.py LI2E4 001
python3 plot_CV_elec_MNH.py LI2E4 002
python3 plot_CV_elec_MNH.py LI2E4 003
python3 plot_CV_elec_MNH.py LI2E4 004
python3 plot_CV_elec_MNH.py LI2E4 005
python3 plot_CV_elec_MNH.py LI2E4 006
convert Fig_*LI2E4*.png lima2.pdf

python3 plot_flash.py IC3E3
python3 plot_CV_elec_MNH.py IC3E3 001
python3 plot_CV_elec_MNH.py IC3E3 002
python3 plot_CV_elec_MNH.py IC3E3 003
python3 plot_CV_elec_MNH.py IC3E3 004
python3 plot_CV_elec_MNH.py IC3E3 005
python3 plot_CV_elec_MNH.py IC3E3 006
convert Fig_*IC3E3*.png old.pdf
