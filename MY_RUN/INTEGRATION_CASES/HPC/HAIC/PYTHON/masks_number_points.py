#!/usr/bin/python3
# -*- coding: utf-8 -*-

from plot_MNH_basics_cartopy_55 import *
from netCDF4 import Dataset
from matplotlib.backends.backend_pdf import PdfPages
import numpy as np
import matplotlib.pyplot as plt

import matplotlib.ticker

import os

#################################
# Read data from the Meso-NH file
#################################

simu='../03_LIMA_ICE1M_SNOWT'
fic0=Dataset(simu+"/LIMA_.1.SEG01.000.nc",'r')
nmask,ntime,nz = read_MNH_budgets_conf(fic0)
fic0.close()
masks=np.arange(0,nmask)
region=["convective","stratiform","cirriform"]
z=np.arange(1,nz+1)

simulations=os.listdir('../')
simulations2=os.listdir('../')
for i in simulations2:
    if (not os.path.isdir("../"+i)) or (i[3:7]!='ICE3' and i[3:7]!='LIMA') :
        simulations.remove(i)

budget_length=3600

output_file = PdfPages('masks_number_points.pdf')

for nmask in masks:
    for simu in simulations:
        sim=simu[3:7]
        if sim=='LIMA' or sim=='ICE3':
            fic=Dataset("../"+simu+"/"+sim+"_.1.SEG01.000.nc",'r')
            f=np.sum(fic.variables['MASKS'][:,nmask,:,:],axis=(1,2))
            f2=f*0.
            f2[1:]=f[1:]-f[:-1]
            f2[0]=0.
            plt.plot(f2,label=simu[3:])
            fic.close()
    plt.title('number of '+region[nmask]+' points')
    plt.legend()
    plt.savefig(output_file,format='pdf')
    plt.close()

output_file.close()

