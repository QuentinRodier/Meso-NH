# -*- coding: utf-8 -*-
"""
Created on Tues Jan 15 12:09:23 2021

@auteur: Pierre-Antoine Joulin
"""
##########################################################
#INPUT :
# - fichiers .nc4
#OUTPUT :
# - Tracé sour la forme :
#    _________________________________
#   |  _____________   _____________  |
#   | |             | |             | |
#   | | ^ AOA       | | ^ mean(AOA) | |
#   | | |           | | |           | |
#   | | |           | | |           | |
#   | | |      r    | | |      r    | |
#   | | |-------->  | | |-------->  | |
#   | |_____________| |_____________| |
#   |                                 |
#   |  _____________   _____________  |
#   | |             | |             | |
#   | | ^ FT        | | ^ mean(FT)  | |
#   | | |           | | |           | |
#   | | |           | | |           | |
#   | | |      r    | | |      r    | |
#   | | |-------->  | | |-------->  | |
#   | |_____________| |_____________| |
#   |                                 |
#   |  _____________   _____________  |
#   | |             | |             | |
#   | | ^ FN        | | ^ mean(FN)  | |
#   | | |           | | |           | |
#   | | |           | | |           | |
#   | | |      r    | | |      r    | |
#   | | |-------->  | | |-------->  | |
#   | |_____________| |_____________| |
#   |_________________________________|
#   
#
##########################################################
#
import matplotlib
matplotlib.use("Agg")
import sys
import os
import matplotlib.pyplot as plt
import numpy as np
import math
import netCDF4 as nc #import Dataset
from matplotlib import gridspec #pour le placement des graphs
from matplotlib.mlab import griddata
import pandas as pd
import os.path 
  
#
#       0. Inputs
#       ---------
#
#        a. Names
#
nc_file  = sys.argv[1]
#
#       I. Data extraction
#       ------------------
#
#	 a. From nc file
#
# Radius
RAD = nc.Dataset(nc_file).variables['ELT_RAD'][0,:,0,0]
#
# Angle of attack
AOA = nc.Dataset(nc_file).variables['AOA_BLEQ'][0,:,:]
AOA_MEAN = nc.Dataset(nc_file).variables['AOAMME_BLEQ'][0,:,:]
#
# Aerodynamic loads
FAERO_RA = nc.Dataset(nc_file).variables['FAERO_BLEQ_RA'][0,:,:,0]
FAERO_RA_MEAN = nc.Dataset(nc_file).variables['FAEROMME_BLEQ_RA'][0,:,:,0]
#
#
#       II. Preliminaries
#       -----------------
#
#	 a. Compute F/dr 
DR = RAD[1] - RAD[0]
FAERO_DR_RA = FAERO_RA/DR
FAERO_DR_RA_MEAN = FAERO_RA_MEAN/DR
#
#
#       II. Preliminaries
#       -----------------
#
#
#        a. Subplots
#
fig, ax = plt.subplots(3, 2, sharex='col', figsize=(15,10.5))
plt.suptitle('Blade equivalent analysis', fontsize=16)
my_label = 'new'
my_color = 'royalblue'
#
#	 b. AoA
#
ax[0][0].plot(RAD[:], AOA[:]*180./np.pi, c=my_color, label=my_label, linestyle='-', linewidth=3)
#
#	 c. Mean(AoA)
#
ax[0][1].plot(RAD[:], AOA_MEAN[:]*180./np.pi, c=my_color, label=my_label, linestyle='-', linewidth=3)
#
#	 d. Fn
# 
ax[1][0].plot(RAD[:], -FAERO_DR_RA[2,:], c=my_color, label=my_label, linestyle='-', linewidth=3)
#
#	 e. Mean(Fn)
# 
ax[1][1].plot(RAD[:], -FAERO_DR_RA_MEAN[2,:], c=my_color, label=my_label, linestyle='-', linewidth=3)
#
#	 f. Ft
# 
ax[2][0].plot(RAD[:], -FAERO_DR_RA[1,:], c=my_color, label=my_label, linestyle='-', linewidth=3)
#
#	 g. Mean(Ft)
# 
ax[2][1].plot(RAD[:], -FAERO_DR_RA_MEAN[1,:], c=my_color, label=my_label, linestyle='-', linewidth=3)
#
#	 h. Some text...  
#
size = 10
#  
ax[0][0].set_xlabel(r'$r$ [m]')
ax[0][0].set_ylabel(r'$\alpha$ [deg]')
ax[0][0].xaxis.grid()
ax[0][0].yaxis.grid()
ax[0][0].legend(prop={'size': size})
#
ax[0][1].set_xlabel(r'$r$ [m]')
ax[0][1].set_ylabel(r'$\overline{\alpha}$ [deg]')
ax[0][1].xaxis.grid()
ax[0][1].yaxis.grid()
ax[0][1].legend(prop={'size': size})
#
ax[1][0].set_xlabel(r'$r$ [m]')
ax[1][0].set_ylabel(r'$F_N$ [N/m]')
ax[1][0].yaxis.grid()
ax[1][0].legend(loc=2, prop={'size': size})
# 
ax[1][1].set_xlabel(r'$r$ [m]')
ax[1][1].set_ylabel(r'$\overline{F_N}$ [N/m]')
ax[1][1].yaxis.grid()
ax[1][1].legend(loc=2, prop={'size': size})
# 
ax[2][0].set_xlabel(r'$r$ [m]')
ax[2][0].set_ylabel(r'$F_T$ [N/m]')
ax[2][0].xaxis.grid()
ax[2][0].yaxis.grid()
ax[2][0].legend(loc=2, prop={'size': size})
# 
ax[2][1].set_xlabel(r'$r$ [m]')
ax[2][1].set_ylabel(r'$\overline{F_T}$ [N/m]')
ax[2][1].xaxis.grid()
ax[2][1].yaxis.grid()
ax[2][1].legend(loc=2, prop={'size': size})
#
#plt.show()
name = 'AERO_var_blade_eq.png'
plt.savefig(name,format='png')	

