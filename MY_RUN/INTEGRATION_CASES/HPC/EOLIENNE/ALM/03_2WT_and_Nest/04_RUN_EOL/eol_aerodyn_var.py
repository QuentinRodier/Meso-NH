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
RAD = nc.Dataset(nc_file).variables['ELT_RAD'][0,:,:,0]
#
# Angle of attack
AOA = nc.Dataset(nc_file).variables['AOA'][0,:,:,0]
AOA_MEAN = nc.Dataset(nc_file).variables['AOAMME'][0,:,:,0]
#
# Aerodynamic loads
FAERO_RE = nc.Dataset(nc_file).variables['FAERO_RE'][0,:,:,:,0]
FAERO_RE_MEAN = nc.Dataset(nc_file).variables['FAEROMME_RE'][0,:,:,:,0]
#
#
#       II. Preliminaries
#       -----------------
#
#	 a. Compute F/dr 
DR = RAD[1,0] - RAD[0,0]
FAERO_DR_RE = FAERO_RE/DR
FAERO_DR_RE_MEAN = FAERO_RE_MEAN/DR
#
#
#       II. Preliminaries
#       -----------------
#
IROT_ID = 0
for IBLA_ID in range(0,3):
#
#        a. Subplots
#
 fig, ax = plt.subplots(3, 2, sharex='col', figsize=(15,10.5))
 plt.suptitle('Blade ' + str(IBLA_ID) + ' analysis', fontsize=16)
 my_label = 'new'
 my_color = 'royalblue'
#
#	 b. AoA
#
 ax[0][0].plot(RAD[:,IBLA_ID], AOA[:,IBLA_ID]*180./np.pi, c=my_color, label=my_label, linestyle='-', linewidth=3)
#
#	 c. Mean(AoA)
#
 ax[0][1].plot(RAD[:,IBLA_ID], AOA_MEAN[:,IBLA_ID]*180./np.pi, c=my_color, label=my_label, linestyle='-', linewidth=3)
#
#	 d. Fn
# 
 ax[1][0].plot(RAD[:,IBLA_ID], FAERO_DR_RE[0,:,IBLA_ID], c=my_color, label=my_label, linestyle='-', linewidth=3)
#
#	 e. Mean(Fn)
# 
 ax[1][1].plot(RAD[:,IBLA_ID], FAERO_DR_RE_MEAN[0,:,IBLA_ID], c=my_color, label=my_label, linestyle='-', linewidth=3)
#
#	 f. Ft
# 
 ax[2][0].plot(RAD[:,IBLA_ID], -FAERO_DR_RE[1,:,IBLA_ID], c=my_color, label=my_label, linestyle='-', linewidth=3)
#
#	 g. Mean(Ft)
# 
 ax[2][1].plot(RAD[:,IBLA_ID], -FAERO_DR_RE_MEAN[1,:,IBLA_ID], c=my_color, label=my_label, linestyle='-', linewidth=3)
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
# plt.show()
 name = 'AERO_var_blade' + str(IBLA_ID) + '.png'
 plt.savefig(name,format='png')	

