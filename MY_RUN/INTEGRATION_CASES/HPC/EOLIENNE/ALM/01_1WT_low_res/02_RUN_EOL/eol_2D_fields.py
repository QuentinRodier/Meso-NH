# -*- coding: utf-8 -*-
"""
Created on Wed Dec 7 13:53:26 2016

@autor: Pierre-Antoine Joulin
"""
##########################################################
#INPUT :
# - Meso-NH file : .nc
# - Data file : data_farm.csv
# - Data file : data_turbine.csv
# - Variable name to plot
#OUTPUT :
# - Plot like :
#    ______________________________________________
#   |  ________________________________   _______  |
#   | |                                | | cut3  | |
#   | |       cut1 (X,Z)               | | (Y,Z) | |
#   | |________________________________| |_______| |
#   |  ________________________________            |
#   | |                                |           |
#   | |       cut2 (X,Y)               |           |
#   | |________________________________|           |
#   |______________________________________________|
#
##########################################################
import matplotlib
matplotlib.use("Agg")
import sys
import pandas as pd
import netCDF4 as nc
import numpy as np 
import matplotlib.pyplot as plt
from matplotlib import gridspec
from matplotlib.mlab import griddata
import warnings
warnings.filterwarnings("ignore")
#
#
#	0. Inputs
#	---------
#
#        a. Arguments
#
nc_file  = sys.argv[1]
svar     = sys.argv[2]
#
#	 b. Names
#
farm_file = "data_farm.csv"
turbine_file = "data_turbine.csv"
#
#	I. Data extraction
#       ------------------
#
#	 a. Extraction from csv data
#
# Root position
farm_data = pd.read_csv(farm_file)
Xpos = farm_data.values[0,0]
Ypos = farm_data.values[0,1]
# Hub height
turbine_data = pd.read_csv(turbine_file)
Hhub = turbine_data.values[0,2]
# Rotor diameter
R = turbine_data.values[0,4]
D = R*2
#
#	 b. Extraction from netCDF data
#
# Domain
IMAX = nc.Dataset(nc_file).variables['IMAX'][:]
JMAX = nc.Dataset(nc_file).variables['JMAX'][:]
KMAX = nc.Dataset(nc_file).variables['KMAX'][:]
HEXT = nc.Dataset(nc_file).variables['JPHEXT'][:]
KB = 1 + HEXT
#
# Mesh
XHAT = nc.Dataset(nc_file).variables['XHAT'][:]
YHAT = nc.Dataset(nc_file).variables['YHAT'][:]
ZHAT = nc.Dataset(nc_file).variables['ZHAT'][:]
#
# Orography
ZS = nc.Dataset(nc_file).variables['ZS'][:,:]
#
# Variable to plot
VAR = nc.Dataset(nc_file).variables[svar][0,:,:,:]
VARmin = VAR.min()
VARmax = VAR.max()
#
# Variables to print
THRUT   = nc.Dataset(nc_file).variables['THRUT'][:]
THRUMME = nc.Dataset(nc_file).variables['THRUMME'][:]
TORQT   = nc.Dataset(nc_file).variables['TORQT'][:]
TORQMME = nc.Dataset(nc_file).variables['TORQMME'][:]
POWT    = nc.Dataset(nc_file).variables['POWT'][:]
POWMME  = nc.Dataset(nc_file).variables['POWMME'][:]

#
#
#       II. Preliminaries
#       -----------------
#
#        a. Compute true vertical component
#
Htop = ZHAT[KMAX+1]                             # Domain height 
Z =  np.zeros((IMAX+2,JMAX+2,KMAX+2))           # New Z, "more cartesian"
for I in range(IMAX):
    for J in range (JMAX):
        for K in range (KMAX):
            Z[I,J,K] = ((Htop-ZS[J,I])/Htop)*ZHAT[K] + ZS[J,I]    #Gal-Chen & Sommerville
#
#	 b. Finding position indices
#
# Along X
Ipos = 0
while XHAT[Ipos] < Xpos:
 Ipos = Ipos + 1
# Along Y
Jpos = 0
while YHAT[Jpos] < Ypos:
 Jpos = Jpos + 1
# Along Z 
Kpos = 0
while Z[Ipos,Jpos,Kpos] < Hhub + ZS[Jpos,Ipos]:
 Kpos = Kpos + 1
#
#
#       III.1. Cut1 (X,Z)
#       -----------------
#
#	 a. Creating a proper array VARxz : [X, Z, VAR]  
#
VARxz_cut1 = np.zeros(((IMAX+2)*(KMAX+2-KB),3))
line = 0
for I in range(IMAX+2):
 for K in range (KB,KMAX+2):
  VARxz_cut1[line,0] = XHAT[I]
  VARxz_cut1[line,1] = Z[I,Jpos,K]
  VARxz_cut1[line,2] = VAR[K,Jpos,I]
  line = line + 1
#
#	 b. Meshgrid 
#
xi_cut1 = np.linspace(VARxz_cut1[:,0].min(), VARxz_cut1[:,0].max(), IMAX+2)
zi_cut1 = np.linspace(VARxz_cut1[:,1].min(), VARxz_cut1[:,1].max(), KMAX+2-KB+1)
xi_cut1, zi_cut1 = np.meshgrid(xi_cut1, zi_cut1)
#
#	 c. Interpolation 
#
vari_cut1 = griddata(VARxz_cut1[:,0],VARxz_cut1[:,1],VARxz_cut1[:,2], xi_cut1,zi_cut1, interp='linear')
#
#
#       III.2. Cut2 (X,Y)
#       -----------------
#
#	 a. Creating a proper array VARxy : [X, Y, VAR]  
#
VARxy_cut2 = np.zeros(((IMAX+2)*(JMAX+2),3))
line = 0
for I in range(IMAX+2):
 for J in range (JMAX+2):
  VARxy_cut2[line,0] = XHAT[I]
  VARxy_cut2[line,1] = YHAT[J]
  for K in range (KB,KMAX+2):
   if (ZHAT[K] <= Z[0,0,Kpos]) and (ZHAT[K+1] > Z[0,0,Kpos]):
    VARxy_cut2[line,2] = VAR[K,J,I]
    break
  line = line + 1
#
#	 b. Meshgrid 
#
xi_cut2 = np.linspace(VARxy_cut2[:,0].min(), VARxy_cut2[:,0].max(), IMAX+2)
yi_cut2 = np.linspace(VARxy_cut2[:,1].min(), VARxy_cut2[:,1].max(), JMAX+2)
xi_cut2, yi_cut2 = np.meshgrid(xi_cut2, yi_cut2)
#
#	 c. Interpolation 
#
vari_cut2 = griddata(VARxy_cut2[:,0],VARxy_cut2[:,1],VARxy_cut2[:,2], xi_cut2,yi_cut2, interp='linear')
#
#
#       III.3. Cut3 (Y,Z)
#       -----------------
#
#	 a. Creating a proper array VARyz : [Y, Z, VAR]  
#
VARyz_cut3 = np.zeros(((JMAX+2)*(KMAX+2-KB),3))
line = 0
for J in range(JMAX+2):
 for K in range (KB,KMAX+2):
  VARyz_cut3[line,0] = YHAT[J]
  VARyz_cut3[line,1] = Z[Ipos,J,K]
  VARyz_cut3[line,2] = VAR[K,J,Ipos]
  line = line + 1
#
#	 b. Meshgrid 
#
yi_cut3 = np.linspace(VARyz_cut3[:,0].min(), VARyz_cut3[:,0].max(), JMAX+2)
zi_cut3 = np.linspace(VARyz_cut3[:,1].min(), VARyz_cut3[:,1].max(), KMAX+2)
yi_cut3, zi_cut3 = np.meshgrid(yi_cut3, zi_cut3)
#
#	 c. Interpolation 
#
vari_cut3 = griddata(VARyz_cut3[:,0],VARyz_cut3[:,1],VARyz_cut3[:,2], yi_cut3,zi_cut3, interp='linear')
#
#
#	IV. Plots
#	---------
#
#	 a. Gridspec
#
fig = plt.figure(figsize=(15,8))
gs = gridspec.GridSpec(2, 14)    
#
ax_cut1 = fig.add_subplot(gs[0 ,:-5])
ax_cut2 = fig.add_subplot(gs[-1,:-5])
ax_cut3 = fig.add_subplot(gs[0 ,-4:-1])
#
#	 b. VAR field
#
my_levels = np.linspace(VARmin,VARmax,100)
my_map = 'jet'
#
ax1 = ax_cut1.contourf(xi_cut1, zi_cut1, vari_cut1, cmap=my_map, levels=my_levels, extend='both')
ax2 = ax_cut2.contourf(xi_cut2, yi_cut2, vari_cut2, cmap=my_map, levels=my_levels, extend='both')
ax3 = ax_cut3.contourf(yi_cut3, zi_cut3, vari_cut3, cmap=my_map, levels=my_levels, extend='both')
#
#	 c. Orography
#
ax_cut1.fill_between(XHAT, 0, ZS[Jpos,:], facecolor='black')
ax_cut3.fill_between(YHAT, 0, ZS[:,Ipos], facecolor='black')
#
#	 d. Wind turbine
#
ax_cut1.plot([Xpos,Xpos], [Hhub+ZS[Jpos,Ipos]-R,Hhub+ZS[Jpos,Ipos]+R], 'black', lw=2)
ax_cut2.plot([Xpos,Xpos], [Ypos-R,Ypos+R], 'black', lw=2)
ax_cut3.add_artist(plt.Circle((Ypos, Hhub+ZS[Jpos,Ipos]), R, color='black', lw=1, fill=False))
#
#	 e. Cut planes
#
ax_cut1.plot([XHAT[Ipos],XHAT[Ipos]], [ZHAT[0], ZHAT[KMAX]], 'w--', lw=1)
ax_cut1.plot([XHAT[0],XHAT[IMAX]], [Z[Ipos,Jpos,Kpos],Z[Ipos,Jpos,Kpos]], 'w--', lw=1)
#
ax_cut2.plot([XHAT[Ipos],XHAT[Ipos]], [YHAT[0], YHAT[JMAX]], 'w--', lw=1)
ax_cut2.plot([XHAT[0],XHAT[IMAX]], [YHAT[Jpos], YHAT[Jpos]], 'w--', lw=1)
#
ax_cut3.plot([YHAT[Jpos],YHAT[Jpos]], [ZHAT[0], ZHAT[KMAX]], 'w--', lw=1)
ax_cut3.plot([YHAT[0],YHAT[JMAX]], [Z[Ipos,Jpos,Kpos],Z[Ipos,Jpos,Kpos]], 'w--', lw=1)
#
#	 f. Some text
#
csfont = {'fontname':'Times New Roman'}
#
ax_cut1.set_title(svar + ' at $Y = %.1f$ m' % YHAT[Jpos], **csfont)
ax_cut1.set_xlabel('x (m)', **csfont)
ax_cut1.set_ylabel('z (m)', **csfont)
ax_cut1.set_xlim(Xpos-D, Xpos+11*D)
ax_cut1.set_ylim(ZHAT[KB], 3*D)
ax_cut1.set_aspect(1)
#
ax_cut2.set_title(svar + ' at $Z= %.1f$ m' % Z[0,0,Kpos], **csfont)
ax_cut2.set_xlabel('x (m)', **csfont)
ax_cut2.set_ylabel('y (m)', **csfont)
ax_cut2.set_xlim(Xpos-D, Xpos+11*D)
ax_cut2.set_ylim(Ypos-3*R, Ypos+3*R)
ax_cut2.set_aspect(1)
#
ax_cut3.set_title(svar + ' at $X = %.1f$ m' %XHAT[Ipos], **csfont)
ax_cut3.set_xlabel('y (m)', **csfont)
ax_cut3.set_ylabel('z (m)', **csfont)
ax_cut3.set_xlim(Ypos-3*R, Ypos+3*R)
ax_cut3.invert_xaxis()
ax_cut3.set_ylim(ZHAT[KB], 3*D)
ax_cut3.set_aspect(1)
#
cbar = fig.colorbar(ax3)
cbar.ax.set_title(svar,**csfont)
#
textstr = '\n'.join((
    r'$F_T = %.2f$ kN' % (THRUT/1000, ),
    r'$\overline{F_T} =%.2f$ kN' % (THRUMME/1000, ),
    r'$T =%.2f$ MNm' % (TORQT/1000000, ),
    r'$\overline{T} = %.2f$ MNm' % (TORQMME/1000000, ),
    r'$P = %.2f$ MW' % (POWT/1000000, ),
    r'$\overline{P} = %.2f$ MW' % (POWMME/1000000, )))
props = dict(boxstyle='round', facecolor='wheat', alpha=0.5)
ax_cut3.text(0.05, -0.9, textstr,transform=ax_cut3.transAxes, fontsize=14,
        verticalalignment='top', bbox=props)
#
#
#plt.show()
#
#
#	V. Saving
#	---------
#
name = svar+ '_2D_Fields.png'
plt.savefig(name,format='png')
