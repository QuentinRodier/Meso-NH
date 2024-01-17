# -*- coding: utf-8 -*-

import matplotlib as mpl
mpl.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap
from matplotlib.colors import LinearSegmentedColormap
from netCDF4 import Dataset
import numpy as np
from pylab import *


import sys

import glob
import os
import matplotlib
import math

##########################
# Constantes
##########################

#### colormap
class nlcmap(LinearSegmentedColormap):
    """A nonlinear colormap"""

    name = 'nlcmap'

    def __init__(self, cmap, levels):
        self.cmap = cmap
        self.monochrome = self.cmap.monochrome
        self.levels = asarray(levels, dtype='float64')
        self._x = self.levels-self.levels.min()
        self._x/= self._x.max()
        self._y = linspace(0, 1, len(self.levels))

    def __call__(self, xi, alpha=1.0, **kw):
        yi = interp(xi, self._x, self._y)
        return self.cmap(yi, alpha)



##########################
# Lectures des données
##########################



fic1='CLB50.1.03_04.008.nc' #chemin vers simu
fic2='CLB50.1.03_04.002.nc' #chemin vers simu


ncfile1 = Dataset(fic1,'r')

ncfile2 = Dataset(fic2,'r')

lat=ncfile1.variables['LAT'][:,:]
lon=ncfile1.variables['LON'][:,:]
latmin=np.nanmin(lat)
latmax=np.nanmax(lat)
lonmin=np.nanmin(lon)
lonmax=np.nanmax(lon)
proj='merc'


SNWM01T=ncfile1.variables['SNWM01'][0,:,:,:]  
SNWM01T[np.where(SNWM01T==999.)]=float('nan')

SNWM02T=ncfile1.variables['SNWM02'][0,:,:,:]  
SNWM02T[np.where(SNWM02T==999.)]=float('nan')

CANSNW_M2L01=ncfile1.variables['CANSNW_M2L01'][:,:]  
CANSNW_M2L01[np.where(CANSNW_M2L01==999.)]=float('nan')

SNOW_SALT=ncfile1.variables['SNOW_SALT'][:,:]  
SNOW_SALT[np.where(SNOW_SALT==999.)]=float('nan')

COL_SNWSUBL=ncfile1.variables['COL_SNWSUBL'][0,:,:]  
COL_SNWSUBL[np.where(COL_SNWSUBL==999.)]=float('nan')

SNW_SUBL_INS=ncfile1.variables['SNW_SUBL_INS'][:,:]  
SNW_SUBL_INS[np.where(SNW_SUBL_INS==999.)]=float('nan')

SNW_SUBL_ACC=ncfile1.variables['SNW_SUBL_ACC'][:,:]  
SNW_SUBL_ACC[np.where(SNW_SUBL_ACC==999.)]=float('nan')


WSN_T_ISBA=ncfile1.variables['WSN_T_ISBA'][:,:]  
WSN_T_ISBA[np.where(WSN_T_ISBA==999.)]=float('nan')


WSN_T_ISBA_0=ncfile2.variables['WSN_T_ISBA'][:,:]  
WSN_T_ISBA_0[np.where(WSN_T_ISBA==999.)]=float('nan')


x,y = lon,lat
cs = contourf(x,y,SNWM01T[2,:,:],cmap=get_cmap("rainbow"),extend='both')
cb=plt.colorbar(cs)
plt.title('SNWM01 : level 2')
cb.set_label('kg/kg', labelpad=-40, y=1.05, rotation=0)        
nom_fig='SNWM01_L2.png'
plt.savefig(nom_fig,format='png')
plt.close()

cs = contourf(x,y,SNWM01T[4,:,:],cmap=get_cmap("rainbow"),extend='both')
cb=plt.colorbar(cs)
plt.title('SNWM01 : level 4')
cb.set_label('kg/kg', labelpad=-40, y=1.05, rotation=0)  
nom_fig='SNWM01_L4.png'      
plt.savefig(nom_fig,format='png')
plt.close()

cs = contourf(x,y,SNWM02T[2,:,:],cmap=get_cmap("rainbow"),extend='both')
cb=plt.colorbar(cs)
plt.title('SNWM02 : level 2')
cb.set_label('kg/kg', labelpad=-40, y=1.05, rotation=0)        
nom_fig='SNWM02_L2.png'
plt.savefig(nom_fig,format='png')
plt.close()

cs = contourf(x,y,SNWM02T[4,:,:],cmap=get_cmap("rainbow"),extend='both')
cb=plt.colorbar(cs)
plt.title('SNWM02T : level 4')
cb.set_label('kg/kg', labelpad=-40, y=1.05, rotation=0)  
nom_fig='SNWM02T_L4.png'      
plt.savefig(nom_fig,format='png')
plt.close()


cs = contourf(x,y,CANSNW_M2L01[:,:],cmap=get_cmap("rainbow"),extend='both')
cb=plt.colorbar(cs)
plt.title('CANSNW_M2L01')
cb.set_label('kg/kg', labelpad=-40, y=1.05, rotation=0)  
nom_fig='CANSNW_M2L01.png'      
plt.savefig(nom_fig,format='png')
plt.close()


cs = contourf(x,y,SNOW_SALT[:,:],cmap=get_cmap("rainbow"),extend='both')
cb=plt.colorbar(cs)
plt.title('SNOW_SALT')
cb.set_label('kg/m/s', labelpad=-40, y=1.05, rotation=0)  
nom_fig='SNOW_SALT.png'      
plt.savefig(nom_fig,format='png')
plt.close()

cs = contourf(x,y,COL_SNWSUBL[:,:],cmap=get_cmap("rainbow"),extend='both')
cb=plt.colorbar(cs)
plt.title('COL_SNWSUBL')
cb.set_label('mm/day', labelpad=-40, y=1.05, rotation=0)  
nom_fig='COL_SNWSUBL.png'      
plt.savefig(nom_fig,format='png')
plt.close()



cs = contourf(x,y,SNW_SUBL_INS[:,:],cmap=get_cmap("rainbow"),extend='both')
cb=plt.colorbar(cs)
plt.title('SNW_SUBL_INS')
cb.set_label('mmSWE/day', labelpad=-40, y=1.05, rotation=0)  
nom_fig='SNW_SUBL_INS.png'      
plt.savefig(nom_fig,format='png')
plt.close()

cs = contourf(x,y,SNW_SUBL_ACC[:,:],cmap=get_cmap("rainbow"),extend='both')
cb=plt.colorbar(cs)
plt.title('SNW_SUBL_ACC')
cb.set_label('kg/m2', labelpad=-40, y=1.05, rotation=0)  
nom_fig='SNW_SUBL_ACC.png'      
plt.savefig(nom_fig,format='png')
plt.close()


DIFF=WSN_T_ISBA-WSN_T_ISBA_0
cs = contourf(x,y,DIFF[:,:],cmap=get_cmap("coolwarm"),extend='both')
cb=plt.colorbar(cs)
plt.title('WSN_T_ISBA t=10800s - t=0s')
cb.set_label('kg/m2', labelpad=-40, y=1.05, rotation=0)  
nom_fig='diff_.WSN_T_ISBA.png'      
plt.savefig(nom_fig,format='png')
plt.close()

