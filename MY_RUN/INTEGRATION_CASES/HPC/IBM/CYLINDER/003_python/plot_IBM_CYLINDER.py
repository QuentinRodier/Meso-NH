#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
 Original: Tim Nagel, Météo-France, January 2021
"""

import matplotlib
from netCDF4 import Dataset
import matplotlib.pyplot as plt
matplotlib.use('Agg')
plt.ioff()
import numpy as np
import matplotlib.gridspec as gridspec
from mpl_toolkits.axes_grid1 import make_axes_locatable
from matplotlib.colors import Normalize

from matplotlib import rc
rc('font',**{'family':'sans-serif','sans-serif':['Helvetica']})

matplotlib.rcParams['xtick.labelsize'] = 16
matplotlib.rcParams['ytick.labelsize'] = 16


#If you have Matplotlib 3.1 use DivergingNorm, if you have 3.2 use TwoSlopesNorm
class MidpointNormalize(Normalize):
    def __init__(self, vmin=None, vmax=None, midpoint=None, clip=False):
        self.midpoint = midpoint
        Normalize.__init__(self, vmin, vmax, clip)

    def __call__(self, value, clip=None):
        x, y = [self.vmin, self.midpoint, self.vmax], [0, 0.5, 1]
        return np.ma.masked_array(np.interp(value, x, y))

##########################
# Constants
##########################
D=320.         #Cylinder Diameter
jphext=1
jpvext=1

###############################
# MESO-NH output reading
###############################
path = '../002_mesonh/'
file= path + 'MNHOU.1.SEGM.010.nc'
ncfile1 = Dataset(file,'r')
u=ncfile1.variables['UMME'][0,:,:,:]
v=ncfile1.variables['VMME'][0,:,:,:]
x=ncfile1.variables['XHAT'][:]
y=ncfile1.variables['YHAT'][:]
z=ncfile1.variables['ZHAT'][:]
lsfp=ncfile1.variables['LSFP'][0,:,:,:]

#Mass point Interpolation
lenx=len(x)
x=0.5*(x[0:(lenx-1)]+x[1:lenx])
u=0.5*(u[:,:,0:(lenx-1)]+u[:,:,1:lenx])
lenx=len(x)

leny=len(y)
y=0.5*(y[0:(leny-1)]+y[1:leny])
v=0.5*(v[:,0:(leny-1),:]+v[:,1:leny,:])
leny=len(y)

lenz=len(z)
z=0.5*(z[0:(lenz-1)]+z[1:lenz])
lenz=len(z)

x=x[jphext:lenx]
y=y[jphext:leny] 
z=z[jpvext:lenz]
u=u[jpvext:lenz,jphext:leny,jphext:lenx]
v=v[jpvext:lenz,jphext:leny,jphext:lenx]
lsfp=lsfp[jpvext:lenz,jphext:leny,jphext:lenx]

#########################
# Recirculating length
##########################
i=220
j=200
ib=216
zi = 4
xdown=np.zeros(np.size(x[i:]))
xdown=x[i:]
L=(np.min(xdown[np.where(u[zi,j,i:]>0.)])-x[ib])/D

print(r'Literature Recirculating length (Lr/D): [2.1-2.3]')
print('MNH Recirculating length (Lr/D): ', L)


#########################
# Figures
##########################
gs2 = gridspec.GridSpec(1,1)

uplot=u[zi,:,:]
vplot=v[zi,:,:]

#--------FIG:
fig = plt.figure(num=1,figsize=(16,8),dpi=100, facecolor='w', edgecolor='w')
ax = fig.add_subplot(gs2[0,0])
levels = np.linspace(np.min(uplot),np.max(uplot),256)
norm = MidpointNormalize(midpoint=0)
CS_colors=plt.contourf(x, y, uplot, levels, norm=norm, cmap=plt.cm.coolwarm)
CS_lines = plt.streamplot(x,y,uplot,vplot,density=5,color='k')
plt.contourf(x, y, lsfp[5,:,:], [0,200.], colors=['grey', 'grey'],zorder=10)
plt.xlim(1800,3200)
plt.ylim(1500,2500)
plt.xlabel(r'x(m)',fontsize=20)
plt.ylabel(r'y(m)',fontsize=20)
ax.set_aspect('equal')

divider = make_axes_locatable(ax)
cbar_ax = divider.append_axes("right", size="3%", pad=0.05)
cb=plt.colorbar(CS_colors, cax=cbar_ax)
cb.formatter.set_powerlimits((0, 0))
cb.set_label(r'$\overline{U}(m.s^{-1})$',fontsize=20)
cb.update_ticks()

plt.savefig('IBM_CYLINDER.png',dpi = 300)












