#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jun 19 16:17:23 2020

@author: nagelt
"""

import matplotlib.pyplot as plt
import matplotlib
matplotlib.use('Agg')
plt.ioff()
from netCDF4 import Dataset
import numpy as np
import matplotlib.gridspec as gridspec
from scipy import integrate

from matplotlib import rc
rc('font',**{'family':'sans-serif','sans-serif':['Helvetica']})
## for Palatino and other serif fonts use:
#rc('font',**{'family':'serif','serif':['Palatino']})
#rc('text', usetex=True)

matplotlib.rcParams['xtick.labelsize'] = 16
matplotlib.rcParams['ytick.labelsize'] = 16


##########################
# Constantes
##########################
jphext = 1
jpvext = 1
H = 0.6396       #Cube height
rho = 1.184      #Fluid density
dx = 0.0312      #Mesh size x-direction
dy = 0.0312      #Mesh size y-direction
dxdy = 0.0312**2 #Cell surface

#Cubes boundaries
xf=479       #x-front face position
xb=500       #x-back face position
yf=70        #y-front face position
yb=89        #y-back face position
zt=20        #ztop

zt2=int(zt/2)#z middle
ys=80        #y middle
ys1=89

#################################
# Lecture des données MESO-NH-CFD
#################################
path = '../002_mesonh/'


file= path + 'MNHOU.1.SEGM.003.nc'

ncfile1 = Dataset(file,'r')
x=ncfile1.variables['XHAT'][:]
y=ncfile1.variables['YHAT'][:]
z=ncfile1.variables['ZHAT'][:]
p=ncfile1.variables['PABSMME'][0,:,:,:]
ut=ncfile1.variables['UT'][0,:,:,:]
ume=ncfile1.variables['UMME'][0,:,:,:]
pvol=ncfile1.variables['LSFP'][0,:,:,:]

#Interpolation at mass point (P)
lenx=len(x)
x=0.5*(x[0:(lenx-1)]+x[1:lenx])
ut=0.5*(ut[:,:,0:(lenx-1)]+ut[:,:,1:lenx])
ume=0.5*(ume[:,:,0:(lenx-1)]+ume[:,:,1:lenx])
lenx=len(x)

leny=len(y)
y=0.5*(y[0:(leny-1)]+y[1:leny])
leny=len(y)

lenz=len(z)
z=0.5*(z[0:(lenz-1)]+z[1:lenz])
lenz=len(z)

x=x[jphext:lenx]
y=y[jphext:leny] 
z=z[jpvext:lenz]
p=p[jpvext:lenz,jphext:leny,jphext:lenx]
ut=ut[jpvext:lenz,jphext:leny,jphext:lenx]
ume=ume[jpvext:lenz,jphext:leny,jphext:lenx]
pvol=pvol[jpvext:lenz,jphext:leny,jphext:lenx]


#Pressure difference
psum=np.sum((p[0:zt,yf:yb,xf]-p[0:zt,yf:yb,xb])*dxdy)

#Free stream Velocity
maxu= ume[zt2,ys,100]

#Total drag coefficient
Cdtot= 2*psum/(rho*maxu**2*(H*(y[yb]-y[yf])))

print('Obstacle drag coeffcient: ', Cdtot)
print('Modi and Deshpande (2001): ', 1.18)
