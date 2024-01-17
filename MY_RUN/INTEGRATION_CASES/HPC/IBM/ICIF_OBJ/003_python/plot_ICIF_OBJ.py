#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Plot a velocity vertical profile and a 2D birdview of the wind speed field.
Case ICIF (Immeubles continus sur ilots fermes)

Original (21/06/21): Tim Nagel (Meteo-France) 
"""

import matplotlib.pyplot as plt
import matplotlib
matplotlib.use('Agg')
plt.ioff()
from netCDF4 import Dataset
import numpy as np
from scipy.optimize import curve_fit
import matplotlib.gridspec as gridspec

from matplotlib import rc
rc('font',**{'family':'sans-serif','sans-serif':['Helvetica']})

matplotlib.rcParams['xtick.labelsize'] = 16
matplotlib.rcParams['ytick.labelsize'] = 16


##########################
# Constantes
##########################
jphext=1
jpvext=1

#Function READ
def f_read(file,px0,px1,py0,py1):
    ncfile1 = Dataset(file,'r')
    ume=ncfile1.variables['UMME'][0,:,:,:]
    vme=ncfile1.variables['VMME'][0,:,:,:]
    ut=ncfile1.variables['UT'][0,:,:,:]
    vt=ncfile1.variables['VT'][0,:,:,:]
    k=ncfile1.variables['TKEMME'][0,:,:,:]
    ku=ncfile1.variables['U2ME'][0,:,:,:]
    kv=ncfile1.variables['V2ME'][0,:,:,:]
    x=ncfile1.variables['XHAT'][:]
    y=ncfile1.variables['YHAT'][:]
    z=ncfile1.variables['ZHAT'][:]
    lsfp=ncfile1.variables['LSFP'][0,:,:,:]

    #Interpolation au point de masse
    lenx=len(x)
    x=0.5*(x[0:(lenx-1)]+x[1:lenx])
    ume=0.5*(ume[:,:,0:(lenx-1)]+ume[:,:,1:lenx])
    ut=0.5*(ut[:,:,0:(lenx-1)]+ut[:,:,1:lenx])
    ku=0.5*(ku[:,:,0:(lenx-1)]+ku[:,:,1:lenx])
    lenx=len(x)

    leny=len(y)
    y=0.5*(y[0:(leny-1)]+y[1:leny])
    vme=0.5*(vme[:,0:(leny-1),:]+vme[:,1:leny,:])
    vt=0.5*(vt[:,0:(leny-1),:]+vt[:,1:leny,:])
    kv=0.5*(kv[:,0:(leny-1),:]+kv[:,1:leny,:])
    leny=len(y)

    lenz=len(z)
    z=0.5*(z[0:(lenz-1)]+z[1:lenz])
    lenz=len(z)

    x=x[jphext:lenx]
    y=y[jphext:leny] 
    z=z[jpvext:lenz]
    ume=ume[jpvext:lenz,jphext:leny,jphext:lenx]
    vme=vme[jpvext:lenz,jphext:leny,jphext:lenx]  
    ut=ut[jpvext:lenz,jphext:leny,jphext:lenx]
    vt=vt[jpvext:lenz,jphext:leny,jphext:lenx]  
    k=k[jpvext:lenz,jphext:leny,jphext:lenx]
    ku=ku[jpvext:lenz,jphext:leny,jphext:lenx]
    kv=kv[jpvext:lenz,jphext:leny,jphext:lenx]
    lsfp=lsfp[jpvext:lenz,jphext:leny,jphext:lenx]    

    speedM2D = np.sqrt(ume**2+vme**2)
    speed2D =  np.sqrt(ut**2+vt**2)

    ume[np.where(lsfp>0.)] = np.nan
    vme[np.where(lsfp>0.)] = np.nan    

    ume=ume[:,py0:py1,px0:px1]
    ume=np.nanmean(ume,axis=1)
    ume=np.nanmean(ume,axis=1)

    vme=vme[:,py0:py1,px0:px1]
    vme=np.nanmean(vme,axis=1)
    vme=np.nanmean(vme,axis=1)

    speed = np.sqrt(ume**2+vme**2)
    dvent = np.arctan2(vme,ume)*180/np.pi

    tke=k[:,py0:py1,px0:px1]
    tke=np.mean(tke,axis=1)
    tke=np.mean(tke,axis=1)

    ku=ku[:,py0:py1,px0:px1]
    ku=np.mean(ku,axis=1)
    ku=np.mean(ku,axis=1)

    kv=kv[:,py0:py1,px0:px1]
    kv=np.mean(kv,axis=1)
    kv=np.mean(kv,axis=1)
    
    return (x,y,z,lsfp,speedM2D,speed2D,speed,dvent,tke,ku,kv,ume,vme)

###############################
# Lecture des données MESO-NH
###############################
#
#Vertical profile
#Constantes
px0=300
px1=900
py0=300
py1=900

path = '../002_mesonh/'
file= path + 'MNHOU.1.SEGM.001.nc'
ncfile1 = Dataset(file,'r')
x,y,z,lsfp,speedM2D,speed2D,speedMeCFD,dventMeCFD,kMeCFD,kuM,kvM,umeM,vmeM = f_read(file,px0,px1,py0,py1)
#
##########################
# Tracé des figures
##########################
gs12 = gridspec.GridSpec(1,1)

gs2D = gridspec.GridSpec(1,1)

gs2D.update(right=0.9,\
                    left=0.1,\
                    bottom=0.025,\
                    top=0.96,\
                    wspace=0.1,hspace=0.1)


#
#--------FIG1: Vertical profiles in ICIF
#
fig = plt.figure(num=1,figsize=(7,7),dpi=100, facecolor='w', edgecolor='w')
ax = fig.add_subplot(gs12[0,0])
plt.plot(speedMeCFD[:],z,'--k',linewidth=2)
plt.xlim(0.,10)
plt.ylim(0.,140)
plt.ylabel(r'Altitude (m)',fontsize=20)
plt.xlabel(r'Wind speed (m.s$^{-1}$)',fontsize=20)

#ax = fig.add_subplot(gs12[0,1])
#plt.scatter(dventInlet,zInleta,s=50,c='k',marker='o', zorder=10, clip_on=False)
#plt.plot(dventMeCFD[:],z,'--k',linewidth=2)
#plt.xlim(-100,-20)
#plt.ylim(0.,20)
#ax.set_yticklabels([])
#plt.xlabel(r'Wind direction ($^{\circ}$)',fontsize=20)

#ax = fig.add_subplot(gs12[0,2])
#plt.scatter(tkeInlet,zInleta,s=50,c='k',marker='o', zorder=10, clip_on=False)
#plt.plot(tkeMeCFD[:],z,'--k',linewidth=2)
#plt.ylim(0.,20)
#ax.set_yticklabels([])
#plt.xlabel(r'TKE (m$^2$.s$^{-2}$)',fontsize=20)

#fig.legend(loc=9,ncol=4,fontsize=16,numpoints=1)

plt.savefig('ICIF_profiles.png',dpi = 300)

#
#--------FIG3: 2D birdviews
#
fig = plt.figure(num=3,figsize=(16,15),dpi=100, facecolor='w', edgecolor='w')
ax = fig.add_subplot(gs2D[0,0])
levels = np.linspace(0.0,10.,256)
CS_colors=plt.contourf(x, y, speedM2D[2,:,:], levels,cmap=plt.cm.coolwarm, extend='max')
plt.contourf(x, y, lsfp[2,:,:], [0,999.], colors=['grey', 'grey'])
plt.ylabel(r'y(m)',fontsize=20)
plt.xlabel(r'x(m)',fontsize=20)
ax.set_aspect('equal')

fig.subplots_adjust(right=0.92)
cbar_ax = fig.add_axes([0.92, 0.05, 0.0225, 0.88])
cb=fig.colorbar(CS_colors, cax=cbar_ax)
cbar_ticks = np.linspace(0., 10., num=6, endpoint=True)
cb.set_ticks(cbar_ticks)
cb.ax.tick_params(labelsize=20)
cb.set_label(r' U (m.s$^{-1}$)',fontsize=20)
cb.update_ticks()


plt.savefig('ICIF_2D_UMEAN.png',dpi = 300)
