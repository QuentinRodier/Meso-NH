#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Dec  6 16:03:32 2019

@author: nagelt
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
    
    ume=ume[:,py0:py1,px0:px1]
    ume=np.mean(ume,axis=1)
    ume=np.mean(ume,axis=1)

    vme=vme[:,py0:py1,px0:px1]
    vme=np.mean(vme,axis=1)
    vme=np.mean(vme,axis=1)

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
#Tower S (outside the containers array)
#Constantes
px0=260
px1=310
py0=200
py1=800

path = '../005_MNHD2/'
file= path + 'MNHOU.2.SEG1.001.nc'
ncfile1 = Dataset(file,'r')
x,y,z,lsfp,speedM2D,speed2D,speedMeCFD,dventMeCFD,kMeCFD,kuM,kvM,umeM,vmeM = f_read(file,px0,px1,py0,py1)
#
tkeMeCFD = 0.5*(kuM+kvM)+kMeCFD**(2./3.)

#MAST T (within the containers array)
#Constantes
px0=636
px1=638
py0=559
py1=561

ncfile1 = Dataset(file,'r')
x,y,z,lsfp,speedM2D,speed2D,speedMeCFD_T,dventMeCFD_T,kMeCFD_T,kuMT,kvMT,umeMT,vmeMT = f_read(file,px0,px1,py0,py1)
#
tkeMeCFD_T = 0.5*(kuMT+kvMT)+kMeCFD_T**(2./3.)


###############################
# Lecture des sondes EXP
###############################
#Données du vent
path = 'MUST_DATA/'

#Tour 16m Inlet
file= path + 'm2681815_S3.dat'
t16,u16,v16 = np.loadtxt(file,usecols=(0, 1, 2),unpack=True)
#Direction du vent
dventI = 180.0/np.pi*np.arctan2(v16,u16)
#Vitesse du vent
speed16I = np.sqrt(u16**2 + v16**2)
#tke
up = u16-np.mean(u16[9000:18000])
vp = v16-np.mean(v16[9000:18000])
tke16S = np.mean(0.5*(up[9000:18000]**2+vp[9000:18000]**2))
tke16 = (0.5*(up[9000:18000]**2+vp[9000:18000]**2))
#
file= path + 'm2681815_S2.dat'
t8,u8,v8 = np.loadtxt(file,usecols=(0, 1, 2),unpack=True)
dvent8I = 180.0/np.pi*np.arctan2(v8,u8)
speed8I = np.sqrt(u8**2 + v8**2)
#tke
up = u8-np.mean(u8[9000:18000])
vp = v8-np.mean(v8[9000:18000])
tke8S = np.mean(0.5*(up[9000:18000]**2+vp[9000:18000]**2))
tke8 = (0.5*(up[9000:18000]**2+vp[9000:18000]**2))
#
file= path + 'm2681815_S1.dat'
t4,u4,v4 = np.loadtxt(file,usecols=(0, 1, 2),unpack=True)
dvent4I = 180.0/np.pi*np.arctan2(v4,u4)
speed4I = np.sqrt(u4**2 + v4**2)
#tke
up = u4-np.mean(u4[9000:18000])
vp = v4-np.mean(v4[9000:18000])
tke4 = (0.5*(up[9000:18000]**2+vp[9000:18000]**2))
tke4S = np.mean(0.5*(up[9000:18000]**2+vp[9000:18000]**2))

sInlet=[np.mean(speed4I[9000:18000]),np.mean(speed8I[9000:18000]),np.mean(speed16I[9000:18000])]
dventInlet=[np.mean(dvent4I[9000:18000]),np.mean(dvent8I[9000:18000]),np.mean(dventI[9000:18000])]
tkeInlet=[tke4S,tke8S,tke16S]
zInleta=[4.,8.,16.]


#######Mat 32m
file= path + 'm2681815_T4.dat'
t32,u32,v32 = np.loadtxt(file,usecols=(0, 1, 2),unpack=True)
#Direction du vent
dvent32 = 180.0/np.pi*np.arctan2(v32,u32)
#Vitesse du vent
speed32 = np.sqrt(u32**2 + v32**2)
std32=np.std(dvent32[9000:18000])
std32s=np.std(speed32[9000:18000])
up = u32-np.mean(u32[9000:18000])
vp = v32-np.mean(v32[9000:18000])
tke32M = np.mean(0.5*(up[9000:18000]**2+vp[9000:18000]**2))
file= path + 'm2681815_T3.dat'
t16M,u16,v16 = np.loadtxt(file,usecols=(0, 1, 2),unpack=True)
dvent16 = np.arctan2(v16,u16)*180/np.pi
speed16 = np.sqrt(u16**2 + v16**2)
std16=np.std(dvent16[9000:18000])
std16s=np.std(speed16[9000:18000])
up = u16-np.mean(u16[9000:18000])
vp = v16-np.mean(v16[9000:18000])
tke16M = np.mean(0.5*(up[9000:18000]**2+vp[9000:18000]**2))
file= path + 'm2681815_T2.dat'
t8M,u8,v8 = np.loadtxt(file,usecols=(0, 1, 2),unpack=True)
dvent8 = np.arctan2(v8,u8)*180/np.pi
speed8 = np.sqrt(u8**2 + v8**2)
std8=np.std(dvent8[9000:18000])
std8s=np.std(speed8[9000:18000])
up = u8-np.mean(u8[9000:18000])
vp = v8-np.mean(v8[9000:18000])
tke8M = np.mean(0.5*(up[9000:18000]**2+vp[9000:18000]**2))
file= path + 'm2681815_T1.dat'
t4M,u4,v4 = np.loadtxt(file,usecols=(0, 1, 2),unpack=True)
dvent4 = np.arctan2(v4,u4)*180/np.pi
speed4 = np.sqrt(u4**2 + v4**2)
std4=np.std(dvent4[9000:18000])
std4s=np.std(speed4[9000:18000])
up = u4-np.mean(u4[9000:18000])
vp = v4-np.mean(v4[9000:18000])
tke4M = np.mean(0.5*(up[9000:18000]**2+vp[9000:18000]**2))

sMast=[np.mean(speed4[9000:18000]),np.mean(speed8[9000:18000]),np.mean(speed16[9000:18000]),np.mean(speed32[9000:18000])]
dventMast=[np.mean(dvent4[9000:18000]),np.mean(dvent8[9000:18000]),np.mean(dvent16[9000:18000]),np.mean(dvent32[9000:18000])]
tkeMast=[tke4M,tke8M,tke16M,tke32M]
zMast=[4.,8.,16.,32.]


##########################
# Tracé des figures
##########################
gs12 = gridspec.GridSpec(1,3)

gs2D = gridspec.GridSpec(1,1)

gs2D.update(right=0.9,\
                    left=0.1,\
                    bottom=0.025,\
                    top=0.96,\
                    wspace=0.1,hspace=0.1)


#
#--------FIG1: Vertical profiles at tower S
#
fig = plt.figure(num=1,figsize=(15,7),dpi=100, facecolor='w', edgecolor='w')
ax = fig.add_subplot(gs12[0,0])
plt.scatter(sInlet,zInleta,s=50,c='k',marker='o', zorder=10, clip_on=False,label=r'Yee & Biltoft [2004]')
plt.plot(speedMeCFD[:],z,'--k',linewidth=2,label=r'CFD')
plt.xlim(0.,15)
plt.ylim(0.,20)
plt.ylabel(r'Altitude (m)',fontsize=20)
plt.xlabel(r'Wind speed (m.s$^{-1}$)',fontsize=20)

ax = fig.add_subplot(gs12[0,1])
plt.scatter(dventInlet,zInleta,s=50,c='k',marker='o', zorder=10, clip_on=False)
plt.plot(dventMeCFD[:],z,'--k',linewidth=2)
plt.xlim(-100,-20)
plt.ylim(0.,20)
ax.set_yticklabels([])
plt.xlabel(r'Wind direction ($^{\circ}$)',fontsize=20)

ax = fig.add_subplot(gs12[0,2])
plt.scatter(tkeInlet,zInleta,s=50,c='k',marker='o', zorder=10, clip_on=False)
plt.plot(tkeMeCFD[:],z,'--k',linewidth=2)
plt.ylim(0.,20)
ax.set_yticklabels([])
plt.xlabel(r'TKE (m$^2$.s$^{-2}$)',fontsize=20)

fig.legend(loc=9,ncol=4,fontsize=16,numpoints=1)

plt.savefig('MUST_profil_S_testcase.png',dpi = 300)

#
#--------FIG2: Vertical profiles at Mast T
#
fig = plt.figure(num=2,figsize=(15,7),dpi=100, facecolor='w', edgecolor='w')
ax = fig.add_subplot(gs12[0,0])
plt.scatter(sMast,zMast,s=50,c='k',marker='o', zorder=10, clip_on=False,label=r'Yee & Biltoft [2004]')
plt.plot(speedMeCFD_T[:],z,'--k',linewidth=2,label=r'CFD')
plt.xlim(0.,15)
plt.ylim(0.,35)
plt.ylabel(r'Altitude (m)',fontsize=20)
plt.xlabel(r'Wind speed (m.s$^{-1}$)',fontsize=20)

ax = fig.add_subplot(gs12[0,1])
plt.scatter(dventMast,zMast,s=50,c='k',marker='o', zorder=10, clip_on=False)
plt.plot(dventMeCFD_T[:],z,'--k',linewidth=2)
plt.xlim(-100,-20)
plt.ylim(0.,35)
ax.set_yticklabels([])
plt.xlabel(r'Wind direction ($^{\circ}$)',fontsize=20)

ax = fig.add_subplot(gs12[0,2])
plt.scatter(tkeMast,zMast,s=50,c='k',marker='o', zorder=10, clip_on=False)
plt.plot(tkeMeCFD_T[:],z,'--k',linewidth=2)
plt.ylim(0.,35)
ax.set_yticklabels([])
plt.xlabel(r'TKE (m$^2$.s$^{-2}$)',fontsize=20)

fig.legend(loc=9,ncol=3,fontsize=16,numpoints=1)

plt.savefig('MUST_profil_T_testcase.png',dpi = 300)

#
#--------FIG3: 2D birdviews
#
fig = plt.figure(num=3,figsize=(16,15),dpi=100, facecolor='w', edgecolor='w')
ax = fig.add_subplot(gs2D[0,0])
levels = np.linspace(0.0,15.,256)
CS_colors=plt.contourf(x, y, speed2D[6,:,:], levels,cmap=plt.cm.coolwarm, extend='max')
plt.contourf(x, y, lsfp[6,:,:], [0,1.], colors=['grey', 'grey'])
plt.ylabel(r'y(m)',fontsize=20)
plt.xlabel(r'x(m)',fontsize=20)
ax.set_aspect('equal')

fig.subplots_adjust(right=0.92)
cbar_ax = fig.add_axes([0.92, 0.05, 0.0225, 0.88])
cb=fig.colorbar(CS_colors, cax=cbar_ax)
cbar_ticks = np.linspace(0., 15., num=6, endpoint=True)
cb.set_ticks(cbar_ticks)
cb.ax.tick_params(labelsize=20)
cb.set_label(r' U (m.s$^{-1}$)',fontsize=20)
cb.update_ticks()


plt.savefig('MUST_2D_UMEAN.png',dpi = 300)
