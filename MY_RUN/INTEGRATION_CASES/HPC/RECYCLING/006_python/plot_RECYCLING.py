#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon May 27 16:56:57 2019


@author: nagelt
"""

import matplotlib
from netCDF4 import Dataset
import numpy as np
import matplotlib.pyplot as plt
matplotlib.use('Agg')
plt.ioff()
import matplotlib.gridspec as gridspec


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
jphext=1
jpvext=1

#Function READ
def f_read(file,space_step,ls,ls2):
    ncfile1 = Dataset(file,'r')
    ut=ncfile1.variables['UT'][0,:,:,:]
    vt=ncfile1.variables['VT'][0,:,:,:]
    x=ncfile1.variables['XHAT'][:]
    y=ncfile1.variables['YHAT'][:]
    z=ncfile1.variables['ZHAT'][:]

    #Interpolation au point de masse
    lenx=len(x)
    x=0.5*(x[0:(lenx-1)]+x[1:lenx])
    ut=0.5*(ut[:,:,0:(lenx-1)]+ut[:,:,1:lenx])
    lenx=len(x)

    leny=len(y)
    y=0.5*(y[0:(leny-1)]+y[1:leny])
    vt=0.5*(vt[:,0:(leny-1),:]+vt[:,1:leny,:])
    leny=len(y)

    lenz=len(z)
    z=0.5*(z[0:(lenz-1)]+z[1:lenz])
    lenz=len(z)

    x=x[jphext:lenx]
    y=y[jphext:leny] 
    z=z[jpvext:lenz]
    ut=ut[jpvext:lenz,jphext:leny,jphext:lenx]
    vt=vt[jpvext:lenz,jphext:leny,jphext:lenx]  

#    Spectre
    umefft=ut[25,:,ls]
    ps = np.abs(np.fft.fft(umefft))**2
    freqs = np.fft.fftfreq(umefft.size, space_step)
    idx = np.argsort(freqs)
    
    umefft=ut[25,:,ls2]
    pps = np.abs(np.fft.fft(umefft))**2
    ffreqs = np.fft.fftfreq(umefft.size, space_step)
    iidx = np.argsort(ffreqs)
    
    return (x,y,z,ut,vt,ps,freqs,idx,pps,ffreqs,iidx)

###################################
# Lecture des données MESO-NH: LES
###################################
path = '../005_MNHD2/'
space_step = 24.
ls = 40
ls2 = 280

#--------RECY
file= path + 'MNHOU.2.SEGM.002.nc'
x2,y2,z2,ume2a,vme2a,ps2a,freqs2a,idx2a,pps2a,ffreqs2a,iidx2a = f_read(file,space_step,ls,ls2)
speed2a = np.sqrt(ume2a**2 + vme2a**2)

#--------NO RECY
path = '../005_MNHD2_NORECY/'
file= path + 'MNHOU.2.SEGM.002.nc'
x2d,y2d,z2d,ume2d,vme2d,ps2d,freqs2d,idx2d,pps2d,ffreqs2d,iidx2d = f_read(file,space_step,ls,ls2)
speed2d = np.sqrt(ume2d**2 + vme2d**2)

##########################
# Figures
##########################
gs = gridspec.GridSpec(2,2)
gs.update(wspace=0.35,hspace=0.075)

xpente=np.linspace(0.00001,1.0,200)
pente=0.1*xpente**(-5./3.)

i=np.size(y2)

#--------FIG1: /U/ + spectre comp avec et sans recyclage, z=1.5m
fig = plt.figure(num=2,figsize=(12,9),dpi=100, facecolor='w', edgecolor='w')
ax = fig.add_subplot(gs[0,1])
levels = np.linspace(0,20,256)
CS_colors=plt.contourf(x2d, y2d, speed2d[6,:,:], levels, cmap=plt.cm.coolwarm)
plt.vlines(x2[40], y2[0], y2[i-1],colors='g',linestyles='dashed')
plt.vlines(x2[280], y2[0], y2[i-1],colors='r',linestyles='dashed')
plt.ylabel(r'y(m)',fontsize=20)
#ax.set_xticks([39000,43000,47000])
ax.set_xticklabels([])
ax.set_aspect('equal')

ax = fig.add_subplot(gs[0,0])
ax.plot(freqs2d[idx2d], ps2d[idx2d], 'g',linewidth=2)
ax.plot(ffreqs2d[iidx2d], pps2d[iidx2d],'r', linewidth=2)
ax.plot(xpente,pente,'0.25',linewidth=2,label=r'-5/3')
plt.plot((0.0104,0.0104), (0.001,1000000),'--k',linewidth=1)
plt.plot((0.0208,0.0208), (0.001,1000000),'--k',linewidth=1)
ax.set_xscale('log')
ax.set_yscale('log')
plt.xlim(1.e-4,0.05)
plt.ylim(1.e-2,1.e5)
ax.set_xticklabels([])
plt.ylabel(r'$F_{uy}$ (m$^{2}$.s$^{-2}$)',fontsize=20)
ax.legend(loc='center left',fontsize=15,numpoints=1)
ax.text(0.00015,0.25 , r'No Recycling', style='italic',fontsize=14,
        bbox={'facecolor':'white', 'alpha':0.5, 'pad':10})
ax.text(0.02,125000 , r'2$\Delta$', style='italic',fontsize=14)
ax.text(0.01,125000 , r'4$\Delta$', style='italic',fontsize=14)

ax = fig.add_subplot(gs[1,1])
CS_colors=plt.contourf(x2, y2, speed2a[6,:,:], levels, cmap=plt.cm.coolwarm)
plt.vlines(x2[40], y2[0], y2[i-1],colors='g',linestyles='dashed')
plt.vlines(x2[280], y2[0], y2[i-1],colors='r',linestyles='dashed')
#ax.set_xticks([39000,43000,47000])
#ax.set_xticklabels(['39000','43000','47000'])
plt.ylabel(r'y(m)',fontsize=20)
plt.xlabel(r'x(m)',fontsize=20)
ax.set_aspect('equal')

ax = fig.add_subplot(gs[1,0])
ax.plot(freqs2a[idx2a], ps2a[idx2a], 'g',linewidth=2)
ax.plot(ffreqs2a[iidx2a], pps2a[iidx2a],'r', linewidth=2)
ax.plot(xpente,pente,'0.25',linewidth=2,label=r'$k_y^{-5/3}$')
plt.plot((0.0104,0.0104), (0.001,1000000),'--k',linewidth=1)
plt.plot((0.0208,0.0208), (0.001,1000000),'--k',linewidth=1)
ax.set_xscale('log')
ax.set_yscale('log')
plt.xlim(1.e-4,0.05)
plt.ylim(1.e-2,1.e5)
plt.ylabel(r'$F_{uy}$ (m$^{2}$.s$^{-2}$)',fontsize=20)
plt.xlabel(r'Wave Number (m$^{-1}$)',fontsize=20)
ax.text(0.00015,0.25 , r'Recycling', style='italic',fontsize=14,
        bbox={'facecolor':'white', 'alpha':0.5, 'pad':10})

fig.subplots_adjust(right=0.8)
cbar_ax = fig.add_axes([0.81, 0.115, 0.02, 0.76])
cb=plt.colorbar(CS_colors, cax=cbar_ax)
cbar_ticks = np.linspace(0., 20., num=6, endpoint=True)
#cb.formatter.set_powerlimits((0, 0))
cb.set_ticks(cbar_ticks)
cb.set_label(r'$ \mid$U$\mid (m.s^{-1})$',fontsize=20)

plt.savefig('recycl_spectres.png',dpi = 200)
