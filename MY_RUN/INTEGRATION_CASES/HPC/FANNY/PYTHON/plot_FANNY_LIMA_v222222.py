#!/usr/bin/env python3
"""
@author: Quentin Rodier
Creation : 07/01/2021

Last modifications
"""

import matplotlib as mpl
mpl.use('Agg')
from read_MNHfile import read_netcdf
from Panel_Plot import PanelPlot
import cartopy.crs as ccrs
import numpy as np
import os
import cartopy.io.shapereader as shpreader

os.system('rm -f tempgraph*')
#
#  User's parameter / Namelist
#
path="../LIMA_v222222/"
LnameFiles = ['FANNY.1.WENO5.025.nc']

Dvar_input = {
'f1':['ZS', 'UT','VT', 'THT', 'ACPRT', 'level', 'longitude','latitude','time','TKET','WT']}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path, removeHALO=True)

# Departements francais
departements_shp='departements-20180101.shp'
adm1_shapes = list(shpreader.Reader(departements_shp).geometries())

################################################################
#########          PANEL 1  # Horizontal cross-section
###############################################################
Panel1 = PanelPlot(2,2, [20,20],'')

Dvar['f1']['WIND'] = np.sqrt(Dvar['f1']['UT']**2 + Dvar['f1']['VT']**2)
Lplot = [ Dvar['f1']['ZS'][:,:], Dvar['f1']['WIND'][0,:,:], Dvar['f1']['ACPRT'], Dvar['f1']['THT'][1,:,:]]

LaxeX = [Dvar['f1']['longitude']]*len(Lplot)
LaxeY = [Dvar['f1']['latitude']]*len(Lplot)
Ltitle = ['Orography', 'Wind speed 1st level','Accumulated RT', 'Potential temperature 1st level']
Lcbarlabel = ['m', 'm/s','mm','K']
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude']*len(Lplot)
Lminval = [0, 0, 0,  288]
Lmaxval = [3800, 16, 150, 308]
Lstep = [200, 1, 2.5, 1]
Lstepticks = [200, 1, 25, 1]
Lfacconv = [1]*len(Lplot)
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Ltime = [Dvar['f1']['time']]*len(Lplot)
Lpltype = ['cf']*len(Lplot)
LaddWhite_cm = [True, True, False, False]
Lprojection = [ccrs.PlateCarree()]*len(Lplot)

fig = Panel1.psectionH(lon=LaxeX, lat=LaxeY, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                colorbar=True, Ltime=Ltime, LaddWhite_cm=LaddWhite_cm, Lproj=Lprojection)
#Add departements to each axes
for i in range(4):
  fig.axes[i*2].add_geometries(adm1_shapes, ccrs.PlateCarree(),edgecolor='black', facecolor='gray', alpha=0.5)


Panel1.save_graph(1, fig)

################################################################
#########          PANEL 1  # Horizontal cross-section
###############################################################
Panel1 = PanelPlot(3,2, [20,20],'Horizontal cross-section at 2500, 5000 and 8000m', titlepad=15,)

Lplot = [ Dvar['f1']['WT'][15,:,:], Dvar['f1']['TKET'][15,:,:],
          Dvar['f1']['WT'][21,:,:], Dvar['f1']['TKET'][21,:,:],
          Dvar['f1']['WT'][26,:,:], Dvar['f1']['TKET'][26,:,:]]

LaxeX = [Dvar['f1']['longitude']]*len(Lplot)
LaxeY = [Dvar['f1']['latitude']]*len(Lplot)
Ltitle = ['w at 2500m', 'TKE at 2500m','w at 5000m', 'TKE at 5000m','w at 8000m', 'TKE at 8000m']
Lcbarlabel = ['m/s', 'm2/s2','m/s','m2/s2','m/s','m2/s2']
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude']*len(Lplot)
Lminval = [-6.125, 0, -6.125,  0, -6.125, 0.]
Lmaxval = [6.125, 4., 6.125, 4., 6.125, 4.]
Lstep = [0.25, 0.05, 0.25, 0.05, 0.25, 0.05]
Lstepticks = [2, 0.5, 2, 0.5, 2, 0.5]
Lfacconv = [1]*len(Lplot)
Lcolormap = ['seismic','gist_rainbow_r','seismic','gist_rainbow_r', 'seismic','gist_rainbow_r']
Ltime = [Dvar['f1']['time']]*len(Lplot)
Lpltype = ['cf']*len(Lplot)
LaddWhite_cm = [False, True, False, True, False, True]
Lprojection = [ccrs.PlateCarree()]*len(Lplot)

fig = Panel1.psectionH(lon=LaxeX, lat=LaxeY, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv,
                                colorbar=True, Ltime=Ltime, LaddWhite_cm=LaddWhite_cm, Lproj=Lprojection, Lcarte=[4, 7.5, 43, 45.5])
#Add departements to each axes
for i in range(6):
  fig.axes[i*2].add_geometries(adm1_shapes, ccrs.PlateCarree(),edgecolor='black', facecolor='gray', alpha=0.5)


Panel1.save_graph(2, fig)
