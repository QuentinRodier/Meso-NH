#!/usr/bin/env python3
"""
@author: Quentin Rodier
Creation : 07/01/2021

Last modifications
"""
import matplotlib as mpl
mpl.use('Agg')
import cartopy.crs as ccrs
from read_MNHfile import read_netcdf
from Panel_Plot import PanelPlot
import os
import cartopy.feature as cfeature

os.system('rm -f tempgraph*')
#
#  User's parameter / Namelist
#
#
path=""

LnameFiles = ['K_MAP.3.WENO5.009diag.nc'] + ['K_MAP.3.WENO5.01'+str(i)+'diag.nc' for i in range(4)]

Dvar_input = {'f'+str(i):['RARE', 'level', 'latitude', 'longitude', 'time'] for i in range(1,6)}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path, removeHALO=True)

################################################################
#########          PANEL 1  
###############################################################
Panel1 = PanelPlot(3,2, [15,20],'Radar reflectivity RARE', minmaxpad=1.05)

Lplot = [Dvar['f'+str(i)]['RARE'] for i in range(1,6)]
lon = [Dvar['f'+str(i)]['longitude'] for i in range(1,6)]
lat = [Dvar['f'+str(i)]['latitude'] for i in range(1,6)]
Ltitle = ['']*len(Lplot)
Lcbarlabel = ['dbz']*len(Lplot)
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude']*len(Lplot)
Lminval = [4]*len(Lplot)
Lmaxval = [55]*len(Lplot)
Lstep = [1]*len(Lplot)
Lstepticks = [5]*len(Lplot)
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Llvl = [15]*len(Lplot)
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,6)]
Lproj = [ccrs.PlateCarree()]*len(Lplot)
fig1 = Panel1.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lcarte=[6.5, 11, 44.5,47.5], Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lproj=Lproj)

fig1.axes[0].add_feature(cfeature.LAKES, alpha=0)
Panel1.save_graph(1,fig1)

