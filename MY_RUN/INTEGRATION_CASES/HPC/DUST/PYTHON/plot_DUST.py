#!/usr/bin/env python3
"""
@author: Quentin Rodier
Creation : 07/01/2021

Last modifications
"""
import cartopy.crs as ccrs
from read_MNHfile import read_netcdf
from Panel_Plot import PanelPlot
import os

os.system('rm -f tempgraph*')
#
#  User's parameter / Namelist
#
#
path=""

LnameFiles = ['DUST7.1.W5_S3.009.nc']

Dvar_input = {'f1':['F_DST001P1','F_DST002P1','F_DST003P1', 'latitude', 'longitude', 'time']}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path, removeHALO=True)

################################################################
#########          PANEL 1  
###############################################################
Panel1 = PanelPlot(2,2, [20,20],'', minmaxpad=1.05)

Lplot = [Dvar['f1']['F_DST001P1'], Dvar['f1']['F_DST002P1'], Dvar['f1']['F_DST003P1']]
lon = [Dvar['f1']['longitude']]*len(Lplot)
lat = [Dvar['f1']['latitude']]*len(Lplot)
Ltitle = ['F_DST001P1', 'F_DST002P1', 'F_DST003P1']
Lcbarlabel = ['kg/m²/s']*len(Lplot)
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude']*len(Lplot)
Lminval = [-16, -1, -1.5]
Lmaxval = [0]*len(Lplot)
Lstep = [0.1, 0.01, 0.01]
Lstepticks = [1, 0.1, 0.1]
Lfacconv = [10**15]*len(Lplot)
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Llvl = [0]*len(Lplot)
Ltime = [Dvar['f1']['time']]*len(Lplot)
Lproj = [ccrs.PlateCarree()]*len(Lplot)
fig1 = Panel1.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lcarte=[-20,-12,10.3,18], Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, Ltime=Ltime, Lproj=Lproj)

Panel1.save_graph(1,fig1)
