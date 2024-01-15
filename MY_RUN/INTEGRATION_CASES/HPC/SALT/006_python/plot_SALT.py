#!/usr/bin/env python3
"""
@author: Quentin Rodier
Creation : 07/01/2021

Last modifications
"""
import matplotlib as mpl
mpl.use('Agg')
import cartopy.crs as ccrs
try:
	import MNHPy
	from MNHPy.read_MNHfile import read_netcdf
	from MNHPy.Panel_Plot import PanelPlot
except:
	from read_MNHfile import read_netcdf
	from Panel_Plot import PanelPlot
import os
import numpy as np
os.system('rm -f tempgraph*')

LnameFiles=['SALHS.2.18D03.006.nc','SALHS.2.18D03.006dg.nc']

Dvar_input = {
'f1': ['THT','UT','VT','longitude','latitude'],
'f2': ['SLTN0A1','SLTN0A2','SLTN0A3','SLTN0A4','SLTN0A5','SLTM1','SLTM2','SLTM3','SLTM4','SLTM5','longitude','latitude','SLTAOD2D'] 
}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path="", removeHALO=True)

Dvar['f1']['WIND'] = np.sqrt(Dvar['f1']['UT']**2 + Dvar['f1']['VT']**2)

Panel1 = PanelPlot(1,3, [20,5],'SALT', minmaxpad=1.05, titlepad=30)

Lplot = [ Dvar['f1']['THT'][0,:,:],Dvar['f1']['WIND'][0,:,:],Dvar['f2']['SLTAOD2D']]
lon = [Dvar['f1']['longitude']]*len(Lplot)
lat = [Dvar['f1']['latitude']]*len(Lplot)
Ltitle = ['THT K=2','Wind speed K=2','Salt Aerosol Optical Depth']
Lcbarlabel = ['K','m/s','-']
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude','','']
Lminval = [298, 0, 0]
Lmaxval = [312,32,0.4]
Lstep = [1, 2,0.05]
Lstepticks = [4, 8,0.05]
Lfacconv = [1.0, 1.0,1.0]
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Lprojection = [ccrs.PlateCarree()]*len(Lplot)
fig1 = Panel1.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lcarte=[], Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lproj=Lprojection, Lfacconv=Lfacconv)
Panel1.save_graph(1,fig1)


Lcbarlabel = ['m-3','ugm-3']

Panel1 = PanelPlot(1,2, [10,5],'', minmaxpad=1.05)
Lplot = [ Dvar['f2']['SLTN0A1'],Dvar['f2']['SLTM1']]
lon = [Dvar['f1']['longitude']]*len(Lplot)
lat = [Dvar['f1']['latitude']]*len(Lplot)
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude','']
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Lprojection = [ccrs.PlateCarree()]*len(Lplot)

Ltitle = ['Salt Number mode 0','Salt Concentration mode 0']
Lminval = [0, 0]
Lmaxval = [4E7,5E-4]
Lstep = [0.5E7,0.2E-4]
Lstepticks = [1E7, 1E-4]
Lfacconv = [1.0, 1.0]
Llvl = [0, 0]
fig1 = Panel1.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lcarte=[], Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lproj=Lprojection, Lfacconv=Lfacconv)
Panel1.save_graph(2,fig1)

Panel1 = PanelPlot(1,2, [10,5],'', minmaxpad=1.05)
Lplot = [ Dvar['f2']['SLTN0A2'],Dvar['f2']['SLTM2']]
Ltitle = ['Salt Number mode 1','Salt Concentration mode 1']
Lminval = [0, 0]
Lmaxval = [5E7,1.2E-2]
Lstep = [0.5E7,1.5E-3]
Lstepticks = [1E7, 3E-3]
fig1 = Panel1.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lcarte=[], Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lproj=Lprojection, Lfacconv=Lfacconv)
Panel1.save_graph(3,fig1)


Panel1 = PanelPlot(1,2, [10,5],'', minmaxpad=1.05)
Lplot = [ Dvar['f2']['SLTN0A3'],Dvar['f2']['SLTM3']]
Ltitle = ['Salt Number mode 2','Salt Concentration mode 2']
Lminval = [0, 0]
Lmaxval = [7E7,1E-1]
Lstep = [0.5E7,1E-2]
Lstepticks = [1E7, 2E-2]
fig1 = Panel1.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lcarte=[], Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lproj=Lprojection, Lfacconv=Lfacconv)
Panel1.save_graph(4,fig1)


Panel1 = PanelPlot(1,2, [10,5],'', minmaxpad=1.05)
Lplot = [ Dvar['f2']['SLTN0A4'],Dvar['f2']['SLTM4']]
Ltitle = ['Salt Number mode 3','Salt Concentration mode 3']
Lminval = [0, 0]
Lmaxval = [5E7,2]
Lstep = [0.5E7,0.2]
Lstepticks = [1E7, 0.5]
fig1 = Panel1.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lcarte=[], Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lproj=Lprojection, Lfacconv=Lfacconv)
Panel1.save_graph(5,fig1)


Panel1 = PanelPlot(1,2, [10,5],'', minmaxpad=1.05)
Lplot = [ Dvar['f2']['SLTN0A5'],Dvar['f2']['SLTM5']]
Ltitle = ['Salt Number mode 4','Salt Concentration mode 4']
Lminval = [0, 0]
Lmaxval = [3E7,120]
Lstep = [0.25E7,15]
Lstepticks = [1E7, 30]
fig1 = Panel1.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lcarte=[], Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lproj=Lprojection, Lfacconv=Lfacconv)
Panel1.save_graph(6,fig1)

