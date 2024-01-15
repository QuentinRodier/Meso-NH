#!/usr/bin/env python3
"""

@author: Quentin Rodier
Creation : 07/01/2021

Last modifications
"""
import matplotlib as mpl
mpl.use('Agg')
from read_MNHfile import read_netcdf, read_withEPY
from Panel_Plot import PanelPlot
import cartopy.crs as ccrs
import os
import epygram
import numpy as np

epygram.init_env()

os.system('rm -f tempgraph*')
#
#  User's parameter / Namelist
#
LnameFiles = ['PREP_ERA5.nc','PREP_ERA5dg.nc']

Dvar_input = {
'f1':['ZS', 'SST', 'PABST','longitude','latitude'],
'f2':['THT950HPA','UT950HPA','VT950HPA',
      'THT700HPA','UT700HPA','VT700HPA',
      'THT500HPA','UT500HPA','VT500HPA',
      'THT300HPA','UT300HPA','VT300HPA']
}

LfilesEPY = ['ecmwf.EA.20180513.00']
Dvar_inputEPY = {'fGRIB': [[172,172,1,0,'landsea'], [129,129,1,0,'orog'],
[134,134,1,0,'surfpressure'],
[131,131,100,950,'u950'], [132,132,100,950,'v950'],[130,130,100,950,'t950'],
[131,131,100,700,'u700'], [132,132,100,700,'v700'],[130,130,100,700,'t700'],
[131,131,100,500,'u500'], [132,132,100,500,'v500'],[130,130,100,500,'t500'],
[131,131,100,300,'u300'], [132,132,100,300,'v300'],[130,130,100,300,'t300']]
#  GRIB2 list [discipline, parameterCategory, typeOfFirstFixedSurface, parameterNumber, level, casual name]
#  GRIB1 list [indicatorOfParameter, paramId, indicatorOfTypeOfLevel, level, casual name]
}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path="", removeHALO=True)
Dvar = read_withEPY(LfilesEPY, Dvar_inputEPY, Dvar)

MNH_T950HPA = Dvar['f2']['THT950HPA']*((95000./100000.)**(287./1004.))
MNH_T700HPA = Dvar['f2']['THT700HPA']*((70000./100000.)**(287./1004.))
MNH_T500HPA = Dvar['f2']['THT500HPA']*((50000./100000.)**(287./1004.))
MNH_T300HPA = Dvar['f2']['THT300HPA']*((30000./100000.)**(287./1004.))

Dvar['fGRIB']['orog'].data = Dvar['fGRIB']['orog'].data / 9.81

latcity=[45.16667, 43.70313]
loncity=[5.71667, 7.26608]
label=['Grenoble','Nice']

#  Extract a zoom to save computation time
for var in Dvar['fGRIB']:
    Dvar['fGRIB'][var] = Dvar['fGRIB'][var].extract_zoom({'lonmin':0,'lonmax':10,'latmin':42,'latmax':46})

################################################################
#########          PANEL 1  
###############################################################
Panel = PanelPlot(2,3, [27,14],'General variables', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=22)

#  Get Horizontal Geometry of the GRIB
(lonGRIB,latGRIB) = Dvar['fGRIB']['landsea'].geometry.get_lonlat_grid()


Lplot = [ Dvar['f1']['SST'], Dvar['fGRIB']['landsea'].data, Dvar['f1']['ZS'], Dvar['fGRIB']['orog'].data, Dvar['f1']['PABST'][0,:,:],Dvar['fGRIB']['surfpressure'].data]
lon = [Dvar['f1']['longitude'], lonGRIB]*3
lat = [Dvar['f1']['latitude'], latGRIB]*3
Ltitle = ['SST MesoNH','Land-Sea mask ERA5', 'Orography MNH','Orography ERA5','MNH Ps','ERA5 Ps']
Lcbarlabel = ['K','','m','m','hPa','hPa']
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude']*len(Lplot)
Lminval = [287, 0, 0, 0, 700, 700] 
Lmaxval = [295, 1, 4000, 4000,1020,1020]
Lstep = [0.2, 0.5, 50, 50, 1, 1]
Lstepticks = [1, 0.5, 500, 500, 50, 50]
Lcolormap = ['gist_rainbow_r','gist_rainbow_r','terrain','terrain','gist_rainbow_r','gist_rainbow_r']
Lprojection = [ccrs.PlateCarree()]*len(Lplot)
LaddWhite = [False]*len(Lplot)
Lfacconv = [1, 1, 1, 1, 1./100, 1./100.]
fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel,
                                LaddWhite_cm=LaddWhite, Lproj=Lprojection, ax=[], Lcarte=[5, 8.25, 43.2,45.5], Lfacconv=Lfacconv)

#Add departements + label cities point to each axes
for i in range(6):
  fig.axes[i*2].scatter(loncity,latcity, alpha=0.8, c='black', edgecolors='none')
  for lab, txt in enumerate(label):
    fig.axes[i*2].annotate(label[lab], (loncity[lab], latcity[lab]), color='black',size=10, weight="bold")

Panel.save_graph(1,fig)

################################################################
#########          PANEL 2 
###############################################################
Panel = PanelPlot(2,4, [30,14],'Temperature at isobaric surface', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=22)

Lplot = [ MNH_T950HPA, MNH_T700HPA, MNH_T500HPA, MNH_T300HPA, Dvar['fGRIB']['t950'].data, Dvar['fGRIB']['t700'].data, Dvar['fGRIB']['t500'].data, Dvar['fGRIB']['t300'].data]
Ltitle = ['MNH T 950hPa', 'MNH T700hPa', 'MNH T 500hPa', 'MNH T 300hPa', 'ERA5 T 950hPa', 'ERA5 T 700hPa', 'ERA5 T 500hPa', 'ERA5 T 300hPa']
lon = [Dvar['f1']['longitude'], Dvar['f1']['longitude'], Dvar['f1']['longitude'], Dvar['f1']['longitude'], lonGRIB, lonGRIB, lonGRIB, lonGRIB]
lat = [Dvar['f1']['latitude'], Dvar['f1']['latitude'], Dvar['f1']['latitude'], Dvar['f1']['latitude'], latGRIB, latGRIB, latGRIB, latGRIB]
Lcbarlabel = ['K']*len(Lplot)
Lxlab = ['']*len(Lplot)
Lylab = ['']*len(Lplot)
Lminval = [270, 265, 250, 220, 270, 265, 250, 220]
Lmaxval = [290, 275, 260, 230, 290, 275, 260, 230]
Lstep = [0.25]*len(Lplot)
Lstepticks = [2]*len(Lplot)
Lcolormap = ['gist_rainbow_r','seismic','seismic','seismic','gist_rainbow_r','seismic','seismic','seismic']
Lprojection = [ccrs.PlateCarree()]*len(Lplot)
LaddWhite = [False]*len(Lplot)
Lfacconv = [1]*len(Lplot)
fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel,
                                LaddWhite_cm=LaddWhite, Lproj=Lprojection, ax=[], Lcarte=[5, 8.25, 43.2,45.5], Lfacconv=Lfacconv)

Panel.save_graph(2,fig)

################################################################
#########          PANEL 3
###############################################################
Panel = PanelPlot(2,4, [30,14],'Zonal wind at isobaric surface', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=22)

Lplot = [ Dvar['f2']['UT950HPA'], Dvar['f2']['UT700HPA'], Dvar['f2']['UT500HPA'], Dvar['f2']['UT300HPA'], Dvar['fGRIB']['u950'].data, Dvar['fGRIB']['u700'].data, Dvar['fGRIB']['u500'].data, Dvar['fGRIB']['u300'].data]
Ltitle = ['MNH u 950hPa', 'MNH u 700hPa', 'MNH u 500hPa', 'MNH u 300hPa', 'ERA5 u 950hPa', 'ERA5 u 700hPa', 'ERA5 u 500hPa', 'ERA5 u 300hPa']
Lcbarlabel = ['m/s']*len(Lplot)
Lminval = [-10]*len(Lplot)
Lmaxval = [10]*len(Lplot)
Lstep = [0.25]*len(Lplot)
Lstepticks = [5]*len(Lplot)
Lcolormap = ['seismic']*len(Lplot)
fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel,
                                LaddWhite_cm=LaddWhite, Lproj=Lprojection, ax=[], Lcarte=[5, 8.25, 43.2,45.5], Lfacconv=Lfacconv)

Panel.save_graph(3,fig)

################################################################
#########          PANEL 4
###############################################################
Panel = PanelPlot(2,4, [30,14],'Meridionnal wind at isobaric surface', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=22)

Lplot = [ Dvar['f2']['VT950HPA'], Dvar['f2']['VT700HPA'], Dvar['f2']['VT500HPA'],Dvar['f2']['VT300HPA'],  Dvar['fGRIB']['v950'].data, Dvar['fGRIB']['v700'].data, Dvar['fGRIB']['v500'].data, Dvar['fGRIB']['v300'].data]
Ltitle = ['MNH v 950hPa', 'MNH v 700hPa', 'MNH v 500hPa', 'MNH v 300hPa', 'ERA5 v 950hPa', 'ERA5 v 700hPa', 'ERA5 v 500hPa', 'ERA5 v 300hPa']
Lcbarlabel = ['m/s']*len(Lplot)
Lminval = [-10, 0, 0, 0, -10, 0, 0, 0]
Lmaxval = [10, 20, 30, 30, 10, 20, 30, 30]
Lstep = [0.25]*len(Lplot)
Lstepticks = [5]*len(Lplot)
Lcolormap = ['seismic', 'gist_rainbow_r','seismic','seismic']*2
fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel,
                                LaddWhite_cm=LaddWhite, Lproj=Lprojection, ax=[], Lcarte=[5, 8.25, 43.2,45.5], Lfacconv=Lfacconv)

Panel.save_graph(4,fig)
