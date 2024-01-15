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
path="../LIMA_v22222/"

LnameFiles = ['FOG3D.1.CE_S2.004.nc', 'FOG3D.1.CE_S2.007.nc']
Dvar_input = {
'f1':['ZS', 'UT','VT','RCT', 'CLDFR','level','ZTOP', 'longitude','latitude','time'],
'f2':['ZS', 'UT','VT','RCT', 'CLDFR','level','ZTOP', 'longitude','latitude','time']
}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path, removeHALO=True)


# Departements francais
departements_shp='departements-20180101.shp'
adm1_shapes = list(shpreader.Reader(departements_shp).geometries())

################################################################
#########          PANEL 1  # Horizontal cross-section file 1
###############################################################
Panel1 = PanelPlot(2,2, [20,10],'FOG_3D 3h')

Dvar['f1']['WIND'] = np.sqrt(Dvar['f1']['UT']**2 + Dvar['f1']['VT']**2)

Lplot = [ Dvar['f1']['ZS'][:,:],  Dvar['f1']['RCT'][6,:,:], Dvar['f1']['WIND'][6,:,:], Dvar['f1']['CLDFR'][6,:,:]]
LaxeX = [Dvar['f1']['longitude']]*len(Lplot)
LaxeY = [Dvar['f1']['latitude']]*len(Lplot)
Ltitle = ['Orography', 'Cloud mixing ratio at K=8', 'Wind speed ','Cloud fraction']
Lcbarlabel = ['m', 'g/kg','m/s','']
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude']*len(Lplot)
Lminval = [0, 0, 0., 0]
Lmaxval = [250, 1, 8, 1]
Lstep = [10, 0.025, 0.5, 0.1]
Lstepticks = [10, 0.1, 0.5, 0.1]
Lfacconv = [1, 1000, 1, 1]
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Ltime = [Dvar['f1']['time']]*len(Lplot)
Lpltype = ['cf']*len(Lplot)
LaddWhite_cm = [True, True, True, True]
Lprojection = [ccrs.PlateCarree()]*len(Lplot)

fig1 = Panel1.psectionH(lon=LaxeX, lat=LaxeY, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                colorbar=True, Ltime=Ltime, LaddWhite_cm=LaddWhite_cm, Lproj=Lprojection)

latcar=[48.81,48.6,48.72,49.08,48.96,49.00,49.25,49.43]
loncar=[2.33,2.66,2.38,2.01,2.42,2.53,2.52,2.12]
label=['MONTS','MEL','ORL','PONT','BOUR','ROIS','CREI','BEAU']

#Add departements + label cities point to each axes
for i in range(4):
  fig1.axes[i*2].add_geometries(adm1_shapes, ccrs.PlateCarree(),edgecolor='black', facecolor='white', alpha=0.)
  fig1.axes[i*2].scatter(loncar,latcar, alpha=0.8, c='black', edgecolors='none')
  for lab, txt in enumerate(label):
    fig1.axes[i*2].annotate(label[lab], (loncar[lab], latcar[lab]), color='white',size=10, weight="bold")


# Wind vectors
Lplot1 = [ Dvar['f1']['UT'], ]
Lplot2 = [ Dvar['f1']['VT'], ]
Ltitle = ['Wind vectors at K=8']
Lxlab = ['longitude']*len(Lplot1)
Lylab = ['latitude']*len(Lplot1)
Llegendval = [7]
Lcbarlabel = ['m/s']*len(Lplot1)
Larrowstep = [6]*len(Lplot1)
Lwidth = [0.002]*len(Lplot1)
Lcolor = ['black']*len(Lplot1)
Llvl = [6]
LaxeX = [Dvar['f1']['longitude']]*len(Lplot1)
LaxeY = [Dvar['f1']['latitude']]*len(Lplot1)
Lscale = [200]*len(Lplot1)
fig2 = Panel1.pvector(Lxx=LaxeX, Lyy=LaxeY, Lvar1=Lplot1, Lvar2=Lplot2, Llevel=Llvl, 
                      Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lwidth=Lwidth, Larrowstep=Larrowstep, 
                      Lcolor=Lcolor, Llegendval=Llegendval, Llegendlabel=Lcbarlabel, Lid_overlap=[4], ax=fig1.axes, Lscale=Lscale)
Panel1.save_graph(1, fig2)

################################################################
#########          PANEL 2  # Horizontal cross-section file 2
###############################################################
Panel2 = PanelPlot(2,2, [20,10],'FOG_3D 6h')

Dvar['f2']['WIND'] = np.sqrt(Dvar['f2']['UT']**2 + Dvar['f2']['VT']**2)

Lplot = [ Dvar['f2']['ZS'][:,:],  Dvar['f2']['RCT'][6,:,:], Dvar['f2']['WIND'][6,:,:], Dvar['f2']['CLDFR'][6,:,:]]
LaxeX = [Dvar['f2']['longitude']]*len(Lplot)
LaxeY = [Dvar['f2']['latitude']]*len(Lplot)
Ltitle = ['Orography', 'Cloud mixing ratio at K=8', 'Wind speed ','Cloud fraction']
Lcbarlabel = ['m', 'g/kg','m/s','']
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude']*len(Lplot)
Lminval = [0, 0, 0., 0]
Lmaxval = [250, 1, 8, 1]
Lstep = [10, 0.025, 0.5, 0.1]
Lstepticks = [10, 0.1, 0.5, 0.1]
Lfacconv = [1, 1000, 1, 1]
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Ltime = [Dvar['f2']['time']]*len(Lplot)
Lpltype = ['cf']*len(Lplot)
LaddWhite_cm = [True, True, True, True]
Lprojection = [ccrs.PlateCarree()]*len(Lplot)

fig1 = Panel2.psectionH(lon=LaxeX, lat=LaxeY, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv,
                                colorbar=True, Ltime=Ltime, LaddWhite_cm=LaddWhite_cm, Lproj=Lprojection)

#Add departements + label cities point to each axes
for i in range(4):
  fig1.axes[i*2].add_geometries(adm1_shapes, ccrs.PlateCarree(),edgecolor='black', facecolor='white', alpha=0.)
  fig1.axes[i*2].scatter(loncar,latcar, alpha=0.8, c='black', edgecolors='none')
  for lab, txt in enumerate(label):
    fig1.axes[i*2].annotate(label[lab], (loncar[lab], latcar[lab]), color='white',size=10, weight="bold")


# Wind vectors
Lplot1 = [ Dvar['f2']['UT'], ]
Lplot2 = [ Dvar['f2']['VT'], ]
Ltitle = ['Wind vectors at K=8']
Lxlab = ['longitude']*len(Lplot1)
Lylab = ['latitude']*len(Lplot1)
Llegendval = [7]
Lcbarlabel = ['m/s']*len(Lplot1)
Larrowstep = [6]*len(Lplot1)
Lwidth = [0.002]*len(Lplot1)
Lcolor = ['black']*len(Lplot1)
Llvl = [6]
LaxeX = [Dvar['f2']['longitude']]*len(Lplot1)
LaxeY = [Dvar['f2']['latitude']]*len(Lplot1)
Lscale = [200]*len(Lplot1)
fig2 = Panel2.pvector(Lxx=LaxeX, Lyy=LaxeY, Lvar1=Lplot1, Lvar2=Lplot2, Llevel=Llvl,
                      Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lwidth=Lwidth, Larrowstep=Larrowstep,
                      Lcolor=Lcolor, Llegendval=Llegendval, Llegendlabel=Lcbarlabel, Lid_overlap=[4], ax=fig1.axes, Lscale=Lscale)
Panel2.save_graph(2, fig2)

