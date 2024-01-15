#!/usr/bin/env python3
"""
@author: Quentin Rodier
Creation : 07/01/2021

Last modifications
"""
import cartopy.crs as ccrs
import cartopy.io.shapereader as shpreader
import matplotlib as mpl
mpl.use('Agg')
from read_MNHfile import read_netcdf
from Panel_Plot import PanelPlot
from misc_functions import mean_operator
from misc_functions import oblique_proj
from misc_functions import comp_altitude2DVar
import numpy as np
import os

os.system('rm -f tempgraph*')
#
#  User's parameter / Namelist
#
#
path="../RUN/"

LnameFiles = ['XYNTH.1.CEN4T.010.nc']

Dvar_input = {'f1':['ZS', 'ZTOP', 'TKET', 'THT', 'LSUM', 'LSVM', 'LSWM', 'UT', 'VT', 'WT', 'ni', 'nj', 'ni_u', 'nj_u', 'level', 'level_w', 'latitude', 'longitude', 'time']}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path, removeHALO=True)

################################################################
#########          PANEL 1
###############################################################
Panel1 = PanelPlot(6,2, [40,120],'', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30)

tomass = mean_operator()
Dvar['f1']['UM'] = tomass.MXM(Dvar['f1']['UT'])
Dvar['f1']['VM'] = tomass.MYM(Dvar['f1']['VT'])
Dvar['f1']['WM'] = tomass.MZM(Dvar['f1']['WT'])

Dvar['f1']['LSUMVM'] = np.sqrt(Dvar['f1']['LSUM']**2 + Dvar['f1']['LSVM']**2)
Dvar['f1']['UTVT'] = np.sqrt(Dvar['f1']['UT']**2 + Dvar['f1']['VT']**2)

Lplot = [Dvar['f1']['LSUM'], Dvar['f1']['LSVM'], Dvar['f1']['LSWM'],
         Dvar['f1']['LSUM'], Dvar['f1']['LSVM'], Dvar['f1']['LSWM'],
         Dvar['f1']['WT'], Dvar['f1']['LSUMVM'], Dvar['f1']['UTVT'],
         Dvar['f1']['WT'][:,29:96,56:130], Dvar['f1']['LSUMVM'][:,29:96,56:130], Dvar['f1']['UTVT'][:,29:96,56:130]]
lon = [Dvar['f1']['longitude']]*9 + [Dvar['f1']['longitude'][29:96,56:130]]*3
lat = [Dvar['f1']['latitude']]*9 + [Dvar['f1']['latitude'][29:96,56:130]]*3
Ltitle = ['Large Scale U component at K=2', 'Large Scale V component at K=2', 'Large Scale Vertical Wind at K=2', 'Large Scale U component at K=30',
          'Large Scale V component at K=30', 'Large Scale Vertical Wind at K=30', 'Vertical Velocity at K=30', 'Large Scale U and V components at K=2',
          'U and V components of Wind at K=2', 'Vertical Velocity at K=30', 'Large Scale U and V components at K=2', 'U and V components of Wind at K=2']
Lcbarlabel = ['m/s']*7 + ['km/h']*2 + ['m/s'] + ['km/h']*2
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude']*len(Lplot)
Lminval = [-23, -9, -2.1, 16, 4, -3.4, -12, 10, 10, -12, 10, 10]
Lmaxval = [15, 29, 0.7, 46, 29, 3.3, 12, 120, 120, 12, 200, 200]
Lstep = [0.2, 0.2, 0.05, 0.2, 0.2, 0.2, 0.1, 1, 1, 0.1, 1, 1]
Lstepticks = [2, 2, 0.5, 2, 2, 2, 1, 10, 10, 1, 10, 10]
Lfacconv = [1]*7 + [3.6]*2 + [1] + [3.6]*2
Lcolormap = ['gist_rainbow_r']*6 + ['seismic'] + ['gist_rainbow_r']*2 + ['seismic'] + ['gist_rainbow_r']*2
Llvl = [0, 0, 0, 30, 30, 30, 30, 0, 0, 30, 0, 0]
Ltime = [Dvar['f1']['time']]*len(Lplot)
Lproj = [ccrs.PlateCarree()]*len(Lplot)
fig1 = Panel1.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lcarte=[], Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lfacconv=Lfacconv, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lproj=Lproj)
# Departements francais
departements_shp='departements-20180101.shp'
adm1_shapes = list(shpreader.Reader(departements_shp).geometries())
for i in range(len(Lplot)):
  fig1.axes[i*2].add_geometries(adm1_shapes, ccrs.PlateCarree(),edgecolor='black', facecolor='white', alpha=0.2)

fig1.tight_layout()

# Wind vectors
Lplot1 = [Dvar['f1']['LSUM'], Dvar['f1']['UT'], Dvar['f1']['LSUM'][:,29:96,56:130], Dvar['f1']['UT'][:,29:96,56:130]]
Lplot2 = [Dvar['f1']['LSVM'], Dvar['f1']['VT'], Dvar['f1']['LSVM'][:,29:96,56:130], Dvar['f1']['VT'][:,29:96,56:130]]
Ltitle = ['Large Scale Wind', 'Wind']*2
Llegendval = [10]*len(Lplot1)
Llegendlabel = ['km/h']*len(Lplot1)
Larrowstep = [6]*len(Lplot1)
Lwidth = [0.002]*len(Lplot1)
Lcolor = ['black']*len(Lplot1)
Llvl = [0]*len(Lplot1)
lon = [Dvar['f1']['longitude']]*2 + [Dvar['f1']['longitude'][29:96,56:130]]*2
lat = [Dvar['f1']['latitude']]*2 + [Dvar['f1']['latitude'][29:96,56:130]]*2
Lscale = [600]*len(Lplot1)
Lfacconv = [3.6]*len(Lplot1)
fig2 = Panel1.pvector(Lxx=lon, Lyy=lat, Lvar1=Lplot1, Lvar2=Lplot2, Llevel=Llvl, Ltitle=Ltitle,
                      Lwidth=Lwidth, Larrowstep=Larrowstep, Lcolor=Lcolor, Llegendval=Llegendval,
                      Llegendlabel=Llegendlabel, Lid_overlap=[14,16,20,22], ax=fig1.axes, Lscale=Lscale, Lfacconv=Lfacconv, Lproj=Lproj)

Panel1.save_graph(1,fig2)

################################################################
#########          PANEL 2
###############################################################
Panel2 = PanelPlot(3,2, [40,60],'Vertical section at I=83 and 15<=J<=84', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50)

Lplot = [Dvar['f1']['ZS']]
lon = [Dvar['f1']['ni']]
lat = [Dvar['f1']['nj']]
Ltitle = ['Orography']
Lcbarlabel = ['m']
Lxlab = ['x']
Lylab = ['y']
Lminval = [0]
Lmaxval = [2800]
Lstep = [50]
Lstepticks = [500]
Lcolormap = ['gist_rainbow_r']
Ltime = [Dvar['f1']['time']]
LaddWhite_cm = [True]
fig3 = Panel2.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lcarte=[], Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lproj=[], LaddWhite_cm = LaddWhite_cm)

i_beg, j_beg = (82,14)
i_end, j_end = (82,84)

Panel2.addLine(fig3.axes[0], [Dvar['f1']['ni'][i_beg], Dvar['f1']['nj'][j_beg]], [Dvar['f1']['ni'][i_end], Dvar['f1']['nj'][j_end]], 'black', 10)

Dvar['f1']['altitude'], Dvar['f1']['ni_3D'], Dvar['f1']['nj_3D'] = comp_altitude2DVar(Dvar['f1']['THT'], Dvar['f1']['ZS'], Dvar['f1']['ZTOP'], Dvar['f1']['level'], Dvar['f1']['latitude'], Dvar['f1']['longitude'])
Dvar['f1']['altitude_w'], Dvar['f1']['ni_3D'], Dvar['f1']['nj_3D'] = comp_altitude2DVar(Dvar['f1']['WT'], Dvar['f1']['ZS'], Dvar['f1']['ZTOP'], Dvar['f1']['level_w'], Dvar['f1']['latitude'], Dvar['f1']['longitude'])

Lplot = [Dvar['f1']['WT'][:,j_beg:j_end,i_beg], Dvar['f1']['TKET'][:,j_beg:j_end,i_beg], Dvar['f1']['UTVT'][:,j_beg:j_end,i_beg], Dvar['f1']['THT'][:,j_beg:j_end,i_beg]-273]
LaxeX = [Dvar['f1']['nj_3D'][:,j_beg:j_end,i_beg]]*len(Lplot)
LaxeZ = [Dvar['f1']['altitude_w'][:,j_beg:j_end,i_beg]] + [Dvar['f1']['altitude'][:,j_beg:j_end,i_beg]]*3
Ltitle = ['Vertical Velocity', 'Turbulent Kinetic Energy', 'U and V Wind Components', 'Potential Temperature Anomaly']
Lcbarlabel = ['m/s','m²/s²','km/h','K']
Lxlab = ['latitude']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0, 8000)]*len(Lplot)
Lminval = [-15, 2.5, 10, 15]
Lmaxval = [15, 52.5, 230, 60]
Lstep = [0.1, 0.25, 0.1, 0.5]
Lstepticks = [1, 1, 10, 5]
Lfacconv = [1, 1, 3.6, 1]
Lcolormap = ['seismic','gist_rainbow_r','gist_rainbow_r','gist_rainbow_r','gist_rainbow_r']
Ltime = [Dvar['f1']['time']]*len(Lplot)
Lpltype = ['cf']*len(Lplot)
LaddWhite = [True]*len(Lplot)
orog = [Dvar['f1']['ZS'][j_beg:j_end,i_beg][i] for i in range(j_end-j_beg)]
fig4 = Panel2.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Lylim=Lylim, Ltitle=Ltitle,
                        Lminval=Lminval, Lmaxval=Lmaxval, Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap,
                        Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, Ltime=Ltime, Lpltype=Lpltype, LaddWhite_cm=LaddWhite, orog=orog, ax=fig3.axes)

Panel2.save_graph(2,fig4)

################################################################
#########          PANEL 3
###############################################################
Panel3 = PanelPlot(3,2, [40,60],'Vertical section at 75<=I<=95 and 21<=J<=95', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50)

Lplot = [Dvar['f1']['ZS']]
lon = [Dvar['f1']['ni']]
lat = [Dvar['f1']['nj']]
Ltitle = ['Orography']
Lcbarlabel = ['m']
Lxlab = ['x']
Lylab = ['y']
Lminval = [0]
Lmaxval = [2800]
Lstep = [50]
Lstepticks = [200]
Lcolormap = ['gist_rainbow_r']
Ltime = [Dvar['f1']['time']]
LaddWhite = [True]
fig5 = Panel3.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lcarte=[], Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lproj=[], LaddWhite_cm = LaddWhite)

i_beg, j_beg = (74,20)
i_end, j_end = (95,95)
Panel3.addLine(fig5.axes[0], [Dvar['f1']['ni'][i_beg], Dvar['f1']['nj'][j_beg]], [Dvar['f1']['ni'][i_end], Dvar['f1']['nj'][j_end]], 'black', 10)

angle_sec, WT_sec, axe_m = oblique_proj(Dvar['f1']['WT'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec, TKET_sec, axe_m = oblique_proj(Dvar['f1']['TKET'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec, UTVT_sec, axe_m = oblique_proj(Dvar['f1']['UTVT'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec, THT_sec, axe_m = oblique_proj(Dvar['f1']['THT'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec, nj_3D_sec, axe_m = oblique_proj(Dvar['f1']['nj_3D'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec, altitude_sec, axe_m = oblique_proj(Dvar['f1']['altitude'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec, altitude_w_sec, axe_m = oblique_proj(Dvar['f1']['altitude_w'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec, orog_sec, axe_m = oblique_proj(Dvar['f1']['ZS'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)

Lplot = [WT_sec, TKET_sec, UTVT_sec, THT_sec-273]
LaxeX = [nj_3D_sec]*len(Lplot)
LaxeZ = [altitude_w_sec] + [altitude_sec]*3
Ltitle = ['Vertical Velocity', 'Turbulent Kinetic Energy', 'U and V Wind Components', 'Potential Temperature Anomaly']
Lcbarlabel = ['m/s','m²/s²','km/h','K']
Lxlab = ['latitude']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0, 8000)]*len(Lplot)
Lminval = [-15, 1, 0, 15]
Lmaxval = [15, 52.5, 220, 60]
Lstep = [0.2, 0.25, 0.1, 0.5]
Lstepticks = [2, 2.5, 10, 5]
Lfacconv = [1, 1, 3.6, 1]
Lcolormap = ['seismic','gist_rainbow_r','gist_rainbow_r','gist_rainbow_r','gist_rainbow_r']
Ltime = [Dvar['f1']['time']]*len(Lplot)
Lpltype = ['cf']*len(Lplot)
LaddWhite = [True]*len(Lplot)
orog = [orog_sec[i] for i in range(len(orog_sec))]
fig6 = Panel3.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Lylim=Lylim, Ltitle=Ltitle,
                        Lminval=Lminval, Lmaxval=Lmaxval, Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap,
                        Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, Ltime=Ltime, Lpltype=Lpltype, LaddWhite_cm=LaddWhite, orog=orog, ax=fig5.axes)

Panel3.save_graph(3,fig6)
