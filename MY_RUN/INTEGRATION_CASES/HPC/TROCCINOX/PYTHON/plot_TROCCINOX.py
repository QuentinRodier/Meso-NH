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

os.system('rm -f tempgraph*')
#
#  User's parameter / Namelist
#
#
path=""

LnameFiles = ['TROCC.1.CEN4T.008dg.nc']

Dvar_input = {'f1':['LINOX', 'IC_TOTAL_NB', 'PABST', 'UT', 'VT', 'ni_u', 'nj_u', 'level', 'latitude', 'longitude','time']}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path, removeHALO=True)

################################################################
#########          PANEL 1  
###############################################################
Panel1 = PanelPlot(1,2, [20,10],'', minmaxpad=1.05)

Lplot = [Dvar['f1']['LINOX'], Dvar['f1']['IC_TOTAL_NB']]
lon = [Dvar['f1']['longitude']]*len(Lplot)
lat = [Dvar['f1']['latitude']]*len(Lplot)
Ltitle = ['LiNOx tracer (ppbv) at 300hPa (m49)', 'Intracloud lightning number (m49)']
Lcbarlabel = ['ppb', '1']
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude']*len(Lplot)
Lminval = [0.2, 10]
Lmaxval = [3.6, 8500]
Lstep = [0.02, 50]
Lstepticks = [0.2, 500]
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Llvl = [40, 0]
Ltime = [Dvar['f1']['time']]*len(Lplot)
Lproj = [ccrs.PlateCarree()]*len(Lplot)
fig1 = Panel1.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lcarte=[], Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lproj=Lproj)

fig1.tight_layout()

# Wind vectors
Lplot1 = [Dvar['f1']['UT']]
Lplot2 = [Dvar['f1']['VT']]
Ltitle = ['Wind']
Llegendval = [10]
Llegendlabel = ['m/s']
Larrowstep = [6]
Lwidth = [0.002]
Lcolor = ['black']
Llvl = [40]
lon = [Dvar['f1']['longitude']]
lat = [Dvar['f1']['latitude']]
Lscale = [600]
fig2 = Panel1.pvector(Lxx=lon, Lyy=lat, Lvar1=Lplot1, Lvar2=Lplot2, Llevel=Llvl, 
                      Ltitle=Ltitle, Lwidth=Lwidth, Larrowstep=Larrowstep, 
                      Lcolor=Lcolor, Llegendval=Llegendval, Llegendlabel=Llegendlabel, Lid_overlap=[0], ax=fig1.axes, Lscale=Lscale, Lproj=Lproj)

Panel1.save_graph(1,fig2)
