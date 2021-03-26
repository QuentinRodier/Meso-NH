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
from misc_functions import mean_operator, convert_date
import cartopy.crs as ccrs
import numpy as np
import os

os.system('rm -f tempgraph*')
#
#  User's parameter / Namelist
#
output_name = '012_dust.pdf'                 #Name of the output PDF file
path=""
LnameFiles = ['DUST7.1.SEG02.004.nc']

Dvar_input = {
'f1':['ZS', 'UT','VT', 'WT','THT',
      'DSTM03T','DSTM33T','DSTM02T','DSTM32T','DSTM01T','DSTM31T','F_DST001P1','F_DST002P1','F_DST003P1',
      'latitude','longitude','level',
      'INPRR','ACPRR','PABST','RCT','RVT','RRT','LSTHM']}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path, removeHALO=True)
Dvar['f1']['date'] = convert_date(Dvar['f1']['date'], Dvar['f1']['time'])
################################################################
#########          PANEL 1
###############################################################
Panel1 = PanelPlot(1,2, [20,10],'012_dust', titlepad=25, minmaxpad=1.04, timepad=-0.10, colorbarpad=0.01, colorbaraspect=30)
tomass = mean_operator()
Dvar['f1']['UM'] = tomass.MXM(Dvar['f1']['UT'])
Dvar['f1']['VM'] = tomass.MYM(Dvar['f1']['VT'])
Dvar['f1']['WIND'] = np.sqrt(Dvar['f1']['VM']**2 + Dvar['f1']['UM']**2)

Lplot = [ Dvar['f1']['WIND'],Dvar['f1']['THT']]
lon = [Dvar['f1']['longitude']]*len(Lplot)
lat = [Dvar['f1']['latitude']]*len(Lplot)
Ltitle = ['Wind speed', 'Potential Temperature at 1st level']
Lcbarlabel = ['m/s','K']
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude']*len(Lplot)
Lminval = [0, 296.5]
Lmaxval = [10.5, 307.5]
Lstep = [0.5, 0.5]
Lstepticks = Lstep
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Lprojection = [ccrs.PlateCarree()]*len(Lplot)
Llvl = [0, 0]
LaddWhite = [True, False]
Ltime = [Dvar['f1']['date']]*len(Lplot)

fig1 = Panel1.psectionH(lon=lon, lat=lat, Lvar=Lplot, Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, 
                                Ltime=Ltime, LaddWhite_cm=LaddWhite, Lproj=Lprojection)

Lplot1 = [ Dvar['f1']['UM']]
Lplot2 = [ Dvar['f1']['VM']]
Ltitle = [' vectors at 1st level']
Llegendval = [20]
Lcbarlabel = ['m/s']*len(Lplot)
Larrowstep = [1]*len(Lplot)
Lwidth = [0.002]*len(Lplot)
Lcolor = ['black']*len(Lplot)
Lscale = [200]*len(Lplot)

fig2 = Panel1.pvector(Lxx=lon, Lyy=lat, Lvar1=Lplot1, Llevel=Llvl, Lvar2=Lplot2, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lwidth=Lwidth, Larrowstep=Larrowstep, 
                        Llegendval=Llegendval, Lcbarlabel=Lcbarlabel, Lid_overlap=[0], ax=fig1.axes, Lscale=Lscale)

Panel1.save_graph(1,fig2)
################################################################
#########          PANEL 2
###############################################################
Panel2 = PanelPlot(2,3, [25,14],'', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)
Lplot = [ Dvar['f1']['DSTM03T'],Dvar['f1']['DSTM33T'],Dvar['f1']['DSTM02T'],Dvar['f1']['DSTM32T'],Dvar['f1']['DSTM01T'],Dvar['f1']['DSTM31T']]
lon = [Dvar['f1']['longitude']]*len(Lplot)
lat = [Dvar['f1']['latitude']]*len(Lplot)
Ltitle = ['DSTM03T','DSTM33T','DSTM02T','DSTM32T','DSTM01T','DSTM31T']
Lcbarlabel = ['kg/kg']*len(Lplot)
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude']*len(Lplot)
Lminval = [0,0,0,0,0,0]
Lmaxval = [0.11E-21, 0.62E-9, 0.33E-17, 0.44E-6, 0.45E-16, 0.17E-7]
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Lprojection = [ccrs.PlateCarree()]*len(Lplot)
LaddWhite = [True]*len(Lplot)
Ltime = [Dvar['f1']['time']]*len(Lplot)
Llvl = [0]*len(Lplot)
fig3 = Panel2.psectionH(lon=lon, lat=lat, Lvar=Lplot, Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=[], Lstepticks=[], Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, 
                                Ltime=Ltime, LaddWhite_cm=LaddWhite, Lproj=Lprojection, ax=[])

Panel2.save_graph(2,fig3)
################################################################
#########          PANEL 3
###############################################################
Panel3 = PanelPlot(2,3, [25,14],'', titlepad=25, minmaxpad=1.04, timepad=-0.10, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=50)
Lplot = [ Dvar['f1']['F_DST001P1'],Dvar['f1']['F_DST002P1'],Dvar['f1']['F_DST003P1']]
lon = [Dvar['f1']['longitude']]*len(Lplot)
lat = [Dvar['f1']['latitude']]*len(Lplot)
Ltitle = ['F_DST001P1','F_DST002P1','F_DST003P1']
Lcbarlabel = ['kg/m2/s']*len(Lplot)
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude']*len(Lplot)
Lminval = [0,0,0]
Lmaxval = [0.54E-9,0.76E-7, 0.28E-8]
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Lprojection = [ccrs.PlateCarree()]*len(Lplot)
LaddWhite = [True]*len(Lplot)
Ltime = [Dvar['f1']['date']]*len(Lplot)
Lcarte = [Dvar['f1']['longitude'].min(),Dvar['f1']['longitude'].max(), Dvar['f1']['latitude'].min(), Dvar['f1']['latitude'].max()]
fig4 = Panel3.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=[], Lstepticks=[], Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel,Lcarte=Lcarte,
                                Ltime=Ltime, LaddWhite_cm=LaddWhite, Lproj=Lprojection, ax=[])

Panel3.save_graph(3,fig4)
