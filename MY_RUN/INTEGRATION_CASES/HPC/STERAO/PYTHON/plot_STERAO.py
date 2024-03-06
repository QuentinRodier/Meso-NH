#!/usr/bin/env python3
"""
@author: Quentin Rodier
Creation : 07/01/2021

Last modifications
"""
from read_MNHfile import read_netcdf
from Panel_Plot import PanelPlot
from misc_functions import oblique_proj
import numpy as np
import os

os.system('rm -f tempgraph*')
#
#  User's parameter / Namelist
#
#
path="../RUN/"

LnameFiles = ['STERA.1.CEN4T.007.nc', 'STERA.1.CEN4T.013.nc', 'STERA.1.CEN4T.019.nc']

Dvar_input = {'f'+str(i):['UT', 'VT', 'WT', 'INPRT', 'ACPRT', 'RCT', 'RIT', 'RGT', 
                          'EFIELDU', 'EFIELDV', 'EFIELDW', 'EMODULE', 'NI_IAGGS',
                          'NI_IDRYG', 'NI_SDRYG', 'INDUC_CG', 'QNIONP', 'QCELEC',
                          'QRELEC', 'QIELEC', 'QSELEC', 'QGELEC','QNIONN',
                          'ni', 'nj', 'ni_u', 'nj_u', 'level', 'level_w', 'time']
              for i in range(1,4)}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path, removeHALO=True)

################################################################
#########          PANEL 1
###############################################################
Panel1 = PanelPlot(1,3, [75,20],'Magnitude of U and V components of Wind at K=2', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50, bigtitlepad=0.97)

for i in range(1,4):
    Dvar['f'+str(i)]['UTVT'] = np.sqrt(Dvar['f'+str(i)]['UT']**2 + Dvar['f'+str(i)]['VT']**2)

Lplot = [Dvar['f'+str(i)]['UTVT'] for i in range(1,4)]
LaxeX = [Dvar['f'+str(i)]['ni'] for i in range(1,4)]
LaxeY = [Dvar['f'+str(i)]['nj'] for i in range(1,4)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['m/s']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['y (m)']*len(Lplot)
Lminval = [0]*len(Lplot)
Lmaxval = [30]*len(Lplot)
Lstep = [1]*len(Lplot)
Lstepticks = [1]*len(Lplot)
Llvl = [0]*len(Lplot)
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,4)]
Lcolormap = ['gist_ncar']*len(Lplot)
fig1 = Panel1.psectionH(lon=LaxeX, lat=LaxeY, Llevel=Llvl, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap)

Panel1.save_graph(1,fig1)

################################################################
#########          PANEL 2
###############################################################
Panel2 = PanelPlot(1,3, [75,20],'U and V components of Wind at K=2', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50, bigtitlepad=0.97)

Lplot1 = [Dvar['f'+str(i)]['UT'] for i in range(1,4)]
Lplot2 = [Dvar['f'+str(i)]['VT'] for i in range(1,4)]
Ltitle = ['']*len(Lplot1)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['y (m)']*len(Lplot)
Llegendval = [10]*len(Lplot1)
Llegendlabel = ['km/h']*len(Lplot1)
Larrowstep = [6]*len(Lplot1)
Lwidth = [0.002]*len(Lplot1)
Lcolor = ['black']*len(Lplot1)
Llvl = [0]*len(Lplot1)
lon = [Dvar['f'+str(i)]['ni'] for i in range(1,4)]
lat = [Dvar['f'+str(i)]['nj'] for i in range(1,4)]
Lscale = [200]*len(Lplot1)
Lfacconv = [3.6]*len(Lplot1)
fig2 = Panel2.pvector(Lxx=lon, Lyy=lat, Lvar1=Lplot1, Lvar2=Lplot2, Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle,
                      Lwidth=Lwidth, Larrowstep=Larrowstep, Lcolor=Lcolor, Llegendval=Llegendval,
                      Llegendlabel=Llegendlabel, Lscale=Lscale, Lfacconv=Lfacconv)

Panel2.save_graph(2,fig2)

################################################################
#########          PANEL 3
###############################################################
Panel3 = PanelPlot(1,3, [75,20],'W component of Wind at K=10', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50, bigtitlepad=0.97)

Lplot = [Dvar['f'+str(i)]['WT'] for i in range(1,4)]
LaxeX = [Dvar['f'+str(i)]['ni'] for i in range(1,4)]
LaxeY = [Dvar['f'+str(i)]['nj'] for i in range(1,4)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['m/s']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['y (m)']*len(Lplot)
Lminval = [-7]*len(Lplot)
Lmaxval = [6]*len(Lplot)
Lstep = [0.5]*len(Lplot)
Lstepticks = [1]*len(Lplot)
Llvl = [8]*len(Lplot)
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,4)]
Lcolormap = ['gist_ncar']*len(Lplot)
fig3 = Panel3.psectionH(lon=LaxeX, lat=LaxeY, Llevel=Llvl, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap)

Panel3.save_graph(3,fig3)

################################################################
#########          PANEL 4
###############################################################
Panel4 = PanelPlot(1,3, [75,20],'INPRT', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50, bigtitlepad=0.97)

Lplot = [Dvar['f'+str(i)]['INPRT'] for i in range(1,4)]
LaxeX = [Dvar['f'+str(i)]['ni'] for i in range(1,4)]
LaxeY = [Dvar['f'+str(i)]['nj'] for i in range(1,4)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['mm/hour']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['y (m)']*len(Lplot)
Lminval = [0]*len(Lplot)
Lmaxval = [12.5]*len(Lplot)
Lstep = [0.15]*len(Lplot)
Lstepticks = [1]*len(Lplot)
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,4)]
Lcolormap = ['gist_ncar']*len(Lplot)
LaddWhite_cm = [True]*len(Lplot)
fig4 = Panel4.psectionH(lon=LaxeX, lat=LaxeY, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap, LaddWhite_cm=LaddWhite_cm)

Panel4.save_graph(4,fig4)

################################################################
#########          PANEL 5
###############################################################
Panel5 = PanelPlot(1,3, [75,20],'ACPRT', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50, bigtitlepad=0.97)

i_beg, j_beg = (32,148)
i_end, j_end = (148,8)

Lplot = [Dvar['f'+str(i)]['ACPRT'] for i in range(1,4)]
LaxeX = [Dvar['f'+str(i)]['ni'] for i in range(1,4)]
LaxeY = [Dvar['f'+str(i)]['nj'] for i in range(1,4)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['mm']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['y (m)']*len(Lplot)
Lminval = [0]*len(Lplot)
Lmaxval = [4]*len(Lplot)
Lstep = [0.05]*len(Lplot)
Lstepticks = [0.5]*len(Lplot)
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,4)]
Lcolormap = ['gist_ncar']*len(Lplot)
LaddWhite_cm = [True]*len(Lplot)
fig5 = Panel5.psectionH(lon=LaxeX, lat=LaxeY, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap, LaddWhite_cm=LaddWhite_cm)

for i in range(1,4):
    Panel5.addLine(fig5.axes[(i-1)*2], [Dvar['f'+str(i)]['ni'][i_beg], Dvar['f'+str(i)]['nj'][j_beg]], [Dvar['f'+str(i)]['ni'][i_end], Dvar['f'+str(i)]['nj'][j_end]], 'black', 10)

Panel5.save_graph(5,fig5)

################################################################
#########          PANEL 6
###############################################################
Panel6 = PanelPlot(1,3, [75,20],'WT', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50, bigtitlepad=0.97)

i_beg, j_beg = (32,148)
i_end, j_end = (148,8)

angle_sec, var_sec1, axe_m = oblique_proj(Dvar['f1']['WT'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec2, axe_m = oblique_proj(Dvar['f2']['WT'], Dvar['f2']['ni'], Dvar['f2']['nj'], Dvar['f2']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec3, axe_m = oblique_proj(Dvar['f3']['WT'], Dvar['f3']['ni'], Dvar['f3']['nj'], Dvar['f3']['level_w'], i_beg, j_beg, i_end, j_end)

Lplot = [var_sec1, var_sec2, var_sec3]
LaxeX = [axe_m]*len(Lplot)
LaxeZ = [Dvar['f'+str(i)]['level_w'] for i in range(1,4)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['m/s']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['Altitude (m)']*len(Lplot)
Lminval = [-8,-4,-2.5]
Lmaxval = [35,4,6]
Lstep = [1,0.5,0.5]
Lstepticks = [1]*len(Lplot)
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,4)]
Lcolormap = ['gist_ncar']*len(Lplot)
fig6 = Panel6.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap)

Panel6.save_graph(6,fig6)

################################################################
#########          PANEL 7
###############################################################
Panel7 = PanelPlot(1,3, [75,20],'RCT', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50, bigtitlepad=0.97)

i_beg, j_beg = (32,148)
i_end, j_end = (148,8)

angle_sec, var_sec1, axe_m = oblique_proj(Dvar['f1']['RCT'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec2, axe_m = oblique_proj(Dvar['f2']['RCT'], Dvar['f2']['ni'], Dvar['f2']['nj'], Dvar['f2']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec3, axe_m = oblique_proj(Dvar['f3']['RCT'], Dvar['f3']['ni'], Dvar['f3']['nj'], Dvar['f3']['level_w'], i_beg, j_beg, i_end, j_end)

Lplot = [var_sec1, var_sec2, var_sec3]
LaxeX = [axe_m]*len(Lplot)
LaxeZ = [Dvar['f'+str(i)]['level'] for i in range(1,4)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['kg/kg']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['Altitude (m)']*len(Lplot)
Lminval = [0]*len(Lplot)
Lmaxval = [0.0026, 0.00085, 0.0014]
Lstep = [0.0001, 0.00002, 0.0001]
Lstepticks = [0.0002, 0.00005, 0.0002]
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,4)]
Lcolormap = ['gist_ncar']*len(Lplot)
LaddWhite_cm = [True]*len(Lplot)
fig7 = Panel7.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap, LaddWhite_cm=LaddWhite_cm)

Panel7.save_graph(7,fig7)

################################################################
#########          PANEL 8
###############################################################
Panel8 = PanelPlot(1,3, [75,20],'RIT', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50, bigtitlepad=0.97)

i_beg, j_beg = (32,148)
i_end, j_end = (148,8)

angle_sec, var_sec1, axe_m = oblique_proj(Dvar['f1']['RIT'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec2, axe_m = oblique_proj(Dvar['f2']['RIT'], Dvar['f2']['ni'], Dvar['f2']['nj'], Dvar['f2']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec3, axe_m = oblique_proj(Dvar['f3']['RIT'], Dvar['f3']['ni'], Dvar['f3']['nj'], Dvar['f3']['level_w'], i_beg, j_beg, i_end, j_end)

Lplot = [var_sec1, var_sec2, var_sec3]
LaxeX = [axe_m]*len(Lplot)
LaxeZ = [Dvar['f'+str(i)]['level'] for i in range(1,4)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['kg/kg']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['Altitude (m)']*len(Lplot)
Lminval = [0]*len(Lplot)
Lmaxval = [0.002, 0.0006, 0.00014]
Lstep = [0.0001, 0.00001, 0.000005]
Lstepticks = [0.0002, 0.00006, 0.00001]
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,4)]
Lcolormap = ['gist_ncar']*len(Lplot)
LaddWhite_cm = [True]*len(Lplot)
fig8 = Panel8.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap, LaddWhite_cm=LaddWhite_cm)

Panel8.save_graph(8,fig8)

################################################################
#########          PANEL 9
###############################################################
Panel9 = PanelPlot(1,3, [75,20],'RGT', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50, bigtitlepad=0.97)

i_beg, j_beg = (32,148)
i_end, j_end = (148,8)

angle_sec, var_sec1, axe_m = oblique_proj(Dvar['f1']['RGT'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec2, axe_m = oblique_proj(Dvar['f2']['RGT'], Dvar['f2']['ni'], Dvar['f2']['nj'], Dvar['f2']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec3, axe_m = oblique_proj(Dvar['f3']['RGT'], Dvar['f3']['ni'], Dvar['f3']['nj'], Dvar['f3']['level_w'], i_beg, j_beg, i_end, j_end)

Lplot = [var_sec1, var_sec2, var_sec3]
LaxeX = [axe_m]*len(Lplot)
LaxeZ = [Dvar['f'+str(i)]['level'] for i in range(1,4)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['kg/kg']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['Altitude (m)']*len(Lplot)
Lminval = [0]*len(Lplot)
Lmaxval = [0.006, 0.002, 0.00006]
Lstep = [0.0001, 0.0001, 0.000001]
Lstepticks = [0.0006, 0.0002, 0.000006]
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,4)]
Lcolormap = ['gist_ncar']*len(Lplot)
LaddWhite_cm = [True]*len(Lplot)
fig9 = Panel9.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap, LaddWhite_cm=LaddWhite_cm)

Panel9.save_graph(9,fig9)

################################################################
#########          PANEL 10
###############################################################
Panel10 = PanelPlot(1,3, [75,20],'EFIELDU', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50, bigtitlepad=0.97)

i_beg, j_beg = (32,148)
i_end, j_end = (148,8)

angle_sec, var_sec1, axe_m = oblique_proj(Dvar['f1']['EFIELDU'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec2, axe_m = oblique_proj(Dvar['f2']['EFIELDU'], Dvar['f2']['ni'], Dvar['f2']['nj'], Dvar['f2']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec3, axe_m = oblique_proj(Dvar['f3']['EFIELDU'], Dvar['f3']['ni'], Dvar['f3']['nj'], Dvar['f3']['level_w'], i_beg, j_beg, i_end, j_end)

Lplot = [var_sec1, var_sec2, var_sec3]
LaxeX = [axe_m]*len(Lplot)
LaxeZ = [Dvar['f'+str(i)]['level'] for i in range(1,4)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['V/m']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['Altitude (m)']*len(Lplot)
Lminval = [-8000,-2400,-900]
Lmaxval = [6000, 800, 300]
Lstep = [50, 20, 10]
Lstepticks = [500, 200, 100]
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,4)]
Lcolormap = ['gist_ncar']*len(Lplot)
fig10 = Panel10.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap)

Panel10.save_graph(10,fig10)

################################################################
#########          PANEL 10
###############################################################
Panel11 = PanelPlot(1,3, [75,20],'EFIELDV', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50, bigtitlepad=0.97)

i_beg, j_beg = (32,148)
i_end, j_end = (148,8)

angle_sec, var_sec1, axe_m = oblique_proj(Dvar['f1']['EFIELDV'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec2, axe_m = oblique_proj(Dvar['f2']['EFIELDV'], Dvar['f2']['ni'], Dvar['f2']['nj'], Dvar['f2']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec3, axe_m = oblique_proj(Dvar['f3']['EFIELDV'], Dvar['f3']['ni'], Dvar['f3']['nj'], Dvar['f3']['level_w'], i_beg, j_beg, i_end, j_end)

Lplot = [var_sec1, var_sec2, var_sec3]
LaxeX = [axe_m]*len(Lplot)
LaxeZ = [Dvar['f'+str(i)]['level'] for i in range(1,4)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['V/m']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['Altitude (m)']*len(Lplot)
Lminval = [-9500,-2000,-900]
Lmaxval = [11000, 900, 180]
Lstep = [100, 20, 5]
Lstepticks = [1000, 200, 50]
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,4)]
Lcolormap = ['gist_ncar']*len(Lplot)
fig11 = Panel11.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap)

Panel11.save_graph(11,fig11)

################################################################
#########          PANEL 10
###############################################################
Panel12 = PanelPlot(1,3, [75,20],'EFIELDW', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50, bigtitlepad=0.97)

i_beg, j_beg = (32,148)
i_end, j_end = (148,8)

angle_sec, var_sec1, axe_m = oblique_proj(Dvar['f1']['EFIELDW'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec2, axe_m = oblique_proj(Dvar['f2']['EFIELDW'], Dvar['f2']['ni'], Dvar['f2']['nj'], Dvar['f2']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec3, axe_m = oblique_proj(Dvar['f3']['EFIELDW'], Dvar['f3']['ni'], Dvar['f3']['nj'], Dvar['f3']['level_w'], i_beg, j_beg, i_end, j_end)

Lplot = [var_sec1, var_sec2, var_sec3]
LaxeX = [axe_m]*len(Lplot)
LaxeZ = [Dvar['f'+str(i)]['level'] for i in range(1,4)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['V/m']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['Altitude (m)']*len(Lplot)
Lminval = [-11000,-1000,-400]
Lmaxval = [16500, 2800, 800]
Lstep = [150, 20, 10]
Lstepticks = [1500, 200, 100]
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,4)]
Lcolormap = ['gist_ncar']*len(Lplot)
fig12 = Panel12.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap)

Panel12.save_graph(12,fig12)

################################################################
#########          PANEL 13
###############################################################
Panel13 = PanelPlot(1,3, [75,20],'EMODULE', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50, bigtitlepad=0.97)

i_beg, j_beg = (32,148)
i_end, j_end = (148,8)

angle_sec, var_sec1, axe_m = oblique_proj(Dvar['f1']['EMODULE'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec2, axe_m = oblique_proj(Dvar['f2']['EMODULE'], Dvar['f2']['ni'], Dvar['f2']['nj'], Dvar['f2']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec3, axe_m = oblique_proj(Dvar['f3']['EMODULE'], Dvar['f3']['ni'], Dvar['f3']['nj'], Dvar['f3']['level_w'], i_beg, j_beg, i_end, j_end)

Lplot = [var_sec1, var_sec2, var_sec3]
LaxeX = [axe_m]*len(Lplot)
LaxeZ = [Dvar['f'+str(i)]['level'] for i in range(1,4)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['V/m']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['Altitude (m)']*len(Lplot)
Lminval = [0]*len(Lplot)
Lmaxval = [16500, 3000, 1200]
Lstep = [500, 100, 50]
Lstepticks = [1000, 200, 100]
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,4)]
Lcolormap = ['gist_ncar']*len(Lplot)
LaddWhite_cm = [True]*len(Lplot)
fig13 = Panel13.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap, LaddWhite_cm=LaddWhite_cm)

Panel13.save_graph(13,fig13)

################################################################
#########          PANEL 14
###############################################################
Panel14 = PanelPlot(1,3, [75,20],'NI IAGGS', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50, bigtitlepad=0.97)

i_beg, j_beg = (32,148)
i_end, j_end = (148,8)

angle_sec, var_sec1, axe_m = oblique_proj(Dvar['f1']['NI_IAGGS'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec2, axe_m = oblique_proj(Dvar['f2']['NI_IAGGS'], Dvar['f2']['ni'], Dvar['f2']['nj'], Dvar['f2']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec3, axe_m = oblique_proj(Dvar['f3']['NI_IAGGS'], Dvar['f3']['ni'], Dvar['f3']['nj'], Dvar['f3']['level_w'], i_beg, j_beg, i_end, j_end)

Lplot = [var_sec1, var_sec2, var_sec3]
LaxeX = [axe_m]*len(Lplot)
LaxeZ = [Dvar['f'+str(i)]['level'] for i in range(1,4)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['pC m-3 s-1']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['Altitude (m)']*len(Lplot)
Lminval = [0]*len(Lplot)
Lmaxval = [1.8, 0.012, 1]
Lstep = [0.1, 0.0005, 0.5]
Lstepticks = [0.2, 0.001, 0.5]
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,4)]
Lcolormap = ['gist_ncar']*len(Lplot)
LaddWhite_cm = [True]*len(Lplot)
fig14 = Panel14.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap, LaddWhite_cm=LaddWhite_cm)

Panel14.save_graph(14,fig14)

################################################################
#########          PANEL 15
###############################################################
Panel15 = PanelPlot(1,3, [75,20],'NI IDRYG', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50, bigtitlepad=0.97)

i_beg, j_beg = (32,148)
i_end, j_end = (148,8)

angle_sec, var_sec1, axe_m = oblique_proj(Dvar['f1']['NI_IDRYG'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec2, axe_m = oblique_proj(Dvar['f2']['NI_IDRYG'], Dvar['f2']['ni'], Dvar['f2']['nj'], Dvar['f2']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec3, axe_m = oblique_proj(Dvar['f3']['NI_IDRYG'], Dvar['f3']['ni'], Dvar['f3']['nj'], Dvar['f3']['level_w'], i_beg, j_beg, i_end, j_end)

Lplot = [var_sec1, var_sec2, var_sec3]
LaxeX = [axe_m]*len(Lplot)
LaxeZ = [Dvar['f'+str(i)]['level'] for i in range(1,4)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['pC m-3 s-1']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['Altitude (m)']*len(Lplot)
Lminval = [0]*len(Lplot)
Lmaxval = [9, 0.08, 1]
Lstep = [0.2, 0.005, 0.5]
Lstepticks = [1, 0.01, 0.5]
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,4)]
Lcolormap = ['gist_ncar']*len(Lplot)
LaddWhite_cm = [True]*len(Lplot)
fig15 = Panel15.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap, LaddWhite_cm=LaddWhite_cm)

Panel15.save_graph(15,fig15)

################################################################
#########          PANEL 16
###############################################################
Panel16 = PanelPlot(1,3, [75,20],'NI SDRYG', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50, bigtitlepad=0.97)

i_beg, j_beg = (32,148)
i_end, j_end = (148,8)

angle_sec, var_sec1, axe_m = oblique_proj(Dvar['f1']['NI_SDRYG'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec2, axe_m = oblique_proj(Dvar['f2']['NI_SDRYG'], Dvar['f2']['ni'], Dvar['f2']['nj'], Dvar['f2']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec3, axe_m = oblique_proj(Dvar['f3']['NI_SDRYG'], Dvar['f3']['ni'], Dvar['f3']['nj'], Dvar['f3']['level_w'], i_beg, j_beg, i_end, j_end)

Lplot = [var_sec1, var_sec2, var_sec3]
LaxeX = [axe_m]*len(Lplot)
LaxeZ = [Dvar['f'+str(i)]['level'] for i in range(1,4)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['pC m-3 s-1']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['Altitude (m)']*len(Lplot)
Lminval = [0]*len(Lplot)
Lmaxval = [90, 4, 1]
Lstep = [2.5, 0.1, 0.5]
Lstepticks = [5, 0.5, 0.5]
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,4)]
Lcolormap = ['gist_ncar']*len(Lplot)
LaddWhite_cm = [True]*len(Lplot)
fig16 = Panel16.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap, LaddWhite_cm=LaddWhite_cm)

Panel16.save_graph(16,fig16)

################################################################
#########          PANEL 17
###############################################################
Panel17 = PanelPlot(1,3, [75,20],'INDUC CG', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50, bigtitlepad=0.97)

i_beg, j_beg = (32,148)
i_end, j_end = (148,8)

angle_sec, var_sec1, axe_m = oblique_proj(Dvar['f1']['INDUC_CG'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec2, axe_m = oblique_proj(Dvar['f2']['INDUC_CG'], Dvar['f2']['ni'], Dvar['f2']['nj'], Dvar['f2']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec3, axe_m = oblique_proj(Dvar['f3']['INDUC_CG'], Dvar['f3']['ni'], Dvar['f3']['nj'], Dvar['f3']['level_w'], i_beg, j_beg, i_end, j_end)

Lplot = [var_sec1, var_sec2, var_sec3]
LaxeX = [axe_m]*len(Lplot)
LaxeZ = [Dvar['f'+str(i)]['level'] for i in range(1,4)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['pC m-3 s-1']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['Altitude (m)']*len(Lplot)
Lminval = [0]*len(Lplot)
Lmaxval = [1, 1, 1]
Lstep = [0.5, 0.5, 0.5]
Lstepticks = [0.5, 0.5, 0.5]
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,4)]
Lcolormap = ['gist_ncar']*len(Lplot)
LaddWhite_cm = [True]*len(Lplot)
fig17 = Panel17.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap, LaddWhite_cm=LaddWhite_cm)

Panel17.save_graph(17,fig17)

################################################################
#########          PANEL 18
###############################################################
Panel18 = PanelPlot(1,3, [75,20],'QNIONP', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50, bigtitlepad=0.97)

i_beg, j_beg = (32,148)
i_end, j_end = (148,8)

angle_sec, var_sec1, axe_m = oblique_proj(Dvar['f1']['QNIONP'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec2, axe_m = oblique_proj(Dvar['f2']['QNIONP'], Dvar['f2']['ni'], Dvar['f2']['nj'], Dvar['f2']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec3, axe_m = oblique_proj(Dvar['f3']['QNIONP'], Dvar['f3']['ni'], Dvar['f3']['nj'], Dvar['f3']['level_w'], i_beg, j_beg, i_end, j_end)

Lplot = [var_sec1, var_sec2, var_sec3]
LaxeX = [axe_m]*len(Lplot)
LaxeZ = [Dvar['f'+str(i)]['level'] for i in range(1,4)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['kg-1']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['Altitude (m)']*len(Lplot)
Lminval = [1e9]*len(Lplot)
Lmaxval = [3e10]*len(Lplot)
Lstep = [1e8]*len(Lplot)
Lstepticks = [1e9]*len(Lplot)
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,4)]
Lcolormap = ['gist_ncar']*len(Lplot)
LaddWhite_cm = [True]*len(Lplot)
fig18 = Panel18.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap, LaddWhite_cm=LaddWhite_cm)

Panel18.save_graph(18,fig18)

################################################################
#########          PANEL 19
###############################################################
Panel19 = PanelPlot(1,3, [75,20],'QCELEC', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50, bigtitlepad=0.97)

i_beg, j_beg = (32,148)
i_end, j_end = (148,8)

angle_sec, var_sec1, axe_m = oblique_proj(Dvar['f1']['QCELEC'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec2, axe_m = oblique_proj(Dvar['f2']['QCELEC'], Dvar['f2']['ni'], Dvar['f2']['nj'], Dvar['f2']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec3, axe_m = oblique_proj(Dvar['f3']['QCELEC'], Dvar['f3']['ni'], Dvar['f3']['nj'], Dvar['f3']['level_w'], i_beg, j_beg, i_end, j_end)

Lplot = [var_sec1, var_sec2, var_sec3]
LaxeX = [axe_m]*len(Lplot)
LaxeZ = [Dvar['f'+str(i)]['level'] for i in range(1,4)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['C/kg']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['Altitude (m)']*len(Lplot)
Lminval = [-1.2e-10,-2.5e-11,-1.1e-11]
Lmaxval = [1.4e-10,1.4e-11,5e-13]
Lstep = [1e-11, 1e-12, 5e-14]
Lstepticks = [1e-10, 1e-11, 5e-13]
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,4)]
Lcolormap = ['gist_ncar']*len(Lplot)
fig19 = Panel19.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap)

Panel19.save_graph(19,fig19)

################################################################
#########          PANEL 20
###############################################################
Panel20 = PanelPlot(1,3, [75,20],'QRELEC', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50, bigtitlepad=0.97)

i_beg, j_beg = (32,148)
i_end, j_end = (148,8)

angle_sec, var_sec1, axe_m = oblique_proj(Dvar['f1']['QRELEC'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec2, axe_m = oblique_proj(Dvar['f2']['QRELEC'], Dvar['f2']['ni'], Dvar['f2']['nj'], Dvar['f2']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec3, axe_m = oblique_proj(Dvar['f3']['QRELEC'], Dvar['f3']['ni'], Dvar['f3']['nj'], Dvar['f3']['level_w'], i_beg, j_beg, i_end, j_end)

Lplot = [var_sec1, var_sec2, var_sec3]
LaxeX = [axe_m]*len(Lplot)
LaxeZ = [Dvar['f'+str(i)]['level'] for i in range(1,4)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['C/kg']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['Altitude (m)']*len(Lplot)
Lminval = [-7.3e-11,-1.7e-13,-1.4e-13]
Lmaxval = [7.3e-11,7.6e-13,7.9e-13]
Lstep = [1e-12, 1e-14, 1e-14]
Lstepticks = [1e-11, 1e-13, 1e-13]
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,4)]
Lcolormap = ['gist_ncar']*len(Lplot)
fig20 = Panel20.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap)

Panel20.save_graph(20,fig20)

################################################################
#########          PANEL 21
###############################################################
Panel21 = PanelPlot(1,3, [75,20],'QIELEC', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50, bigtitlepad=0.97)

i_beg, j_beg = (32,148)
i_end, j_end = (148,8)

angle_sec, var_sec1, axe_m = oblique_proj(Dvar['f1']['QIELEC'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec2, axe_m = oblique_proj(Dvar['f2']['QIELEC'], Dvar['f2']['ni'], Dvar['f2']['nj'], Dvar['f2']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec3, axe_m = oblique_proj(Dvar['f3']['QIELEC'], Dvar['f3']['ni'], Dvar['f3']['nj'], Dvar['f3']['level_w'], i_beg, j_beg, i_end, j_end)

Lplot = [var_sec1, var_sec2, var_sec3]
LaxeX = [axe_m]*len(Lplot)
LaxeZ = [Dvar['f'+str(i)]['level'] for i in range(1,4)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['C/kg']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['Altitude (m)']*len(Lplot)
Lminval = [-1.5e-9,-1.9e-10,-6.4e-13]
Lmaxval = [2.2e-11,6.1e-13,9.9e-13]
Lstep = [1e-11, 1e-12, 1e-14]
Lstepticks = [1e-10, 1e-11, 1e-13]
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,4)]
Lcolormap = ['gist_ncar']*len(Lplot)
fig21 = Panel21.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap)

Panel21.save_graph(21,fig21)

################################################################
#########          PANEL 22
###############################################################
Panel22 = PanelPlot(1,3, [75,20],'QSELEC', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50, bigtitlepad=0.97)

i_beg, j_beg = (32,148)
i_end, j_end = (148,8)

angle_sec, var_sec1, axe_m = oblique_proj(Dvar['f1']['QSELEC'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec2, axe_m = oblique_proj(Dvar['f2']['QSELEC'], Dvar['f2']['ni'], Dvar['f2']['nj'], Dvar['f2']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec3, axe_m = oblique_proj(Dvar['f3']['QSELEC'], Dvar['f3']['ni'], Dvar['f3']['nj'], Dvar['f3']['level_w'], i_beg, j_beg, i_end, j_end)

Lplot = [var_sec1, var_sec2, var_sec3]
LaxeX = [axe_m]*len(Lplot)
LaxeZ = [Dvar['f'+str(i)]['level'] for i in range(1,4)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['C/kg']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['Altitude (m)']*len(Lplot)
Lminval = [-3e-9,-8.1e-10,-4.6e-11]
Lmaxval = [2.7e-10,2e-11,4.8e-12]
Lstep = [1e-11, 5e-12, 5e-13]
Lstepticks = [1e-10, 5e-11, 5e-12]
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,4)]
Lcolormap = ['gist_ncar']*len(Lplot)
fig22 = Panel22.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap)

Panel22.save_graph(22,fig22)

################################################################
#########          PANEL 23
###############################################################
Panel23 = PanelPlot(1,3, [75,20],'QGELEC', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50, bigtitlepad=0.97)

i_beg, j_beg = (32,148)
i_end, j_end = (148,8)

angle_sec, var_sec1, axe_m = oblique_proj(Dvar['f1']['QGELEC'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec2, axe_m = oblique_proj(Dvar['f2']['QGELEC'], Dvar['f2']['ni'], Dvar['f2']['nj'], Dvar['f2']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec3, axe_m = oblique_proj(Dvar['f3']['QGELEC'], Dvar['f3']['ni'], Dvar['f3']['nj'], Dvar['f3']['level_w'], i_beg, j_beg, i_end, j_end)

Lplot = [var_sec1, var_sec2, var_sec3]
LaxeX = [axe_m]*len(Lplot)
LaxeZ = [Dvar['f'+str(i)]['level'] for i in range(1,4)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['C/kg']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['Altitude (m)']*len(Lplot)
Lminval = [-1.8e-11,-1.3e-14,0]
Lmaxval = [3.3e-9,8.2e-10,4.3e-11]
Lstep = [1e-10, 1e-11, 1e-12]
Lstepticks = [5e-10, 5e-11, 5e-12]
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,4)]
Lcolormap = ['gist_ncar']*len(Lplot)
LaddWhite_cm = [True]*len(Lplot)
fig23 = Panel23.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap, LaddWhite_cm=LaddWhite_cm)

Panel23.save_graph(23,fig23)

################################################################
#########          PANEL 24
###############################################################
Panel24 = PanelPlot(1,3, [75,20],'QNIONN', minmaxpad=1.05, titlepad=80, xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=50, bigtitlepad=0.97)

i_beg, j_beg = (32,148)
i_end, j_end = (148,8)

angle_sec, var_sec1, axe_m = oblique_proj(Dvar['f1']['QNIONN'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec2, axe_m = oblique_proj(Dvar['f2']['QNIONN'], Dvar['f2']['ni'], Dvar['f2']['nj'], Dvar['f2']['level_w'], i_beg, j_beg, i_end, j_end)
angle_sec, var_sec3, axe_m = oblique_proj(Dvar['f3']['QNIONN'], Dvar['f3']['ni'], Dvar['f3']['nj'], Dvar['f3']['level_w'], i_beg, j_beg, i_end, j_end)

Lplot = [var_sec1, var_sec2, var_sec3]
LaxeX = [axe_m]*len(Lplot)
LaxeZ = [Dvar['f'+str(i)]['level'] for i in range(1,4)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['kg-1']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['Altitude (m)']*len(Lplot)
Lminval = [1e9]*len(Lplot)
Lmaxval = [3e10]*len(Lplot)
Lstep = [1e8]*len(Lplot)
Lstepticks = [1e9]*len(Lplot)
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,4)]
Lcolormap = ['gist_ncar']*len(Lplot)
LaddWhite_cm = [True]*len(Lplot)
fig24 = Panel24.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap, LaddWhite_cm=LaddWhite_cm)

Panel24.save_graph(24,fig24)
