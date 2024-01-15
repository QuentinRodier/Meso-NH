#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Oct  2 10:36:47 2023

@author: lepyo
"""
from read_MNHfile import read_netcdf
from Panel_Plot import PanelPlot
import numpy as np
import os

os.system('rm -f tempgraph*')
#
#  User's parameter / Namelist
#
path=""

LnameFiles = ['BUBBL.1.CEN4T.00'+str(i)+'.nc' for i in range(1,10)] + ['BUBBL.1.CEN4T.010.nc']

Dvar_input = {"f"+str(i):['UT','WT','THT','ni_u','level', 'ni','level_w','time'] for i in range(1,11)}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path, removeHALO=True)

################################################################
#########          PANEL 1 : u-wind UT
###############################################################
Panel1 = PanelPlot(5,2,[60,80],'Cold bubble : u-wind UT', xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=40)

Lplot = [Dvar['f'+str(i)]['UT'][:100,512:872] for i in range(1,11)]
LaxeX = [Dvar['f'+str(i)]['ni_u'][512:872] for i in range(1,11)]
LaxeY = [Dvar['f'+str(i)]['level'][:100] for i in range(1,11)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['m/s']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lminval = [-30]*len(Lplot)
Lmaxval = [45]*len(Lplot)
Lstep = [2.5]*len(Lplot)
Lstepticks = [10]*len(Lplot)
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,11)]
Lcolormap = ['gist_ncar']*len(Lplot)
fig1 = Panel1.psectionV(Lxx=LaxeX, Lzz=LaxeY, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap)

Panel1.save_graph(1,fig1)

################################################################
#########          PANEL 2 : Vertical velocity WT
###############################################################
Panel2 = PanelPlot(5,2,[60,80],'Cold bubble : Vertical velocity WT', xlabelSize=25, ylabelSize=25, legendSize = 25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=40)

Lplot = [Dvar['f'+str(i)]['WT'][:100,512:872] for i in range(1,11)]
LaxeX = [Dvar['f'+str(i)]['ni'][512:872] for i in range(1,11)]
LaxeY = [Dvar['f'+str(i)]['level_w'][:100] for i in range(1,11)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['m/s']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lminval = [-26]*len(Lplot)
Lmaxval = [26]*len(Lplot)
Lstep = [2]*len(Lplot)
Lstepticks = [5]*len(Lplot)
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,11)]
Lcolormap = ['gist_ncar']*len(Lplot)
fig2 = Panel2.psectionV(Lxx=LaxeX, Lzz=LaxeY, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap)

Panel2.save_graph(2,fig2)

################################################################
#########          PANEL 3 : Potential Temperature Anomaly THT - 300K
###############################################################
Panel3 = PanelPlot(5,2,[60,80],'Cold bubble : Potential Temperature Anomaly THT - 300K', xlabelSize=25, ylabelSize=25, legendSize = 25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=40)

Lplot = [np.round(Dvar['f'+str(i)]['THT'][:100,512:872],7)-300 for i in range(1,11)]
LaxeX = [Dvar['f'+str(i)]['ni'][512:872] for i in range(1,11)]
LaxeY = [Dvar['f'+str(i)]['level'][:100] for i in range(1,11)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['K']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lminval = [-17]*len(Lplot)
Lmaxval = [0]*len(Lplot)
Lstep = [1]*len(Lplot)
Lstepticks = Lstep
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,11)]
Lcolormap = ['gist_ncar']*len(Lplot)
LaddWhite_cm = [True]*len(Lplot)
LwhiteTop = [True]*len(Lplot)
fig3 = Panel3.psectionV(Lxx=LaxeX, Lzz=LaxeY, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap, LwhiteTop=LwhiteTop, LaddWhite_cm=LaddWhite_cm)

Panel3.save_graph(3,fig3)
