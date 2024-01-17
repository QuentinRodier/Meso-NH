#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Oct  2 10:36:47 2023

@author: lepyo
"""
from MNHPy.read_MNHfile import read_netcdf
from MNHPy.Panel_Plot import PanelPlot
import os

os.system('rm -f tempgraph*')
#
#  User's parameter / Namelist
#
path=""
    
LnameFiles = ['HYDRO.1.CEN4T.00'+str(i)+'.nc' for i in range(1,10)] + ['HYDRO.1.CEN4T.01'+str(i)+'.nc' for i in range(3)]

Dvar_input = {"f"+str(i):['UT','WT','LSUM','ni_u','level', 'ni','level_w','time'] for i in range(1,13)}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path, removeHALO=True)

################################################################
#########          PANEL 1 : UT(time) - LSUM(0)
###############################################################
Panel1 = PanelPlot(6,2, [40,80],'Hydro : UT(time) - LSUM(0)', xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=40)

Lplot = [Dvar['f'+str(i)]['UT'] - Dvar['f1']['LSUM'] for i in range(1,13)]
LaxeX = [Dvar['f'+str(i)]['ni_u'] for i in range(1,13)]
LaxeY = [Dvar['f'+str(i)]['level'] for i in range(1,13)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['m/s']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lminval = [-31.25]*len(Lplot)
Lmaxval = [31.25]*len(Lplot)
Lstep = [2.5]*len(Lplot)
Lstepticks = Lstep
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,13)]
Lcolormap = ['seismic']*len(Lplot)
Lfacconv = [1000]*len(Lplot)
fig1 = Panel1.psectionV(Lxx=LaxeX, Lzz=LaxeY, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap, Lfacconv=Lfacconv)

Panel1.save_graph(1,fig1)

################################################################
#########          PANEL 2 : Vertical velocity WT
###############################################################
Panel2 = PanelPlot(6,2, [40,80],'Hydro : Vertical velocity WT', xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=40)

Lplot = [Dvar['f'+str(i)]['WT'] for i in range(1,13)]
LaxeX = [Dvar['f'+str(i)]['ni'] for i in range(1,13)]
LaxeY = [Dvar['f'+str(i)]['level_w'] for i in range(1,13)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['m/s']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lminval = [-4.75]*len(Lplot)
Lmaxval = [4.75]*len(Lplot)
Lstep = [0.5]*len(Lplot)
Lstepticks = Lstep
Ltime = [Dvar['f'+str(i)]['time'] for i in range(1,13)]
Lcolormap = ['seismic']*len(Lplot)
Lfacconv = [1000]*len(Lplot)
fig2 = Panel2.psectionV(Lxx=LaxeX, Lzz=LaxeY, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap, Lfacconv=Lfacconv)

Panel2.save_graph(2,fig2)

################################################################
#########          PANEL 3 : WT(time) - WT(0)
###############################################################
Panel3 = PanelPlot(6,2, [40,80],'Hydro : WT(time) - WT(0)', xlabelSize=25, ylabelSize=25, legendSize=25, timeSize=25, cbTicksLabelSize=25, cbTitleSize=25, xyTicksLabelSize=25, minmaxTextSize=25, titleSize=30, bigtitleSize=40)

Lplot = [Dvar['f'+str(i)]['WT'] - Dvar['f1']['WT'] for i in range(2,13)]
LaxeX = [Dvar['f'+str(i)]['ni'] for i in range(2,13)]
LaxeY = [Dvar['f'+str(i)]['level_w'] for i in range(2,13)]

Ltitle = ['']*len(Lplot)
Lcbarlabel = ['m/s']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lminval = [-0.034] + [-4.25]*(len(Lplot)-1)
Lmaxval = [0.034] + [4.25]*(len(Lplot)-1)
Lstep = [0.004] + [0.5]*(len(Lplot)-1)
Lstepticks = Lstep
Ltime = [Dvar['f'+str(i)]['time'] for i in range(2,13)]
Lcolormap = ['seismic']*len(Lplot)
Lfacconv = [1000]*len(Lplot)
fig3 = Panel3.psectionV(Lxx=LaxeX, Lzz=LaxeY, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                        Lstep=Lstep, Lstepticks=Lstepticks, Lcbarlabel=Lcbarlabel, Ltime=Ltime, Lcolormap=Lcolormap, Lfacconv=Lfacconv)

Panel3.save_graph(3,fig3)