#!/usr/bin/env python3
"""

@author: Quentin Rodier
Creation : 20/01/2023

Last modifications
"""

import matplotlib as mpl
mpl.use('Agg')
from read_MNHfile import read_netcdf
from Panel_Plot import PanelPlot
import math
import os

os.system('rm -f tempgraph*')
#
#  User's parameter / Namelist
#
path="../02_mesonh/"

LnameFiles = ['BLAZE.1.TEST4.002.nc']

Dvar_input = {'f1':['UT','VT', 'WT','THT','ni','nj','level',
      'FMROS','FMASE','FMAWC','FMFLUXHDH','FMFLUXHDW','FMWINDU','FMWINDV','FMWINDW',
      'FMBMAP']}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path, removeHALO=True)

################################################################
#########          PANEL 1
###############################################################
Panel1 = PanelPlot(2,2, [25,15],'K=1', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.01)

Lplot = [ Dvar['f1']['UT'], Dvar['f1']['VT'], Dvar['f1']['WT'],Dvar['f1']['THT']]

LaxeX = [Dvar['f1']['ni']]*len(Lplot)
LaxeY = [Dvar['f1']['nj']]*len(Lplot)
Ltitle = ['u', 'v','w','théta',]
Lcbarlabel = ['m/s','m/s','m/s','K']
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['y (m)']*len(Lplot)
Lminval = [-1.5, -3.5, -1.5, 286]
Lmaxval = [3.5,3.5,1.5,300]
Lstep = [0.1,0.1,0.1,0.25]
Lstepticks = [0.5,0.5,0.5,1]
Lfacconv = [1]*len(Lplot)
Lcolormap = ['gist_rainbow_r','RdBu_r','RdBu_r','gist_rainbow_r']*len(Lplot)
Llvl = [1]*len(Lplot)
Ltime = [Dvar['f1']['time']]*len(Lplot)
LaddWhite = [False,False,False,True]*len(Lplot)
Lpltype = ['cf']*len(Lplot)

fig1 = Panel1.psectionH(lon=LaxeX, lat=LaxeY, Lvar=Lplot, Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                Ltime=Ltime, LaddWhite_cm=LaddWhite)
Panel1.save_graph(1,fig1)

################################################################
#########          PANEL 2
###############################################################

Panel1 = PanelPlot(2,2, [25,15],'K=28', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.01)

Llvl = [28]*len(Lplot)
Lminval = [-2, -2., -1.5, 285]
Lmaxval = [4,2.,4,288]
Lstep = [0.1,0.1,0.1,0.05]

fig2 = Panel1.psectionH(lon=LaxeX, lat=LaxeY, Lvar=Lplot, Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                Ltime=Ltime, LaddWhite_cm=LaddWhite)
Panel1.save_graph(2,fig2)

################################################################
#########          PANEL 3
###############################################################

Panel1 = PanelPlot(3,3, [25,15],'', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.01)
Lplot = [ Dvar['f1']['FMROS'], Dvar['f1']['FMASE'], Dvar['f1']['FMAWC'],Dvar['f1']['FMFLUXHDH'],Dvar['f1']['FMFLUXHDW'],
        Dvar['f1']['FMWINDU'],Dvar['f1']['FMWINDV'],Dvar['f1']['FMWINDW'],Dvar['f1']['FMBMAP']]

LaxeX = [Dvar['f1']['ni']]*len(Lplot)
LaxeY = [Dvar['f1']['nj']]*len(Lplot)
Ltitle = [ 'FIRERW','FMASE available H','FMAWC available rl','FMFLUXHDH','FMFLUXHDW','FMWINDU','FMWINDV','FMWINDW','BMAP']
Lcbarlabel = ['m s-1','kJ m-2','kg m-2','W m-2','kg m-2 s-1','m s-1','m s-1','m s-1','s']
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['y (m)']*len(Lplot)
Lminval = [0,0,0,0,0,0,-3.5,-0.4,-1]
Lmaxval = [1,5e5,0.2,3e4,0.015,4,3.5,0.4,600]
Lstep = [0.02,1e4,0.002,5e2,0.0002,0.02,0.02,0.002,10]
Lstepticks = [0.1,1e5,0.05,5e4,0.001,0.5,0.5,0.1,50]
Lfacconv = [1]*len(Lplot)
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Llvl = [1]*len(Lplot)
Ltime = [Dvar['f1']['time']]*len(Lplot)
LaddWhite = [False,False,False,True,True,False,False,False,True]
Lpltype = ['cf']*len(Lplot)

fig3 = Panel1.psectionH(lon=LaxeX, lat=LaxeY, Lvar=Lplot, Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                Ltime=Ltime, LaddWhite_cm=LaddWhite)

fig3.tight_layout()
Panel1.save_graph(3,fig3)
