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
import os

os.system('rm -f tempgraph*')
#
#  User's parameter / Namelist
#
path="../CEN4TH_LEFR/"

LnameFiles = ['FIRE_.1.CEN4T.000.nc' ]
LG_MEAN = '/LES_budgets/Mean/Cartesian/Not_time_averaged/Not_normalized/cart/'
LG_SBG = '/LES_budgets/Subgrid/Cartesian/Not_time_averaged/Not_normalized/cart/'
LG_MISC = '/LES_budgets/Miscellaneous/Cartesian/Not_time_averaged/Not_normalized/cart/'
LG_RAD = '/LES_budgets/Radiation/Cartesian/Not_time_averaged/Not_normalized/cart/'

Dvar_input = {
'f1':[(LG_MEAN,'MEAN_RC'), (LG_MEAN,'MEAN_RR'), (LG_MEAN,'MEAN_U'), (LG_MEAN,'MEAN_V'), 
      (LG_MEAN,'MEAN_W'),(LG_MEAN,'MEAN_THL'), (LG_MEAN,'MEAN_RT'), (LG_MEAN,'MEAN_MF'), 
      (LG_SBG,'SBG_WTHL'), (LG_SBG,'SBG_WRT'), 
      (LG_RAD,'SWU'), (LG_RAD,'SWD'), (LG_RAD,'LWU'),(LG_RAD,'LWD'), (LG_RAD,'DTHRADSW'),(LG_RAD,'DTHRADLW'),
      (LG_MISC,'LWP'),
      'time_les','level_les']
}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path, removeHALO=False)


################################################################
#########          PANEL 1
###############################################################
Panel1 = PanelPlot(2,3, [25,14],'', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)
Lplot = [Dvar['f1'][(LG_MEAN,'MEAN_RC')][:,180:], Dvar['f1'][(LG_MEAN,'MEAN_RR')][:,180:],Dvar['f1'][(LG_MEAN,'MEAN_U')][:,180:], 
        Dvar['f1'][(LG_MEAN,'MEAN_V')][:,180:], Dvar['f1'][(LG_MEAN,'MEAN_W')][:,180:], Dvar['f1'][(LG_MEAN,'MEAN_THL')][:,180:]]

LaxeX = [Dvar['f1']['time_les'][180:]/3600.]*len(Lplot)
LaxeZ = [Dvar['f1']['level_les']]*len(Lplot)
Ltitle = ['MEAN_RC', 'MEAN_RR','MEAN_U', 'MEAN_V','MEAN_W','MEAN_THL']
Lcbarlabel = ['g/kg', 'g/kg' ,'m/s' ,'m/s', 'm/s', 'K']
Lxlab = ['time (h)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,700)]*len(Lplot)
Lminval = [0, 0, 2, -5, -0.2E-12, 286]
Lmaxval = [0.62, 0.1, 6, 0.25, 0.2E-12, 300]
Lstep = [0.025, 0.01, 0.25, 0.25, 0.2E-13,0.5 ]
Lstepticks = Lstep
Lfacconv = [1000, 1000, 1, 1, 1, 1]
Lcolormap = ['gist_rainbow_r', 'gist_rainbow_r', 'gist_rainbow_r', 'gist_rainbow_r', 'seismic','gist_rainbow_r']
LaddWhite = [True, True, False, False, False, False]

fig1 = Panel1.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)

Panel1.save_graph(1,fig1)

################################################################
#########          PANEL 2
###############################################################
Panel2 = PanelPlot(2,3, [25,14],'', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)

Lplot = [Dvar['f1'][(LG_MEAN,'MEAN_RT')][:,180:], Dvar['f1'][(LG_MEAN,'MEAN_MF')][:,180:], Dvar['f1'][(LG_SBG,'SBG_WTHL')][:,180:],
         Dvar['f1'][(LG_SBG,'SBG_WRT')][:,180:], Dvar['f1'][(LG_RAD,'SWU')][:,180:], Dvar['f1'][(LG_RAD,'SWD')][:,180:]]
LaxeX = [Dvar['f1']['time_les'][180:]/3600.]*len(Lplot)
LaxeZ = [Dvar['f1']['level_les']]*len(Lplot)
Ltitle = ['MEAN_RT','MEAN_MF','SBG_WTHL','SBG_WRT','SWU','SWD']
Lcbarlabel = ['kg/kg', 'kg/s', 'K.m/s','K.kg/kg','W/m2','W/m2']
Lxlab = ['time (h)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,700)]*len(Lplot)
Lminval = [0.6E-2, -0.8E-4, -0.01, 0., 0., 0.]
Lmaxval = [0.0105, 0.6E-4, 0.25E-2, 0.76E-5, 500, 1100]
Lstep = [0.03E-2 , 0.05E-4, 0.05E-2, 0.04E-5, 25, 50]
Lstepticks = Lstep
Lfacconv = [1, 1, 1, 1, 1, 1]
Lcolormap = ['gist_rainbow_r']*len(Lplot)
LaddWhite = [False, False, False, True, True, True]

fig2 = Panel2.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)

Panel2.save_graph(2,fig2)

################################################################
#########          PANEL 3
###############################################################
Panel3 = PanelPlot(2,2, [20,20],'', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.01, colorbaraspect=40, labelcolorbarpad = 13)

Lplot = [Dvar['f1'][(LG_RAD,'LWU')][:,180:], Dvar['f1'][(LG_RAD,'LWD')][:,180:], Dvar['f1'][(LG_RAD,'DTHRADSW')][:,180:],
         Dvar['f1'][(LG_RAD,'DTHRADLW')][:,180:]]
LaxeX = [Dvar['f1']['time_les'][180:]/3600.]*len(Lplot)
LaxeZ = [Dvar['f1']['level_les']]*len(Lplot)
Ltitle = ['LWU','LWD','DTHRADSW','DTHRADLW']
Lcbarlabel = ['W/m2','W/m2', 'K/s','K/s']
Lxlab = ['time (h)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,700)]*len(Lplot)
Lminval = [360, 300, 0., -0.29E-2]
Lmaxval = [475, 400, 0.32E-3, 0.2E-3]
Lstep = [5,5,0.2E-4,0.02E-2]
Lstepticks = Lstep
Lfacconv = [1]*len(Lplot)
Lcolormap = ['gist_rainbow_r']*len(Lplot)
LaddWhite = [False, False, True, False]

fig3 = Panel3.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)

Panel3.save_graph(3,fig3)

################################################################
#########          PANEL 4
###############################################################
Panel4 = PanelPlot(3,1, [20,20],'')

Lplot = [ Dvar['f1'][(LG_MISC,'LWP')][180:]]
LaxeT = [Dvar['f1']['time_les'][180:]/3600.]*len(Lplot)
Ltitle = ['Liquid Water Path']*len(Lplot)
Llinelabel = ['LWP']
Lxlab = ['time (h)']*len(Lplot)
Lylab = ['LWP kg/m2']*len(Lplot)
Lylim = [(0,0.2)]*len(Lplot)
Lxlim = [(10,32)]*len(Lplot)
Llinecolor = ['r']
LaxisColor = ['black']
Ltime = [Dvar['f1']['time_les']]
fig4 = Panel4.pXY_lines(Lyy=Lplot, Lxx=LaxeT, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lylim=Lylim, Llinelabel=Llinelabel, Lxlim=Lxlim,
                                Llinecolor=Llinecolor, LaxisColor=LaxisColor, Ltime=Ltime)
Panel4.save_graph(4,fig4)
