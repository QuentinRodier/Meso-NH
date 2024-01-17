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
path=""

LnameFiles = ['ARM__.1.CEN4T.000.nc' ]
LG_MEAN = '/LES_budgets/Mean/Cartesian/Not_time_averaged/Not_normalized/cart/'
LG_SBG = '/LES_budgets/Subgrid/Cartesian/Not_time_averaged/Not_normalized/cart/'

Dvar_input = {
'f1':[(LG_MEAN,'MEAN_THL') , (LG_MEAN,'MEAN_U')  , (LG_MEAN,'MEAN_V') , (LG_MEAN,'MEAN_RC'), (LG_MEAN,'MEAN_RR'),
      (LG_SBG,'SBG_WTHL'), (LG_SBG,'SBG_WRT'),
      (LG_MEAN,'MEAN_W'), (LG_MEAN,'MEAN_RT') , (LG_MEAN,'MEAN_MF'), (LG_MEAN,'MEAN_SV'), 
      'level_les','time_les']}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path, removeHALO=False)

################################################################
#########          PANEL 
###############################################################
Panel = PanelPlot(2,3, [25,14],'(cart variables)', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)
Lplot = [Dvar['f1'][(LG_MEAN,'MEAN_RC')],Dvar['f1'][(LG_MEAN,'MEAN_RR')], Dvar['f1'][(LG_MEAN,'MEAN_U')], Dvar['f1'][(LG_MEAN,'MEAN_V')],
        Dvar['f1'][(LG_MEAN,'MEAN_W')], Dvar['f1'][(LG_MEAN,'MEAN_THL')]]
LaxeX = [Dvar['f1']['time_les']/3600.]*len(Lplot)
LaxeZ = [Dvar['f1']['level_les']]*len(Lplot)
Ltitle = ['MEAN_RC', 'MEAN_RR','MEAN_U','MEAN_V','MEAN_W','MEAN_THL']
Lcbarlabel = ['g/kg', 'g/kg','m/s','m/s','m/s','K']
Lxlab = ['time (h)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,3000)]*len(Lplot)
Lminval = [-0.009, -0.0009, 3, 0, 0, 300]
Lmaxval = [0.2, 0.02, 12, 3.4, 1, 320]
Lstep = [0.01, 0.001,1.0, 0.2,0.1, 0.5]
Lstepticks = Lstep
Lfacconv = [1000, 1000, 1, 1, 1, 1]
Lcolormap = ['gist_rainbow_r']*len(Lplot)
LaddWhite = [True, True, False, False, True, False]

fig = Panel.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)
Panel.save_graph(1,fig)

################################################################
#########          PANEL
###############################################################
Panel = PanelPlot(2,2, [20,20],'(cart variables)', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)
Lplot = [Dvar['f1'][(LG_MEAN,'MEAN_RT')],Dvar['f1'][(LG_MEAN,'MEAN_MF')], Dvar['f1'][(LG_SBG,'SBG_WTHL')], Dvar['f1'][(LG_SBG,'SBG_WRT')]]
LaxeX = [Dvar['f1']['time_les']/3600.]*len(Lplot)
LaxeZ = [Dvar['f1']['level_les']]*len(Lplot)
Ltitle = ['MEAN_RT', 'MEAN_MF','SBG_WTHL','SBG_WRT']
Lcbarlabel = ['kg/kg','kg/s','K.m/s','m/s kg/kg']
Lxlab = ['time (h)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,3000)]*len(Lplot)
Lminval = [0.35E-2, 0., -0.04, -0.1E-4]
Lmaxval = [0.0175,1, 0.115, 0.17E-3]
Lstep = [0.05E-2,0.1, 0.005, 0.01E-3]
Lstepticks = Lstep
Lfacconv = [1, 1, 1, 1]
Lcolormap = ['gist_rainbow_r']*len(Lplot)
LaddWhite = [False]*len(Lplot)

fig = Panel.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv,
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)
Panel.save_graph(2,fig)

################################################################
#########          PANEL
###############################################################
Panel = PanelPlot(2,3, [25,14],'(cart variables)', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)
Lplot = [Dvar['f1'][(LG_MEAN,'MEAN_SV')][:,:,0], Dvar['f1'][(LG_MEAN,'MEAN_SV')][:,:,1], Dvar['f1'][(LG_MEAN,'MEAN_SV')][:,:,2]]
LaxeX = [Dvar['f1']['time_les']/3600.]*len(Lplot)
LaxeZ = [Dvar['f1']['level_les']]*len(Lplot)
Ltitle = ['MEAN_SV 1', 'MEAN_SV 2','MEAN_SV 3']
Lcbarlabel = ['kg/kg']*len(Lplot)
Lxlab = ['time (h)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,3000)]*len(Lplot)
Lminval = [0]*len(Lplot)
Lmaxval = [11]*len(Lplot)
Lstep = [0.25]*len(Lplot)
Lstepticks = [0.5]*len(Lplot)
Lfacconv = [1, 1, 1]
Lcolormap = ['gist_rainbow_r']*len(Lplot)
LaddWhite = [True]*len(Lplot)

fig = Panel.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv,
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)
Panel.save_graph(3,fig)
