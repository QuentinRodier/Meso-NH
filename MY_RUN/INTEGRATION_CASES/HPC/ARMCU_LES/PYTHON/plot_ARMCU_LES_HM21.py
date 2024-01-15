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
path="../HM21/NETCDF/"

LnameFiles = ['ARM__.1.CEN4T.000.nc' ]

LG_CART = '/LES_budgets/Mean/Cartesian/Not_time_averaged/Not_normalized/cart/'
LG_NEB = '/LES_budgets/Mean/Cartesian/Not_time_averaged/Not_normalized/neb/'
LG_CLEAR = '/LES_budgets/Mean/Cartesian/Not_time_averaged/Not_normalized/clear/'
LG_CS1 = '/LES_budgets/Mean/Cartesian/Not_time_averaged/Not_normalized/cs1/'
LG_CS2 = '/LES_budgets/Mean/Cartesian/Not_time_averaged/Not_normalized/cs2/'
LG_CS3 = '/LES_budgets/Mean/Cartesian/Not_time_averaged/Not_normalized/cs3/'
LG_SBGCART = '/LES_budgets/Subgrid/Cartesian/Not_time_averaged/Not_normalized/cart/'
LG_SBGNEB = '/LES_budgets/Subgrid/Cartesian/Not_time_averaged/Not_normalized/neb/'
LG_SBGCLEAR = '/LES_budgets/Subgrid/Cartesian/Not_time_averaged/Not_normalized/clear/'
LG_SBGCS1 = '/LES_budgets/Subgrid/Cartesian/Not_time_averaged/Not_normalized/cs1/'
LG_SBGCS2 = '/LES_budgets/Subgrid/Cartesian/Not_time_averaged/Not_normalized/cs2/'
LG_SBGCS3 = '/LES_budgets/Subgrid/Cartesian/Not_time_averaged/Not_normalized/cs3/'

Dvar_input = {'f1':['time_les','level_les',
(LG_CART, 'MEAN_RC'),  (LG_NEB, 'MEAN_RC'),  (LG_CLEAR, 'MEAN_RC'),  (LG_CS1, 'MEAN_RC'),  (LG_CS2, 'MEAN_RC'),  (LG_CS3, 'MEAN_RC'),
(LG_CART, 'MEAN_RR'),  (LG_NEB, 'MEAN_RR'),  (LG_CLEAR, 'MEAN_RR'),  (LG_CS1, 'MEAN_RR'),  (LG_CS2, 'MEAN_RR'),  (LG_CS3, 'MEAN_RR'),
(LG_CART, 'MEAN_U'),   (LG_NEB, 'MEAN_U'),   (LG_CLEAR, 'MEAN_U'),   (LG_CS1, 'MEAN_U'),   (LG_CS2, 'MEAN_U'),   (LG_CS3, 'MEAN_U'),
(LG_CART, 'MEAN_V'),   (LG_NEB, 'MEAN_V'),   (LG_CLEAR, 'MEAN_V'),   (LG_CS1, 'MEAN_V'),   (LG_CS2, 'MEAN_V'),   (LG_CS3, 'MEAN_V'),
(LG_CART, 'MEAN_W'),   (LG_NEB, 'MEAN_W'),   (LG_CLEAR, 'MEAN_W'),   (LG_CS1, 'MEAN_W'),   (LG_CS2, 'MEAN_W'),   (LG_CS3, 'MEAN_W'),
(LG_CART, 'MEAN_THL'), (LG_NEB, 'MEAN_THL'), (LG_CLEAR, 'MEAN_THL'), (LG_CS1, 'MEAN_THL'), (LG_CS2, 'MEAN_THL'), (LG_CS3, 'MEAN_THL'),
(LG_CART, 'MEAN_RT'),  (LG_NEB, 'MEAN_RT'),  (LG_CLEAR, 'MEAN_RT'),  (LG_CS1, 'MEAN_RT'),  (LG_CS2, 'MEAN_RT'),  (LG_CS3, 'MEAN_RT'),
(LG_CART, 'MEAN_MF'),  (LG_NEB, 'MEAN_MF'),  (LG_CLEAR, 'MEAN_MF'),  (LG_CS1, 'MEAN_MF'),  (LG_CS2, 'MEAN_MF'),  (LG_CS3, 'MEAN_MF'),
(LG_CART, 'MEAN_SV'),  (LG_NEB, 'MEAN_SV'),  (LG_CLEAR, 'MEAN_SV'),  (LG_CS1, 'MEAN_SV'),  (LG_CS2, 'MEAN_SV'),  (LG_CS3, 'MEAN_SV'),
(LG_SBGCART, 'SBG_WTHL'), (LG_SBGNEB, 'SBG_WTHL'), (LG_SBGCLEAR, 'SBG_WTHL'), (LG_SBGCS1, 'SBG_WTHL'), (LG_SBGCS2, 'SBG_WTHL'), (LG_SBGCS3, 'SBG_WTHL'),
(LG_SBGCART, 'SBG_WRT'),  (LG_SBGNEB, 'SBG_WRT'),  (LG_SBGCLEAR, 'SBG_WRT'),  (LG_SBGCS1, 'SBG_WRT'),  (LG_SBGCS2, 'SBG_WRT'),  (LG_SBGCS3, 'SBG_WRT')]
}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path, removeHALO=False)

################################################################
#########          PANEL  1
###############################################################
Panel = PanelPlot(2,3, [25,14],'MEAN_RC', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)
var='MEAN_RC'
Lplot = [Dvar['f1'][(LG_CART,var)],Dvar['f1'][(LG_NEB,var)],Dvar['f1'][(LG_CLEAR,var)],
         Dvar['f1'][(LG_CS1,var)],Dvar['f1'][(LG_CS2,var)],Dvar['f1'][(LG_CS3,var)]]

LaxeX = [Dvar['f1']['time_les']/3600.]*len(Lplot)
LaxeZ = [Dvar['f1']['level_les']]*len(Lplot)
Ltitle = ['cart','neb','clear','cs1','cs2','cs3']
Lcbarlabel = ['g/kg']*len(Lplot)
Lxlab = ['time (h)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,3000)]*len(Lplot)
Lminval = [-0.009]*len(Lplot)
Lmaxval = [0.2]*len(Lplot)
Lstep = [0.01]*len(Lplot)
Lstepticks = Lstep
Lfacconv = [1000]*len(Lplot)
Lcolormap = ['gist_rainbow_r']*len(Lplot)
LaddWhite = [True]*len(Lplot)

fig = Panel.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)
Panel.save_graph(1,fig)
################################################################
#########          PANEL 2
###############################################################
Panel = PanelPlot(2,3, [25,14],'MEAN_RR', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)
var='MEAN_RR'
Lplot = [Dvar['f1'][(LG_CART,var)],Dvar['f1'][(LG_NEB,var)],Dvar['f1'][(LG_CLEAR,var)],
         Dvar['f1'][(LG_CS1,var)],Dvar['f1'][(LG_CS2,var)],Dvar['f1'][(LG_CS3,var)]]

Lcbarlabel = ['g/kg']*len(Lplot)
Lminval = [-0.0009]*len(Lplot)
Lmaxval = [0.02]*len(Lplot)
Lstep = [0.001]*len(Lplot)
Lstepticks = Lstep
Lfacconv = [1000]*len(Lplot)
LaddWhite = [True]*len(Lplot)

fig = Panel.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)
Panel.save_graph(2,fig)
################################################################
#########          PANEL 3
###############################################################
Panel = PanelPlot(2,3, [25,14],'MEAN_U', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)
var='MEAN_U'
Lplot = [Dvar['f1'][(LG_CART,var)],Dvar['f1'][(LG_NEB,var)],Dvar['f1'][(LG_CLEAR,var)],
         Dvar['f1'][(LG_CS1,var)],Dvar['f1'][(LG_CS2,var)],Dvar['f1'][(LG_CS3,var)]]

Lcbarlabel = ['m/s']*len(Lplot)
Lminval = [3]*len(Lplot)
Lmaxval = [11]*len(Lplot)
Lstep = [0.5]*len(Lplot)
Lstepticks = Lstep
Lfacconv = [1]*len(Lplot)
LaddWhite = [False]*len(Lplot)

fig = Panel.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)
Panel.save_graph(3,fig)
################################################################
#########          PANEL 4
###############################################################
Panel = PanelPlot(2,3, [25,14],'MEAN_V', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)
var='MEAN_V'
Lplot = [Dvar['f1'][(LG_CART,var)],Dvar['f1'][(LG_NEB,var)],Dvar['f1'][(LG_CLEAR,var)],
         Dvar['f1'][(LG_CS1,var)],Dvar['f1'][(LG_CS2,var)],Dvar['f1'][(LG_CS3,var)]]

Lcbarlabel = ['m/s']*len(Lplot)
Lminval = [0]*len(Lplot)
Lmaxval = [3.2]*len(Lplot)
Lstep = [0.2]*len(Lplot)
Lstepticks = Lstep
Lfacconv = [1]*len(Lplot)
LaddWhite = [False]*len(Lplot)

fig = Panel.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)
Panel.save_graph(4,fig)
################################################################
#########          PANEL 5
###############################################################
Panel = PanelPlot(2,3, [25,14],'MEAN_W', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)
var='MEAN_W'
Lplot = [Dvar['f1'][(LG_CART,var)],Dvar['f1'][(LG_NEB,var)],Dvar['f1'][(LG_CLEAR,var)],
         Dvar['f1'][(LG_CS1,var)],Dvar['f1'][(LG_CS2,var)],Dvar['f1'][(LG_CS3,var)]]

Lcbarlabel = ['m/s']*len(Lplot)
Lminval = [0., 0., -0.18, 0., -1.25, -1.2]
Lmaxval = [4.25, 4.25, 0., 5.25, 2.75, 0.8]
Lstep = [0.25, 0.25, 0.01, 0.25, 0.25, 0.1]
Lstepticks = Lstep
Lfacconv = [1]*len(Lplot)
LaddWhite = [False]*len(Lplot)

fig = Panel.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)
Panel.save_graph(5,fig)
################################################################
#########          PANEL 6
###############################################################
Panel = PanelPlot(2,3, [25,14],'MEAN_THL', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)
var='MEAN_THL'
Lplot = [Dvar['f1'][(LG_CART,var)],Dvar['f1'][(LG_NEB,var)],Dvar['f1'][(LG_CLEAR,var)],
         Dvar['f1'][(LG_CS1,var)],Dvar['f1'][(LG_CS2,var)],Dvar['f1'][(LG_CS3,var)]]

Lcbarlabel = ['K']*len(Lplot)
Lminval = [300]*len(Lplot)
Lmaxval = [320]*len(Lplot)
Lstep = [0.5]*len(Lplot)
Lstepticks = Lstep
Lfacconv = [1]*len(Lplot)
LaddWhite = [False]*len(Lplot)

fig = Panel.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)
Panel.save_graph(6,fig)
################################################################
#########          PANEL 7
###############################################################
Panel = PanelPlot(2,3, [25,14],'MEAN_SV 1', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)
var='MEAN_SV'
Lplot = [Dvar['f1'][(LG_CART,var)][:,:,0],Dvar['f1'][(LG_NEB,var)][:,:,0],Dvar['f1'][(LG_CLEAR,var)][:,:,0],
         Dvar['f1'][(LG_CS1,var)][:,:,0] ,Dvar['f1'][(LG_CS2,var)][:,:,0],Dvar['f1'][(LG_CS3,var)][:,:,0]]

Lcbarlabel = ['kg/kg']*len(Lplot)
Lminval = [2.5,9, 2.5, 2.5, 2, 2 ]
Lmaxval = [50, 35, 50, 50, 34, 34]
Lstep = [2.5, 1, 2.5, 2.5, 2, 2]
Lstepticks = Lstep
Lfacconv = [1]*len(Lplot)
LaddWhite = [False]*len(Lplot)

fig = Panel.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)
Panel.save_graph(7,fig)
################################################################
#########          PANEL 8
###############################################################
Panel = PanelPlot(2,3, [25,14],'MEAN_SV 2', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)
var='MEAN_SV'
Lplot = [Dvar['f1'][(LG_CART,var)][:,:,1],Dvar['f1'][(LG_NEB,var)][:,:,1],Dvar['f1'][(LG_CLEAR,var)][:,:,1],
         Dvar['f1'][(LG_CS1,var)][:,:,1] ,Dvar['f1'][(LG_CS2,var)][:,:,1],Dvar['f1'][(LG_CS3,var)][:,:,1]]

Lminval = [0.1, 0.3, 0.1, 0.1, 0.1, 0.1 ]
Lmaxval = [2.1, 1.6, 2.1, 1.7, 2.1, 1.75]
Lstep = [0.1, 0.05, 0.1, 0.1, 0.1, 0.05]
Lstepticks = Lstep
Lcolormap = ['gist_rainbow_r']*len(Lplot)
fig = Panel.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)
Panel.save_graph(8,fig)
################################################################
#########          PANEL 9
###############################################################
Panel = PanelPlot(2,3, [25,14],'MEAN_SV 3', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)
var='MEAN_SV'
Lplot = [Dvar['f1'][(LG_CART,var)][:,:,2],Dvar['f1'][(LG_NEB,var)][:,:,2],Dvar['f1'][(LG_CLEAR,var)][:,:,2],
         Dvar['f1'][(LG_CS1,var)][:,:,2] ,Dvar['f1'][(LG_CS2,var)][:,:,2],Dvar['f1'][(LG_CS3,var)][:,:,2]]

Lminval = [0.1,0.05, 0.1, 0.05, 0.1, 0.1 ]
Lmaxval = [2.3, 1.05, 2.3, 1.05, 3.5, 3.1]
Lstep = [0.1, 0.05, 0.1, 0.05, 0.1, 0.1]
Lstepticks = Lstep
fig = Panel.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)
Panel.save_graph(9,fig)
################################################################
#########          PANEL 10
###############################################################
Panel = PanelPlot(2,3, [25,14],'MEAN_RT', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)
var='MEAN_RT'
Lplot = [Dvar['f1'][(LG_CART,var)],Dvar['f1'][(LG_NEB,var)],Dvar['f1'][(LG_CLEAR,var)],
         Dvar['f1'][(LG_CS1,var)],Dvar['f1'][(LG_CS2,var)],Dvar['f1'][(LG_CS3,var)]]

Lcbarlabel = ['kg/kg']*len(Lplot)
Lminval = [0.01]*len(Lplot)
Lmaxval = [0.019]*len(Lplot)
Lstep = [0.001]*len(Lplot)
Lstepticks = Lstep
Lfacconv = [1]*len(Lplot)
LaddWhite = [False]*len(Lplot)

fig = Panel.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)
Panel.save_graph(10,fig)
################################################################
#########          PANEL 11
###############################################################
Panel = PanelPlot(2,3, [25,14],'MEAN_MF', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)
var='MEAN_MF'
Lplot = [Dvar['f1'][(LG_CART,var)],Dvar['f1'][(LG_NEB,var)],Dvar['f1'][(LG_CLEAR,var)],
         Dvar['f1'][(LG_CS1,var)],Dvar['f1'][(LG_CS2,var)],Dvar['f1'][(LG_CS3,var)]]

Lcbarlabel = ['kg/s']*len(Lplot)
Lminval = [-0.5E-3, 0.2, -0.18, 0.2, -1.2, -1.2]
Lmaxval = [0.2E-3, 4.2, 0.,4.2, 2.5, 1.1 ]
Lstep = [0.05E-3, 0.2, 0.01, 0.2, 0.1, 0.1]
Lstepticks = Lstep
Lfacconv = [1]*len(Lplot)
LaddWhite = [False]*len(Lplot)

fig = Panel.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)
Panel.save_graph(11,fig)
################################################################
#########          PANEL 12
###############################################################
Panel = PanelPlot(2,3, [25,14],'SBG_WTHL', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)
var='SBG_WTHL'
Lplot = [Dvar['f1'][(LG_SBGCART,var)],Dvar['f1'][(LG_SBGNEB,var)],Dvar['f1'][(LG_SBGCLEAR,var)],
         Dvar['f1'][(LG_SBGCS1,var)],Dvar['f1'][(LG_SBGCS2,var)],Dvar['f1'][(LG_SBGCS3,var)]]

Lcbarlabel = ['g/kg']*len(Lplot)
Lminval = [-0.035,-0.8, -0.035, -0.8, -0.4, -0.04 ]
Lmaxval = [0.105, 0., 0.105, 0., 0.05, 0.085]
Lstep = [0.005, 0.05, 0.005, 0.05, 0.005, 0.005]
Lstepticks = Lstep
Lfacconv = [1]*len(Lplot)
LaddWhite = [False]*len(Lplot)

fig = Panel.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)
Panel.save_graph(12,fig)
################################################################
#########          PANEL 13
###############################################################
Panel = PanelPlot(2,3, [25,14],'SBG_WRT', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)
var='SBG_WRT'
Lplot = [Dvar['f1'][(LG_SBGCART,var)],Dvar['f1'][(LG_SBGNEB,var)],Dvar['f1'][(LG_SBGCLEAR,var)],
         Dvar['f1'][(LG_SBGCS1,var)],Dvar['f1'][(LG_SBGCS2,var)],Dvar['f1'][(LG_SBGCS3,var)]]

Lcbarlabel = ['m/s kg/kg']*len(Lplot)
Lminval = [0.]*len(Lplot)
Lmaxval = [0.15E-3, 0.16E-2, 0.15E-3, 0.16E-2, 0.85E-3, 0.105E-3]
Lstep = [0.1E-4, 0.05E-3, 0.1E-4,0.05E-3, 0.05E-3, 0.05E-4 ]
Lstepticks = Lstep
Lfacconv = [1]*len(Lplot)
LaddWhite = [True]*len(Lplot)

fig = Panel.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)
Panel.save_graph(13,fig)
