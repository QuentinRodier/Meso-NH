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
output_name = '005_ARM.pdf'                 #Name of the output PDF file
path=""

LnameFiles = ['ARM__.1.CEN4T.000.nc' ]

Dvar_input = {
'f1':['MEAN_TH','MEAN_U','MEAN_V','MEAN_RC','MEAN_RR',
      'SBG_TKE','SBG_WTHL','SBG_WRT',
      'THLUP_MF','RTUP_MF','RVUP_MF','RCUP_MF','RIUP_MF','WUP_MF',
      'MAFLX_MF','DETR_MF','ENTR_MF','FRCUP_MF','THVUP_MF','WTHL_MF',
      'WRT_MF','WTHV_MF','WU_MF','WV_MF',
      'level_les','time_les']
}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path, removeHALO=False)


################################################################
#########          PANEL 1
###############################################################
Panel1 = PanelPlot(2,3, [25,14],'', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)

Lplot = [Dvar['f1']['MEAN_TH'],Dvar['f1']['MEAN_U'], Dvar['f1']['MEAN_V'], Dvar['f1']['MEAN_RC'], Dvar['f1']['MEAN_RR'],Dvar['f1']['SBG_TKE'],]
LaxeX = [Dvar['f1']['time_les']/3600.]*len(Lplot)
LaxeZ = [Dvar['f1']['level_les']]*len(Lplot)
Ltitle = ['Mean potential temperature TH', 'Mean U', 'Mean V', 'Mean cloud mixing ratio RC', 'Mean precipitation RR', 'Subgrid TKE']
Lcbarlabel = ['K','m/s','m/s','g/kg','g/kg','m2 s-2']
Lxlab = ['time (h)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,3000)]*len(Lplot)
Lminval = [298, 3, 0, 0, 0, 0]
Lmaxval = [317, 12, 3.4,0.21, 0.021, 2.5]
Lstep = [1, 0.5, 0.2, 0.01,0.001, 0.25]
Lstepticks = Lstep
Lfacconv = [1, 1, 1, 1000, 1000, 1]
Lcolormap = ['gist_rainbow_r']*len(Lplot)
LaddWhite = [False, False, True, True,True, True]

fig1 = Panel1.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)

Panel1.save_graph(1,fig1)

################################################################
#########          PANEL 2
###############################################################
Panel2 = PanelPlot(2,3, [25,14],'', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)

Lplot = [Dvar['f1']['SBG_WTHL'], Dvar['f1']['SBG_WRT'], Dvar['f1']['THLUP_MF'], Dvar['f1']['RTUP_MF'], Dvar['f1']['RVUP_MF'], Dvar['f1']['RCUP_MF']]
LaxeX = [Dvar['f1']['time_les']/3600.]*len(Lplot)
LaxeZ = [Dvar['f1']['level_les']]*len(Lplot)
Ltitle = ['Subgrid vertical liquid potential temp. flux', 'Subgrid vertical RT flux', 
          'Updraft THL', 'Updraft total mixing ratio', 'Updraft water vapor mixing ratio', 'Updraft cloud mixing ratio']
Lcbarlabel = ['m K s-1','m kg kg-1 s-1','K','kg/kg', 'kg/kg', 'kg/kg']
Lxlab = ['time (h)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,3000)]*len(Lplot)
Lminval = [ -0.3, -0.1E-3, 301,0.35E-2, 0.1E-2, 0.25E-3]
Lmaxval = [0.14, 0.45E-3, 319 ,0.018, 0.018, 0.425E-2]
Lstep = [ 0.02, 0.05E-3, 1 ,0.001, 0.001, 0.25E-3]
Lstepticks = Lstep
Lfacconv = [1]*len(Lplot)
Lcolormap = ['gist_rainbow_r']*len(Lplot)
LaddWhite = [False, False, False, False, False, False]

fig2 = Panel2.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)

Panel2.save_graph(2,fig2)

################################################################
#########          PANEL 3
###############################################################
Panel3 = PanelPlot(2,3, [25,14],'', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)

Lplot = [Dvar['f1']['RIUP_MF'], Dvar['f1']['WUP_MF'], Dvar['f1']['MAFLX_MF'], Dvar['f1']['DETR_MF'], Dvar['f1']['ENTR_MF'], Dvar['f1']['FRCUP_MF']]
LaxeX = [Dvar['f1']['time_les']/3600.]*len(Lplot)
LaxeZ = [Dvar['f1']['level_les']]*len(Lplot)
Ltitle = ['Updraft ice mixing ratio', 'Updraft vertical velocity', 
          'Updraft mass-flux', 'Updraft detrainment', 'Updraft entrainement', 'Subgrid updraft fraction']
Lcbarlabel = ['g/kg','m/s','kg m-2 s-1','kg m-3 s-1', 'kg m-3 s-1', '-']
Lxlab = ['time (h)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,3000)]*len(Lplot)
Lminval = [ 0, 0., 0., 0., 0., 0.]
Lmaxval = [0.001, 5.75, 0.34, 0.55, 0.52E-2, 0.17]
Lstep = [ 0.0001, 0.25, 0.025, 0.025, 0.25E-3, 0.01]
Lstepticks = Lstep
Lfacconv = [1]*len(Lplot)
Lcolormap = ['gist_rainbow_r']*len(Lplot)
LaddWhite = [True]*len(Lplot)

fig3 = Panel3.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)

Panel3.save_graph(3,fig3)


################################################################
#########          PANEL 4
###############################################################
Panel4 = PanelPlot(2,3, [25,14],'', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)

Lplot = [Dvar['f1']['THVUP_MF'], Dvar['f1']['WTHL_MF'], Dvar['f1']['WRT_MF'], Dvar['f1']['WTHV_MF'], Dvar['f1']['WU_MF'], Dvar['f1']['WV_MF']]
LaxeX = [Dvar['f1']['time_les']/3600.]*len(Lplot)
LaxeZ = [Dvar['f1']['level_les']]*len(Lplot)
Ltitle = ['Updraft virtual potential temperature', 'Subgrid WTHL flux from Mass-Flux scheme', 
          'Subgrid WRT flux from Mass-Flux scheme', 'Subgrid WTHV flux from Mass-Flux scheme', 'Subgrid WU from Mass-Flux scheme', 'Subgrid WV from Mass-Flux scheme']
Lcbarlabel = ['K','m K s-1','m kg/kg s-1','m K s-1', 'm2 s-2', 'm2 s-2']
Lxlab = ['time (h)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,3000)]*len(Lplot)
Lminval = [303, -0.07, 0., -0.015, -0.12, -0.024   ]
Lmaxval = [320, 0.06, 0.19E-3, 0.08, 0., 0.052 ]
Lstep = [1, 0.005,0.1E-4, 0.5E-2,0.005,0.4E-2    ]
Lstepticks = Lstep
Lfacconv = [1]*len(Lplot)
Lcolormap = ['gist_rainbow_r']*len(Lplot)
LaddWhite = [False, False, True, False, False, False]

fig4 = Panel4.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)

Panel4.save_graph(4,fig4)
