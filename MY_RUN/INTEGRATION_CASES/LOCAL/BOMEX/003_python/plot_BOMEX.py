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
output_name = 'BOMEX.pdf'                 #Name of the output PDF file
path=""

LnameFiles = ['BOMEX.1.CEN4T.000.nc' ]
LG_MEAN = '/LES_budgets/Mean/Cartesian/Not_time_averaged/Not_normalized/cart/'
LG_SBG = '/LES_budgets/Subgrid/Cartesian/Not_time_averaged/Not_normalized/cart/'

Dvar_input = {
'f1':[(LG_MEAN,'MEAN_TH') , (LG_MEAN,'MEAN_RC'), (LG_MEAN,'MEAN_RR'),
      (LG_SBG,'SBG_TKE') , (LG_SBG,'SBG_WTHL'), (LG_SBG,'RCUP_MF'),(LG_SBG,'SBG_WRT'),
      'level_les','time_les']
}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path, removeHALO=False)

################################################################
#########          PANEL 1
###############################################################
Panel1 = PanelPlot(2,2, [20,20],'', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.01, colorbaraspect=40, labelcolorbarpad = 13)

Lplot = [Dvar['f1'][(LG_SBG,'SBG_TKE')], Dvar['f1'][(LG_MEAN,'MEAN_TH')],Dvar['f1'][(LG_MEAN,'MEAN_RC')], Dvar['f1'][(LG_MEAN,'MEAN_RR')] ]
LaxeX = [Dvar['f1']['time_les']/3600.]*len(Lplot)
LaxeZ = [Dvar['f1']['level_les']]*len(Lplot)
Ltitle = ['Subgrid TKE', 'Mean potential temperature TH', 'Mean cloud mixing ratio RC', 'Mean precipitation RR']
Lcbarlabel = ['m2 s-2','K','g/kg','g/kg']
Lxlab = ['time (h)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,3000)]*len(Lplot)
Lminval = [0, 298, 0, 0, 0, 0]
Lmaxval = [0.3, 313, 0.18E-1, 0.21E-1]
Lstep = [0.025, 1,  0.01E-1 , 0.01E-1 ]
Lstepticks = Lstep
Lfacconv = [1, 1, 1000, 1000]
Lcolormap = ['gist_rainbow_r']*len(Lplot)
LaddWhite = [True, False, True, True]

fig1 = Panel1.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)
Panel1.save_graph(1,fig1)

################################################################
#########          PANEL 2
###############################################################
Panel2 = PanelPlot(2,2, [20,20],'', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.01, colorbaraspect=40, labelcolorbarpad = 13)

Lplot = [Dvar['f1'][(LG_SBG,'SBG_WTHL')], Dvar['f1'][(LG_SBG,'SBG_WRT')], Dvar['f1'][(LG_SBG,'RCUP_MF')]]
LaxeX = [Dvar['f1']['time_les']/3600.]*len(Lplot)
LaxeZ = [Dvar['f1']['level_les']]*len(Lplot)
Ltitle = ['Subgrid vertical liquid potential temp. flux', 'Subgrid vertical RT flux',  'Updraft cloud mixing ratio']
Lcbarlabel = ['m K s-1','m kg kg-1 s-1', 'kg/kg']
Lxlab = ['time (h)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,3000)]*len(Lplot)
Lminval = [ -0.025, -0.5E-4, 0.]
Lmaxval = [0.025, 0.21E-3, 0.32E-2]
Lstep = [ 0.0025, 0.1E-4, 0.01E-2]
Lstepticks = Lstep
Lfacconv = [1]*len(Lplot)
Lcolormap = ['seismic','gist_rainbow_r','gist_rainbow_r']
LaddWhite = [False, False, True]

fig2 = Panel2.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)
Panel2.save_graph(2,fig2)
