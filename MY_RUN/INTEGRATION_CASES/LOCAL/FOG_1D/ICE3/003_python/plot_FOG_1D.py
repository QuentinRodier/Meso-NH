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

LnameFiles = ['FOG00.1.CEN4T.000.nc' ]
LG_MEAN = '/LES_budgets/Mean/Cartesian/Not_time_averaged/Not_normalized/cart/'

Dvar_input = {
'f1':[(LG_MEAN,'MEAN_TH') , (LG_MEAN,'MEAN_RC'), (LG_MEAN,'MEAN_RR'), (LG_MEAN, 'MEAN_RV'),'level_les','time_les']
}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path, removeHALO=False)

################################################################
#########          PANEL 1
###############################################################
Panel1 = PanelPlot(2,2, [20,20],'', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.01, colorbaraspect=40, labelcolorbarpad = 13)

Lplot = [Dvar['f1'][ (LG_MEAN,'MEAN_RC')], Dvar['f1'][ (LG_MEAN,'MEAN_RV')],Dvar['f1'][ (LG_MEAN,'MEAN_TH')], Dvar['f1'][ (LG_MEAN,'MEAN_RR')] ]
LaxeX = [Dvar['f1']['time_les']/3600.]*len(Lplot)
LaxeZ = [Dvar['f1']['level_les']]*len(Lplot)
Ltitle = ['Mean cloud mixing ratio RC', 'MEAN wator vapor mixing ratio RV','Mean potential temperature TH', 'Mean precipitation RR']
Lcbarlabel = ['g/kg','g/kg','K','g/kg']
Lxlab = ['time (h)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,600)]*len(Lplot)
Lminval = [0, 7, 285, 0]
Lmaxval = [0.5, 12, 294, 0.7E-2]
Lstep = [0.05, 0.5, 0.5, 0.05E-2]
Lstepticks = Lstep
Lfacconv = [1000, 1000, 1, 1000]
Lcolormap = ['gist_rainbow_r']*len(Lplot)
LaddWhite = [True, False, False, True]

fig1 = Panel1.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                LaddWhite_cm=LaddWhite, Lylim=Lylim)

Panel1.save_graph(1,fig1)
