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
LnameFiles = ['COPT8.1.CEN4T.000.nc' ]

Dvar_input = {'f1':[('/Budgets/RI','AVEF'),('/Budgets/RR','AVEF'),('/Budgets/RS','AVEF'),('/Budgets/RG','AVEF'),('/Budgets/RC','AVEF'),
                     ('/Budgets/TH','DEPS'),('/Budgets/TH','SFR'),('/Budgets/TH','IMLT'),('/Budgets/TH','DEPG'),('/Budgets/TH','REVA'),('/Budgets/TH','SFR'),
                     ('/Budgets/TH','DRYG'),('/Budgets/TH','WETG'),('/Budgets/TH','DEPI'),('/Budgets/TH','GMLT'),('/Budgets/TH','RIM'),('/Budgets/TH','BERFI'),
                     ('/Budgets/TH','CFRZ'),('/Budgets/TH','ACC'),
                     ('/Budgets/RV','DEPS'), ('/Budgets/RV','DEPG'), ('/Budgets/RV','REVA'), ('/Budgets/RV','DEPI'),
                     ('/Budgets/RC','AUTO'), ('/Budgets/RC','ACCR'), ('/Budgets/RC','RIM'), ('/Budgets/RC','DRYG'),
                     ('/Budgets/RC','SEDI'), ('/Budgets/RC','DEPI'), ('/Budgets/RC','BERFI'), ('/Budgets/RC','IMLT'),('/Budgets/RC','HON'),
                     ('/Budgets/RR','AUTO'), ('/Budgets/RR','ACCR'), ('/Budgets/RR','REVA'), ('/Budgets/RR','DRYG'),('/Budgets/RC','WETG'),
                     ('/Budgets/RR','GMLT'), ('/Budgets/RR','ACC'), ('/Budgets/RR','SFR'), ('/Budgets/RR','WETG'),
                     ('/Budgets/RI','AGGS'), ('/Budgets/RI','AUTS'), ('/Budgets/RI','CFRZ'), ('/Budgets/RI','IMLT'),
                     ('/Budgets/RI','DEPI'), ('/Budgets/RI','BERFI'), ('/Budgets/RI','HON'), ('/Budgets/RI','DRYG'),
                     ('/Budgets/RS','AGGS'), ('/Budgets/RS','AUTS'), ('/Budgets/RS','ACC'), ('/Budgets/RS','CMEL'),
                     ('/Budgets/RS','DEPS'), ('/Budgets/RS','RIM'), ('/Budgets/RS','DRYG'), ('/Budgets/RS','SEDI'),('/Budgets/RS','WETG'),
                     ('/Budgets/RG','DEPG'), ('/Budgets/RG','ACC'), ('/Budgets/RG','CFRZ'), ('/Budgets/RG','CMEL'),('/Budgets/RG','SFR'),
                     ('/Budgets/RG','WETG'), ('/Budgets/RG','GMLT'), ('/Budgets/RG','RIM'), ('/Budgets/RG','DRYG'),
                     'time_budget','mask_level']}

#    
#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path)

################################################################
#########          PANEL 1
###############################################################
Panel = PanelPlot(2,2, [20,20],'COPT81 avec Mask', titlepad=11, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)

# Mixing ratio of hydrometeores
nmask=0 #Convective mask, criteria in set_mask.f90
ntime=7 # 8th hour

Lplot = [Dvar['f1'][('/Budgets/RC','AVEF')][nmask,ntime,:], Dvar['f1'][('/Budgets/RR','AVEF')][nmask,ntime,:], Dvar['f1'][('/Budgets/RI','AVEF')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/RG','AVEF')][nmask,ntime,:], Dvar['f1'][('/Budgets/RS','AVEF')][nmask,ntime,:]]

LaxeZ = [Dvar['f1']['mask_level']]*len(Lplot)
Ltitle = ['Bilan hydrométéores : partie convective - MASK1 AVEF']*len(Lplot)
Llinelabel = ['Rc','Rr','Ri','Rg','Rs']
Lxlab = ['Rapport de mélange (g/kg)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,12000.0)]*len(Lplot)
Lxlim = [(0, 1.2)]*len(Lplot)
Llinecolor = ['red','green','blue','magenta','cyan']
LaxisColor = ['black']*len(Lplot)
Llinewidth = [3]*len(Lplot)
LfacconvX=[1000]*len(Lplot)
fig = Panel.pXY_lines(Lxx=Lplot, Lyy=LaxeZ, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Llinewidth=Llinewidth, LfacconvX=LfacconvX,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor)

nmask=1 #Stratiforme mask
Ltitle = ['Bilan hydrométéores : partie stratiforme - MASK2 AVEF']*len(Lplot)
Lplot = [Dvar['f1'][('/Budgets/RC','AVEF')][nmask,ntime,:], Dvar['f1'][('/Budgets/RR','AVEF')][nmask,ntime,:], Dvar['f1'][('/Budgets/RI','AVEF')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/RG','AVEF')][nmask,ntime,:], Dvar['f1'][('/Budgets/RS','AVEF')][nmask,ntime,:]]
Lxlim = [(0, 0.25)]*len(Lplot)
fig = Panel.pXY_lines(Lxx=Lplot, Lyy=LaxeZ, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Llinewidth=Llinewidth, LfacconvX=LfacconvX,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor,
                        ax=fig.axes)


# Budget of potential temperature
nmask=0 #Convective mask, criteria in set_mask.f90

Lplot = [Dvar['f1'][('/Budgets/TH','SFR')][nmask,ntime,:], Dvar['f1'][('/Budgets/TH','DEPS')][nmask,ntime,:], Dvar['f1'][('/Budgets/TH','DEPG')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/TH','REVA')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/TH','DEPI')][nmask,ntime,:], Dvar['f1'][('/Budgets/TH','IMLT')][nmask,ntime,:], Dvar['f1'][('/Budgets/TH','GMLT')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/TH','DRYG')][nmask,ntime,:], Dvar['f1'][('/Budgets/TH','ACC')][nmask,ntime,:], Dvar['f1'][('/Budgets/TH','RIM')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/TH','BERFI')][nmask,ntime,:], Dvar['f1'][('/Budgets/TH','CFRZ')][nmask,ntime,:], Dvar['f1'][('/Budgets/TH','WETG')][nmask,ntime,:]
         ]
LaxeZ = [Dvar['f1']['mask_level']]*len(Lplot)
Ltitle = ['Bilan température potentielle : partie convective - MASK1']*len(Lplot)
Llinelabel = ['SFR','DEPS','DEPG','REVA','DEPI','IMLT','GMLT','DRYG','ACC','RIM','BERFI','CFRZ','WETG']
Lxlim = [(-0.7E-2, 0.7E-2)]*len(Lplot)
Lxlab = ['Terme du bilan (K)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,12000.0)]*len(Lplot)
LaxisColor = ['black']*len(Lplot)
Llinewidth = [3]*len(Lplot)
LfacconvX=[1]*len(Lplot)
Llinecolor = ['red','green','blue','cyan','indigo','bisque','brown','orange','yellow',
              'magenta','gray','lightblue','black']

fig = Panel.pXY_lines(Lxx=Lplot, Lyy=LaxeZ, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Llinewidth=Llinewidth, LfacconvX=LfacconvX,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor,
                        ax=fig.axes)

nmask=1 #Convective mask, criteria in set_mask.f90
Lplot = [Dvar['f1'][('/Budgets/TH','SFR')][nmask,ntime,:], Dvar['f1'][('/Budgets/TH','DEPS')][nmask,ntime,:], Dvar['f1'][('/Budgets/TH','DEPG')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/TH','REVA')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/TH','DEPI')][nmask,ntime,:], Dvar['f1'][('/Budgets/TH','IMLT')][nmask,ntime,:], Dvar['f1'][('/Budgets/TH','GMLT')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/TH','DRYG')][nmask,ntime,:], Dvar['f1'][('/Budgets/TH','ACC')][nmask,ntime,:], Dvar['f1'][('/Budgets/TH','RIM')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/TH','BERFI')][nmask,ntime,:], Dvar['f1'][('/Budgets/TH','CFRZ')][nmask,ntime,:], Dvar['f1'][('/Budgets/TH','WETG')][nmask,ntime,:]
         ]
Lxlim = [(-0.7E-3, 0.7E-3)]*len(Lplot)
Ltitle = ['Bilan température potentielle : partie stratiforme - MASK2']*len(Lplot)

fig = Panel.pXY_lines(Lxx=Lplot, Lyy=LaxeZ, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Llinewidth=Llinewidth, LfacconvX=LfacconvX,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor,
                        ax=fig.axes)
Panel.save_graph(1,fig)


################################################################
#########          PANEL 2
###############################################################
Panel = PanelPlot(2,2, [20,20],'COPT81 avec Mask', titlepad=11, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)

# Budget of Water vapor
nmask=0 #Convective mask, criteria in set_mask.f90

Lplot = [Dvar['f1'][('/Budgets/RV','DEPS')][nmask,ntime,:], Dvar['f1'][('/Budgets/RV','DEPG')][nmask,ntime,:], Dvar['f1'][('/Budgets/RV','REVA')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/RV','DEPI')][nmask,ntime,:]
         ]
LaxeZ = [Dvar['f1']['mask_level']]*len(Lplot)
Ltitle = ['Bilan rapport de mélange vapeur d\'eau : partie convective - MASK1']*len(Lplot)
Llinelabel = ['DEPS','DEPG','REVA','DEPI']
Lxlim = [(-0.25E-5, 0.25E-5)]*len(Lplot)
Lxlab = ['Terme du bilan ($s^{-1}$)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,12000.0)]*len(Lplot)
LaxisColor = ['black']*len(Lplot)
Llinewidth = [3]*len(Lplot)
LfacconvX=[1]*len(Lplot)
Llinecolor = ['red','green','blue','cyan']

fig = Panel.pXY_lines(Lxx=Lplot, Lyy=LaxeZ, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Llinewidth=Llinewidth, LfacconvX=LfacconvX,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor)


nmask=1 #Convective mask, criteria in set_mask.f90
Lplot = [Dvar['f1'][('/Budgets/RV','DEPS')][nmask,ntime,:], Dvar['f1'][('/Budgets/RV','DEPG')][nmask,ntime,:], Dvar['f1'][('/Budgets/RV','REVA')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/RV','DEPI')][nmask,ntime,:]
         ]
Ltitle = ['Bilan rapport de mélange vapeur d\'eau : partie stratiforme - MASK2']*len(Lplot)

fig = Panel.pXY_lines(Lxx=Lplot, Lyy=LaxeZ, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Llinewidth=Llinewidth, LfacconvX=LfacconvX,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor,
                        ax=fig.axes)

# Budget of Cloud water
nmask=0 #Convective mask, criteria in set_mask.f90

Lplot = [Dvar['f1'][('/Budgets/RC','HON')][nmask,ntime,:], Dvar['f1'][('/Budgets/RC','AUTO')][nmask,ntime,:], Dvar['f1'][('/Budgets/RC','ACCR')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/RC','WETG')][nmask,ntime,:], Dvar['f1'][('/Budgets/RC','BERFI')][nmask,ntime,:], Dvar['f1'][('/Budgets/RC','DEPI')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/RC','IMLT')][nmask,ntime,:], Dvar['f1'][('/Budgets/RC','DRYG')][nmask,ntime,:]
         ]
LaxeZ = [Dvar['f1']['mask_level']]*len(Lplot)
Ltitle = ['Bilan rapport en eau nuageuse : partie convective - MASK1']*len(Lplot)
Llinelabel = ['HON','AUTO','ACCR','WETG','BERFI','DEPI','IMLT','DRYG']
Lxlim = [(-0.25E-5, 0.25E-5)]*len(Lplot)
Lxlab = ['Terme du bilan ($s^{-1}$)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,12000.0)]*len(Lplot)
LaxisColor = ['black']*len(Lplot)
Llinewidth = [3]*len(Lplot)
LfacconvX=[1]*len(Lplot)
Llinecolor = ['red','green','blue','cyan','brown','orange','yellow','magenta']

fig = Panel.pXY_lines(Lxx=Lplot, Lyy=LaxeZ, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Llinewidth=Llinewidth, LfacconvX=LfacconvX,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor,
                        ax=fig.axes)

nmask=1 #Convective mask, criteria in set_mask.f90
Ltitle = ['Bilan rapport en eau nuageuse : partie stratiforme - MASK2']*len(Lplot)
Lxlim = [(-0.25E-6, 0.25E-6)]*len(Lplot)
Lplot = [Dvar['f1'][('/Budgets/RC','HON')][nmask,ntime,:], Dvar['f1'][('/Budgets/RC','AUTO')][nmask,ntime,:], Dvar['f1'][('/Budgets/RC','ACCR')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/RC','WETG')][nmask,ntime,:], Dvar['f1'][('/Budgets/RC','BERFI')][nmask,ntime,:], Dvar['f1'][('/Budgets/RC','DEPI')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/RC','IMLT')][nmask,ntime,:], Dvar['f1'][('/Budgets/RC','DRYG')][nmask,ntime,:]
         ]
fig = Panel.pXY_lines(Lxx=Lplot, Lyy=LaxeZ, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Llinewidth=Llinewidth, LfacconvX=LfacconvX,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor,
                        ax=fig.axes)
Panel.save_graph(2,fig)


################################################################
#########          PANEL 3
###############################################################
Panel = PanelPlot(2,2, [20,20],'COPT81 avec Mask', titlepad=11, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)
                    
# Budget of Rain
nmask=0 #Convective mask, criteria in set_mask.f90

Lplot = [Dvar['f1'][('/Budgets/RR','SFR')][nmask,ntime,:], Dvar['f1'][('/Budgets/RR','AUTO')][nmask,ntime,:], Dvar['f1'][('/Budgets/RR','ACCR')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/RR','WETG')][nmask,ntime,:], Dvar['f1'][('/Budgets/RR','DRYG')][nmask,ntime,:], Dvar['f1'][('/Budgets/RR','REVA')][nmask,ntime,:]
         ]
LaxeZ = [Dvar['f1']['mask_level']]*len(Lplot)
Ltitle = ['Bilan rapport d\'eau précipitante : partie convective - MASK1']*len(Lplot)
Llinelabel = ['SFR','AUTO','ACCR','WETG','DRYG','REVA']
Lxlim = [(-0.3E-5, 0.3E-5)]*len(Lplot)
Lxlab = ['Terme du bilan ($s^{-1}$)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,12000.0)]*len(Lplot)
LaxisColor = ['black']*len(Lplot)
Llinewidth = [3]*len(Lplot)
LfacconvX=[1]*len(Lplot)
Llinecolor = ['red','green','blue','cyan','yellow','magenta']

fig = Panel.pXY_lines(Lxx=Lplot, Lyy=LaxeZ, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Llinewidth=Llinewidth, LfacconvX=LfacconvX,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor)


nmask=1 #Convective mask, criteria in set_mask.f90
Ltitle = ['Bilan rapport d\'eau précipitante : partie stratiforme - MASK2']*len(Lplot)
Lxlim = [(-0.6E-6, 0.6E-6)]*len(Lplot)
Lplot = [Dvar['f1'][('/Budgets/RR','SFR')][nmask,ntime,:], Dvar['f1'][('/Budgets/RR','AUTO')][nmask,ntime,:], Dvar['f1'][('/Budgets/RR','ACCR')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/RR','WETG')][nmask,ntime,:], Dvar['f1'][('/Budgets/RR','DRYG')][nmask,ntime,:], Dvar['f1'][('/Budgets/RR','REVA')][nmask,ntime,:]
         ]
fig = Panel.pXY_lines(Lxx=Lplot, Lyy=LaxeZ, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Llinewidth=Llinewidth, LfacconvX=LfacconvX,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor,
                        ax=fig.axes)

# Budget of Ice
nmask=0 #Convective mask, criteria in set_mask.f90

Lplot = [Dvar['f1'][('/Budgets/RI','HON')][nmask,ntime,:], Dvar['f1'][('/Budgets/RI','AGGS')][nmask,ntime,:], Dvar['f1'][('/Budgets/RI','AUTS')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/RI','DRYG')][nmask,ntime,:], Dvar['f1'][('/Budgets/RI','DEPI')][nmask,ntime,:], Dvar['f1'][('/Budgets/RI','IMLT')][nmask,ntime,:]
         ]
LaxeZ = [Dvar['f1']['mask_level']]*len(Lplot)
Ltitle = ['Bilan rapport de mélange en glace : partie convective - MASK1']*len(Lplot)
Llinelabel = ['HON','AGGS','AUTS','DRYG','DEPI','IMLT']
Lxlim = [(-0.2E-5, 0.2E-5)]*len(Lplot)
Lxlab = ['Terme du bilan ($s^{-1}$)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,12000.0)]*len(Lplot)
LaxisColor = ['black']*len(Lplot)
Llinewidth = [3]*len(Lplot)
LfacconvX=[1]*len(Lplot)
Llinecolor = ['red','green','blue','cyan','yellow','magenta']

fig = Panel.pXY_lines(Lxx=Lplot, Lyy=LaxeZ, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Llinewidth=Llinewidth, LfacconvX=LfacconvX,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor,
                        ax=fig.axes)

nmask=1 #Convective mask, criteria in set_mask.f90

Lplot = [Dvar['f1'][('/Budgets/RI','HON')][nmask,ntime,:], Dvar['f1'][('/Budgets/RI','AGGS')][nmask,ntime,:], Dvar['f1'][('/Budgets/RI','AUTS')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/RI','DRYG')][nmask,ntime,:], Dvar['f1'][('/Budgets/RI','DEPI')][nmask,ntime,:], Dvar['f1'][('/Budgets/RI','IMLT')][nmask,ntime,:]
         ]

Ltitle = ['Bilan rapport de mélange en glace : partie stratiforme - MASK2']*len(Lplot)

fig = Panel.pXY_lines(Lxx=Lplot, Lyy=LaxeZ, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Llinewidth=Llinewidth, LfacconvX=LfacconvX,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor,
                        ax=fig.axes)
Panel.save_graph(3,fig)

################################################################
#########          PANEL 4
###############################################################
Panel = PanelPlot(2,2, [20,20],'COPT81 avec Mask', titlepad=11, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)
                    
# Budget of Snow
nmask=0 #Convective mask, criteria in set_mask.f90

Lplot = [Dvar['f1'][('/Budgets/RS','DEPS')][nmask,ntime,:], Dvar['f1'][('/Budgets/RS','AGGS')][nmask,ntime,:], Dvar['f1'][('/Budgets/RS','RIM')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/RS','ACC')][nmask,ntime,:], Dvar['f1'][('/Budgets/RS','SEDI')][nmask,ntime,:], Dvar['f1'][('/Budgets/RS','AUTS')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/RS','WETG')][nmask,ntime,:]
         ]
LaxeZ = [Dvar['f1']['mask_level']]*len(Lplot)
Ltitle = ['Bilan rapport de mélange de neige : partie convective - MASK1']*len(Lplot)
Llinelabel = ['DEPS','AGGS','RIM','ACC','SEDI','AUTS','WETG']
Lxlim = [(-0.2E-5, 0.2E-5)]*len(Lplot)
Lxlab = ['Terme du bilan ($s^{-1}$)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,12000.0)]*len(Lplot)
LaxisColor = ['black']*len(Lplot)
Llinewidth = [3]*len(Lplot)
LfacconvX=[1]*len(Lplot)
Llinecolor = ['red','green','blue','cyan','orange','yellow','magenta']

fig = Panel.pXY_lines(Lxx=Lplot, Lyy=LaxeZ, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Llinewidth=Llinewidth, LfacconvX=LfacconvX,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor)


nmask=1 #Convective mask, criteria in set_mask.f90
Lplot = [Dvar['f1'][('/Budgets/RS','DEPS')][nmask,ntime,:], Dvar['f1'][('/Budgets/RS','AGGS')][nmask,ntime,:], Dvar['f1'][('/Budgets/RS','RIM')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/RS','ACC')][nmask,ntime,:], Dvar['f1'][('/Budgets/RS','SEDI')][nmask,ntime,:], Dvar['f1'][('/Budgets/RS','AUTS')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/RS','WETG')][nmask,ntime,:]
         ]
Ltitle = ['Bilan rapport de mélange de neige : partie stratiforme - MASK2']*len(Lplot)
Lxlim = [(-0.2E-6, 0.2E-6)]*len(Lplot)

fig = Panel.pXY_lines(Lxx=Lplot, Lyy=LaxeZ, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Llinewidth=Llinewidth, LfacconvX=LfacconvX,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor,
                        ax=fig.axes)

# Budget of Graupel
nmask=0 #Convective mask, criteria in set_mask.f90
               
Lplot = [Dvar['f1'][('/Budgets/RG','SFR')][nmask,ntime,:], Dvar['f1'][('/Budgets/RG','RIM')][nmask,ntime,:], Dvar['f1'][('/Budgets/RG','CMEL')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/RG','ACC')][nmask,ntime,:], Dvar['f1'][('/Budgets/RG','CFRZ')][nmask,ntime,:], Dvar['f1'][('/Budgets/RG','DRYG')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/RG','DEPG')][nmask,ntime,:], Dvar['f1'][('/Budgets/RG','WETG')][nmask,ntime,:]
         ]
LaxeZ = [Dvar['f1']['mask_level']]*len(Lplot)
Ltitle = ['Bilan rapport en graupel : partie convective - MASK1']*len(Lplot)
Llinelabel = ['SFR','RIM','CMEL','ACC','CFRZ','DRYG','DEPG','WETG']
Lxlim = [(-0.3E-5, 0.3E-5)]*len(Lplot)
Lxlab = ['Terme du bilan ($s^{-1}$)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,12000.0)]*len(Lplot)
LaxisColor = ['black']*len(Lplot)
Llinewidth = [3]*len(Lplot)
LfacconvX=[1]*len(Lplot)
Llinecolor = ['red','green','blue','cyan','brown','orange','yellow','magenta']

fig = Panel.pXY_lines(Lxx=Lplot, Lyy=LaxeZ, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Llinewidth=Llinewidth, LfacconvX=LfacconvX,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor,
                        ax=fig.axes)

nmask=1 #Convective mask, criteria in set_mask.f90
Lplot = [Dvar['f1'][('/Budgets/RG','SFR')][nmask,ntime,:], Dvar['f1'][('/Budgets/RG','RIM')][nmask,ntime,:], Dvar['f1'][('/Budgets/RG','CMEL')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/RG','ACC')][nmask,ntime,:], Dvar['f1'][('/Budgets/RG','CFRZ')][nmask,ntime,:], Dvar['f1'][('/Budgets/RG','DRYG')][nmask,ntime,:],
         Dvar['f1'][('/Budgets/RG','DEPG')][nmask,ntime,:], Dvar['f1'][('/Budgets/RG','WETG')][nmask,ntime,:]
         ]

Ltitle = ['Bilan rapport en graupel : partie stratiforme - MASK2']*len(Lplot)
Lxlim = [(-0.3E-6, 0.3E-6)]*len(Lplot)

fig = Panel.pXY_lines(Lxx=Lplot, Lyy=LaxeZ, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Llinewidth=Llinewidth, LfacconvX=LfacconvX,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor,
                        ax=fig.axes)
Panel.save_graph(4,fig)
