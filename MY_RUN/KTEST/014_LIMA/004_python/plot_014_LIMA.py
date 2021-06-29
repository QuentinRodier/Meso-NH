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
from misc_functions import comp_altitude1DVar
import os

os.system('rm -f tempgraph*')
#
#  User's parameter / Namelist
#
path=""
LnameFiles = ['XPREF.1.SEG01.000.nc' ]

Dvar_input = {'f1':[('RI','AVEF'), ('CICE','AVEF'), ('RS','AVEF'),('RG','AVEF'), ('CIFNFREE01','AVEF'),('CIFNNUCL01','AVEF') ]}
Dvar_input_coord_budget = {'f1':['cart_level', 'cart_ni']}
Dvar_input_coord = {'f1':['ZS','ZTOP']}

#  Read the variables in the files
Dvar, Dvar_coord_budget, Dvar_coord = {}, {}, {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path, removeHALO=False)
Dvar_coord_budget = read_netcdf(LnameFiles, Dvar_input_coord_budget, path=path, removeHALO=False)
Dvar_coord = read_netcdf(LnameFiles, Dvar_input_coord, path=path, removeHALO=True)

Dvar['f1']['altitude'], Dvar['f1']['ni_2D'] = comp_altitude1DVar(Dvar['f1'][('CIFNFREE01','AVEF')], Dvar_coord['f1']['ZS'],Dvar_coord['f1']['ZTOP'], Dvar_coord_budget['f1']['cart_level'],Dvar_coord_budget['f1']['cart_ni'])

################################################################
#########          PANEL 1
###############################################################
Panel1 = PanelPlot(2,3, [25,14],'014_LIMA', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)

Lplot = [Dvar['f1'][('RI','AVEF')],Dvar['f1'][('CICE','AVEF')], Dvar['f1'][('RS','AVEF')],
         Dvar['f1'][('RG','AVEF')],Dvar['f1'][('CIFNFREE01','AVEF')], Dvar['f1'][('CIFNNUCL01','AVEF')]]
LaxeX = [Dvar['f1']['ni_2D']]*len(Lplot)
LaxeZ = [Dvar['f1']['altitude']]*len(Lplot)
Ltitle = ['Ice water content', 'Ice concentration', 'Snow water content',
          'Graupel water content', 'Concentration of free IFN', 'Concentration of nucleated IFN']
Lcbarlabel = ['g/kg', '/kg', 'g/kg','g/kg',r'x $10^6$/kg','/kg']
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,16000)]*len(Lplot)
Lminval = [0, 0, 0, 0, 0.8 ,0]
Lmaxval = [3.75E-2, 5400, 3E-1, 3E-1, 7, 1800]
Lstep = [0.25E-2,400,0.01,0.01,0.25, 100]
Lstepticks = Lstep
Lfacconv = [1000, 1, 1000, 1000, 1E-6, 1]
orog = Dvar_coord['f1']['ZS']
Lcolormap = ['gist_rainbow_r']*len(Lplot)
LaddWhite = [True, True, True, True, False, True]

fig1 = Panel1.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv,Lylim=Lylim, 
                                orog=orog, LaddWhite_cm=LaddWhite)

Panel1.save_graph(1,fig1)
