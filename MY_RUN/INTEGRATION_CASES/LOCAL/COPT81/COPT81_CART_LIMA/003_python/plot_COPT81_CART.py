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
LnameFiles = ['COPT8.1.CEN4T.000.nc' ]

Dvar_input = {'f1':[('/Budgets/UU','AVEF'),('/Budgets/WW','AVEF'),('/Budgets/RI','AVEF'),('/Budgets/RR','AVEF'),
                    ('/Budgets/RS','AVEF'),('/Budgets/RG','AVEF'), ('/Budgets/RC','AVEF') ]}
Dvar_input_coord = {'f1':['cart_level', 'time_budget','cart_ni']}

#    
#  Read the variables in the files
Dvar, Dvar_coord = {}, {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path)
Dvar_coord = read_netcdf(LnameFiles, Dvar_input_coord, path=path)

################################################################
#########          PANEL 1
###############################################################
Panel = PanelPlot(3,3, [25,25],'COPT81', titlepad=15, minmaxpad=1.025, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)

time_plot=7

Lplot = [Dvar['f1'][('/Budgets/UU','AVEF')][time_plot,:,:],Dvar['f1'][('/Budgets/WW','AVEF')][time_plot,:,:], Dvar['f1'][('/Budgets/RC','AVEF')][time_plot,:,:], 
Dvar['f1'][('/Budgets/RR','AVEF')][time_plot,:,:],Dvar['f1'][('/Budgets/RG','AVEF')][time_plot,:,:], Dvar['f1'][('/Budgets/RI','AVEF')][time_plot,:,:], 
Dvar['f1'][('/Budgets/RS','AVEF')][time_plot,:,:]]

LaxeX = [Dvar_coord['f1']['cart_ni']]*len(Lplot)
LaxeZ = [Dvar_coord['f1']['cart_level']]*len(Lplot)
Ltitle = ['Zonal wind speed','Vertical velocity','Cloud water content','Rain water content', 'Graupel water content','Ice water content', 'Snow water content']
Lcbarlabel = ['m/s','m/s','g/kg','g/kg', 'g/kg', 'g/kg','g/kg']
Lxlab = ['x']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,16000)]*len(Lplot)
Lminval = [-8, -2, 0, 0, 0, 0, 0]
Lmaxval = [22, 4, 1.5, 1.5, 1.5, 0.8, 1.1]
Lstep = [1, 0.25, 0.02, 0.05, 0.05, 0.01, 0.05]
Lstepticks = [2, 0.5, 0.1, 0.1, 0.1, 0.05, 0.1]
Lfacconv = [1, 1, 1000, 1000, 1000, 1000, 1000]
Lcolormap = ['gist_rainbow_r']*len(Lplot)
LaddWhite = [False, False, True, True, True, True, True]

fig = Panel.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv,Lylim=Lylim, 
                                LaddWhite_cm=LaddWhite)
Panel.save_graph(1,fig)
