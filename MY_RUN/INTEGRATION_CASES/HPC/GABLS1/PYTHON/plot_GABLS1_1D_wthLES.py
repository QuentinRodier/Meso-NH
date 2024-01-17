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
import numpy as np
from scipy.signal import savgol_filter 

os.system('rm -f tempgraph*')
#
#  User's parameter / Namelist
#
path=""

LnameFiles = ['RBL89.1.ECH01.000.nc','RRM17.1.ECH01.000.nc','GABL1.1.ECH01.000.nc']
LG_MEAN = '/LES_budgets/Mean/Cartesian/Not_time_averaged/Not_normalized/cart/'
LG_SBG = '/LES_budgets/Subgrid/Cartesian/Not_time_averaged/Not_normalized/cart/'
LG_RES = '/LES_budgets/Resolved/Cartesian/Not_time_averaged/Not_normalized/cart/'

Dvar_input = {
'f1':[(LG_SBG,'SBG_TKE'),(LG_SBG,'SBG_WU'),(LG_SBG,'SBG_WV'),(LG_SBG,'SBG_KM'),(LG_SBG,'SBG_KH'),(LG_SBG,'SBG_WTHL'),(LG_SBG,'SBG_THL2'),
      (LG_MEAN,'MEAN_U'),(LG_MEAN,'MEAN_V'),(LG_MEAN,'MEAN_TH'),
      'time_les','level_les'],
'f2':[(LG_SBG,'SBG_TKE'),(LG_SBG,'SBG_WU'),(LG_SBG,'SBG_WV'),(LG_SBG,'SBG_KM'),(LG_SBG,'SBG_KH'),(LG_SBG,'SBG_WTHL'),(LG_SBG,'SBG_THL2'),
      (LG_MEAN,'MEAN_U'),(LG_MEAN,'MEAN_V'),(LG_MEAN,'MEAN_TH'),
      'time_les','level_les'],
'f3':[(LG_SBG,'SBG_TKE'),(LG_SBG,'SBG_WU'),(LG_SBG,'SBG_WV'),(LG_SBG,'SBG_KM'),(LG_SBG,'SBG_KH'),(LG_SBG,'SBG_WTHL'),(LG_SBG,'SBG_THL2'),
      (LG_RES,'RES_KE'),(LG_RES,'RES_WU'),(LG_RES,'RES_WV'),(LG_RES,'RES_WTH'),(LG_RES,'RES_TH2'),
      (LG_MEAN,'MEAN_U'),(LG_MEAN,'MEAN_V'),(LG_MEAN,'MEAN_TH'),
      'time_les','level_les'],
}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path, removeHALO=False)

#  Averaging time between 9th to 8th hour since beginning of the run
delta_t_les = Dvar['f1']['time_les'][1]-Dvar['f1']['time_les'][0] # = 100 seconds
t_beg=int(8*(3600/delta_t_les))
t_end=int(9*(3600/delta_t_les))

#  Intermediate computation
# 1D
for ifile in ['f1','f2']:
    Dvar[ifile]['WIND'] = np.sqrt(Dvar[ifile][(LG_MEAN,'MEAN_U')]**2 + Dvar[ifile][(LG_MEAN,'MEAN_V')]**2)
    Dvar[ifile]['STRESS'] = np.sqrt(Dvar[ifile][(LG_SBG,'SBG_WU')]**2 + Dvar[ifile][(LG_SBG,'SBG_WV')]**2)
    Dvar[ifile]['SURF_STRESS'] = Dvar[ifile]['STRESS'][0,:]

# LES
for ifile in ['f3']:
    Dvar[ifile]['WIND'] = np.sqrt(Dvar[ifile][(LG_MEAN,'MEAN_U')]**2 + Dvar[ifile][(LG_MEAN,'MEAN_V')]**2)
    Dvar[ifile]['THL2'] = Dvar[ifile][(LG_RES,'RES_TH2')] + Dvar[ifile][(LG_SBG,'SBG_THL2')]
    Dvar[ifile]['WTHL'] = Dvar[ifile][(LG_RES,'RES_WTH')] + Dvar[ifile][(LG_SBG,'SBG_WTHL')]
    Dvar[ifile]['TKE'] = Dvar[ifile][(LG_RES,'RES_KE')] + Dvar[ifile][(LG_SBG,'SBG_TKE')]
    Dvar[ifile]['UW'] = Dvar[ifile][(LG_RES,'RES_WU')] + Dvar[ifile][(LG_SBG,'SBG_WU')]
    Dvar[ifile]['VW'] = Dvar[ifile][(LG_RES,'RES_WV')] + Dvar[ifile][(LG_SBG,'SBG_WV')]
    Dvar[ifile]['STRESS'] = np.sqrt((Dvar[ifile]['UW'])**2 + (Dvar[ifile]['VW'])**2)
    Dvar[ifile]['SURF_STRESS'] = Dvar[ifile]['STRESS'][0,:]
    Dvar[ifile]['DTHETATZ'] = np.asarray(np.gradient(Dvar[ifile][(LG_MEAN,'MEAN_TH')],tuple(Dvar[ifile]['level_les']),axis=0))
    Dvar[ifile]['DUDZ'] = np.asarray(np.gradient(Dvar[ifile][(LG_MEAN,'MEAN_U')],tuple(Dvar[ifile]['level_les']),axis=0))
    Dvar[ifile]['DVDZ'] = np.asarray(np.gradient(Dvar[ifile][(LG_MEAN,'MEAN_V')],tuple(Dvar[ifile]['level_les']),axis=0))
    Dvar[ifile]['Kh'] = - Dvar[ifile]['WTHL']/Dvar[ifile]['DTHETATZ']
    Dvar[ifile]['Ku'] = - Dvar[ifile]['UW']/Dvar[ifile]['DUDZ']
    Dvar[ifile]['Kv'] = - Dvar[ifile]['VW']/Dvar[ifile]['DVDZ']

for ifile in ['f1','f2','f3']:
    #  Stable boundary layer height diagnostic based on Cuxart et al. 2006
    Dvar[ifile]['SBL_H'] = Dvar[ifile]['STRESS'][0,:]
    for t in range(Dvar[ifile]['time_les'].shape[0]):
        for z in range(Dvar[ifile]['level_les'].shape[0]):
            if np.abs(Dvar[ifile]['STRESS'][z,t] < np.abs(0.05*Dvar[ifile]['SURF_STRESS'][t])):
                Dvar[ifile]['SBL_H'][t] = Dvar[ifile]['level_les'][z]
                break
    Dvar[ifile]['SBL_H'] = Dvar[ifile]['SBL_H'] / 0.95

################################################################
#########          PANEL 
###############################################################
Panel = PanelPlot(3,3, [25,25],'8-9h time averaged vertical profiles', titlepad=11, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=40)

Lplot = [np.mean(Dvar['f1'][(LG_MEAN,'MEAN_TH')][:,t_beg:t_end],axis=1), np.mean(Dvar['f2'][(LG_MEAN,'MEAN_TH')][:,t_beg:t_end],axis=1), 
         np.mean(Dvar['f3'][(LG_MEAN,'MEAN_TH')][:,t_beg:t_end],axis=1)]
LaxeZ = [Dvar['f1']['level_les'], Dvar['f2']['level_les'], Dvar['f3']['level_les']]
Ltitle = ['MEAN_TH']*len(Lplot)
Llinelabel = ['1D BL89','1D RM17', 'LES']
Lxlab = ['theta (K)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,400.1)]*len(Lplot)
Lxlim = [(262, 268)]*len(Lplot)
Llinecolor = ['red','blue', 'black']
LaxisColor = ['black']*len(Lplot)
Llinewidth = [3]*len(Lplot)
fig = Panel.pXY_lines(Lxx=Lplot, Lyy=LaxeZ, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Llinewidth=Llinewidth,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor)

Lplot = [np.mean(Dvar['f1']['WIND'][:,t_beg:t_end],axis=1), np.mean(Dvar['f2']['WIND'][:,t_beg:t_end],axis=1), np.mean(Dvar['f3']['WIND'][:,t_beg:t_end],axis=1)]
Ltitle = ['Wind speed']*len(Lplot)
Lxlab = ['Wind speed (m/s)']*len(Lplot)
Lxlim = [(0, 11)]*len(Lplot)
fig = Panel.pXY_lines(Lxx=Lplot, Lyy=LaxeZ, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, ax=fig.axes, Llinewidth=Llinewidth,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor)

Lplot = [Dvar['f1']['SBL_H'][:], Dvar['f2']['SBL_H'][:], Dvar['f3']['SBL_H'][:]]

Ltitle = ['Boundary layer height']*len(Lplot)
LaxeTime = [Dvar['f1']['time_les']/3600.0, Dvar['f2']['time_les']/3600.0, Dvar['f3']['time_les']/3600.0]
Lxlab = ['Time (h)']*len(Lplot)
Lxlim = [(0, 9)]*len(Lplot)
Lylim = [(0, 300.1)]*len(Lplot)
fig = Panel.pXY_lines(Lyy=Lplot, Lxx=LaxeTime, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, ax=fig.axes,Llinewidth=Llinewidth,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor)

Lplot = [np.mean(Dvar['f1'][(LG_SBG,'SBG_TKE')][:,t_beg:t_end],axis=1), np.mean(Dvar['f2'][(LG_SBG,'SBG_TKE')][:,t_beg:t_end],axis=1),
         np.mean(Dvar['f3']['TKE'][:,t_beg:t_end],axis=1)]
Ltitle = ['TKE']*len(Lplot)
Lxlab = ['TKE (m2/s2)']*len(Lplot)
Lylim = [(0,400.1)]*len(Lplot)
Lxlim = [(0, 0.6)]*len(Lplot)
fig = Panel.pXY_lines(Lxx=Lplot, Lyy=LaxeZ, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, ax=fig.axes,Llinewidth=Llinewidth,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor)

Lplot = [np.mean(Dvar['f1'][(LG_SBG,'SBG_WTHL')][:,t_beg:t_end],axis=1), np.mean(Dvar['f2'][(LG_SBG,'SBG_WTHL')][:,t_beg:t_end],axis=1),
         np.mean(Dvar['f3']['WTHL'][:,t_beg:t_end],axis=1)]
Ltitle = [r"Flux $w' \theta_l'$"]*len(Lplot)
Lxlab = [r"$w' \theta_l'(K.m/s)$"]*len(Lplot)
Lxlim = [(-0.02, 0.)]*len(Lplot)
fig = Panel.pXY_lines(Lxx=Lplot, Lyy=LaxeZ, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, ax=fig.axes,Llinewidth=Llinewidth,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor)

Lplot = [np.mean(Dvar['f1']['STRESS'][:,t_beg:t_end],axis=1), np.mean(Dvar['f2']['STRESS'][:,t_beg:t_end],axis=1), 
         np.mean(Dvar['f3']['STRESS'][:,t_beg:t_end],axis=1)]
Ltitle = ['Stress']*len(Lplot)
Lxlab = [r"$\tau (m^2/s^2)$"]*len(Lplot)
Lxlim = [(0, 0.12)]*len(Lplot)
fig = Panel.pXY_lines(Lxx=Lplot, Lyy=LaxeZ, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, ax=fig.axes,Llinewidth=Llinewidth,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor)

Lplot = [np.mean(Dvar['f1'][(LG_SBG,'SBG_THL2')][:,t_beg:t_end],axis=1), np.mean(Dvar['f2'][(LG_SBG,'SBG_THL2')][:,t_beg:t_end],axis=1), 
         np.mean(Dvar['f3']['THL2'][:,t_beg:t_end],axis=1)]
Ltitle = [r"Variance $\theta_l'^2$ "]*len(Lplot)
Lxlab = [r"$\theta_l'^2 (K^2)$"]*len(Lplot)
Lxlim = [(0, 0.01)]*len(Lplot)
fig = Panel.pXY_lines(Lxx=Lplot, Lyy=LaxeZ, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, ax=fig.axes,Llinewidth=Llinewidth,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor)

Lplot = [np.mean(Dvar['f1'][(LG_SBG,'SBG_KH')][:,t_beg:t_end],axis=1), np.mean(Dvar['f2'][(LG_SBG,'SBG_KH')][:,t_beg:t_end],axis=1), 
         np.mean(Dvar['f3']['Kh'][:,t_beg:t_end],axis=1)]
Ltitle = ['Kh coefficient']*len(Lplot)
Lxlab = ['Kh (m2/s)']*len(Lplot)
Lxlim = [(0, 4.)]*len(Lplot)
fig = Panel.pXY_lines(Lxx=Lplot, Lyy=LaxeZ, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, ax=fig.axes,Llinewidth=Llinewidth,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor)

# Smoothing to avoid highly frequent variations due to gradient compuation of quasi-constant U/TH profile above the boundary layer
Ku_LES=savgol_filter(np.mean(Dvar['f3']['Ku'][:,t_beg:t_end],axis=1),int(Dvar['f3']['level_les'].shape[0]/2),3)
Kv_LES=savgol_filter(np.mean(Dvar['f3']['Kv'][:,t_beg:t_end],axis=1),int(Dvar['f3']['level_les'].shape[0]/2),5)

Lplot = [np.mean(Dvar['f1'][(LG_SBG,'SBG_KM')][:,t_beg:t_end],axis=1), np.mean(Dvar['f2'][(LG_SBG,'SBG_KM')][:,t_beg:t_end],axis=1),
         Ku_LES, Kv_LES]
LaxeZ = [Dvar['f1']['level_les'], Dvar['f2']['level_les'], Dvar['f3']['level_les'],Dvar['f3']['level_les'] ]
Ltitle = ['Km coefficient']*len(Lplot)
Lxlab = ['Km (m2/s)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Llinelabel = ['1D BL89','1D RM17', 'LES Ku','LES Kv']
Lxlim = [(0, 4.)]*len(Lplot)
Lylim = [(0,400.1)]*len(Lplot)
Llinecolor = ['red','blue', 'black','grey']
LaxisColor = ['black']*len(Lplot)
Llinewidth = [3]*len(Lplot)

fig = Panel.pXY_lines(Lxx=Lplot, Lyy=LaxeZ, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, ax=fig.axes,Llinewidth=Llinewidth,
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor)
Panel.save_graph(1,fig)
