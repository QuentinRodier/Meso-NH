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
import cartopy.crs as ccrs
import os

os.system('rm -f tempgraph*')
#
#  User's parameter / Namelist
#
LnameFiles = ['INI_CEP.nc']

Dvar_input = {
'f1':['ZS', 'UT', 'VT', 'WT', 'THT', 'PABST','RVT', 'level','longitude','latitude','level_w','time',
      'SST','TS_WATER','TSRAD_NAT',
      'TG1P1','TG2P1','WG1P1','WG2P1','WG3P1',
      'TROOF1','TROAD1','TWALL1','TDEEP_TEB','TIBLD1','ASN_RD']
}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path="", removeHALO=True)

################################################################
#########          PANEL 1  # Horizontal cross-section U
###############################################################
Panel = PanelPlot(2,3, [25,14],'ERA5 prep_real MNH', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=22)
nb_pl=6

Lplot = [ Dvar['f1']['UT']]*6
lon = [Dvar['f1']['longitude']]*len(Lplot)
lat = [Dvar['f1']['latitude']]*len(Lplot)
Llvl = [0, 3, 8, 17, 26, 39]
Ltitle=['UT K='+str(i)+' ( '+str(Dvar['f1']['level'][i])+' m)' for i in Llvl]
Lcbarlabel = ['m/s']*len(Lplot)
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude']*len(Lplot)
Lminval, Lmaxval, Lstep, Lstepticks = [ [-10]*nb_pl, [22]*nb_pl, [0.5]*nb_pl, [5]*nb_pl ]
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Lprojection = [ccrs.PlateCarree()]*len(Lplot)
LaddWhite = [False]*len(Lplot)
fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, 
                                LaddWhite_cm=LaddWhite, Lproj=Lprojection, ax=[], Lcarte=[5, 8.25, 43.2,45.5])
Panel.save_graph(1,fig)

################################################################
#########          PANEL 1  # Horizontal cross-section V
###############################################################
Panel = PanelPlot(2,3, [25,14],'ERA5 prep_real MNH', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=22)

Lplot = [ Dvar['f1']['VT']]*6
Ltitle=['VT K='+str(i)+' ( '+str(Dvar['f1']['level'][i])+' m)' for i in Llvl]
Lminval, Lmaxval, Lstep, Lstepticks = [ [-10]*nb_pl, [30]*nb_pl, [0.5]*nb_pl, [5]*nb_pl ]
fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, 
                                LaddWhite_cm=LaddWhite, Lproj=Lprojection, ax=[], Lcarte=[5, 8.25, 43.2,45.5])
Panel.save_graph(2,fig)

################################################################
#########          PANEL 1  # Horizontal cross-section W
###############################################################
Panel = PanelPlot(2,3, [25,14],'ERA5 prep_real MNH', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=22)
Lcolormap = ['seismic']*len(Lplot)

Lplot = [ Dvar['f1']['WT']]*6
Ltitle=['WT K='+str(i)+' ( '+str(Dvar['f1']['level'][i])+' m)' for i in Llvl]
Lminval, Lmaxval, Lstep, Lstepticks = [ [-2]*nb_pl, [2]*nb_pl, [0.1]*nb_pl, [1]*nb_pl ]
fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, 
                                LaddWhite_cm=LaddWhite, Lproj=Lprojection, ax=[], Lcarte=[5, 8.25, 43.2,45.5])
Panel.save_graph(3,fig)

################################################################
#########          PANEL 1  # Horizontal cross-section THT
###############################################################
Panel = PanelPlot(2,3, [25,14],'ERA5 prep_real MNH', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=22)

Lcolormap = ['gist_rainbow_r']*len(Lplot)

Lplot = [ Dvar['f1']['THT']]*6
Ltitle=['THT K='+str(i)+' ( '+str(Dvar['f1']['level'][i])+' m)' for i in Llvl]
Lcbarlabel = ['K']*len(Lplot)
Lminval = [270, 270, 270, 300, 300, 430]
Lmaxval = [300, 300, 300, 330, 330, 490]
Lstep = [0.5]*6
Lstepticks = [5]*6
fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, 
                                LaddWhite_cm=LaddWhite, Lproj=Lprojection, ax=[], Lcarte=[5, 8.25, 43.2,45.5])
Panel.save_graph(4,fig)

################################################################
#########          PANEL 1  # Horizontal cross-section PABST
###############################################################
Panel = PanelPlot(2,3, [25,14],'ERA5 prep_real MNH', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=22)

Lplot = [ Dvar['f1']['PABST']]*6
Lcbarlabel = ['hPa']*len(Lplot)
Ltitle=['PABST K='+str(i)+' ( '+str(Dvar['f1']['level'][i])+' m)' for i in Llvl]
Lminval = [600, 600, 600, 420, 200, 50]
Lmaxval = [1010, 1010, 900, 650, 330, 66]
Lstep = [2]*6
Lstepticks = [50]*6
fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, 
                                LaddWhite_cm=LaddWhite, Lproj=Lprojection, ax=[], Lfacconv=[0.01]*6, Lcarte=[5, 8.25, 43.2,45.5])
Panel.save_graph(5,fig)

################################################################
#########          PANEL 1  # Horizontal cross-section RV
###############################################################
Panel = PanelPlot(2,3, [25,14],'ERA5 prep_real MNH', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=22)

Lplot = [ Dvar['f1']['RVT']]*6
Ltitle=['RVT K='+str(i)+' ( '+str(Dvar['f1']['level'][i])+' m)' for i in Llvl]
Lcbarlabel = ['g/kg']*len(Lplot)
Lminval, Lmaxval, Lstep, Lstepticks = [ [0]*nb_pl, [12]*nb_pl, [0.05]*nb_pl, [1]*nb_pl ]
fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, 
                                LaddWhite_cm=[True]*6, Lproj=Lprojection, ax=[], Lfacconv=[1000]*6, Lcarte=[5, 8.25, 43.2,45.5])
Panel.save_graph(6,fig)

################################################################
#########          PANEL 1  # Horizontal cross-section surface
###############################################################
Panel = PanelPlot(2,2, [20,20],'ERA5 prep_real MNH', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=22)

Lplot = [ Dvar['f1']['ZS'], Dvar['f1']['SST'], Dvar['f1']['TS_WATER'], Dvar['f1']['TSRAD_NAT']]
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Ltitle=['ZS','SST','TS_WATER','TSRAD_NAT']
Lcbarlabel = ['m','K','K','K']
Lminval = [0, 280, 260,265]
Lmaxval = [3000, 295, 300, 285]
Lstep = [25, 0.5, 0.5, 0.5]
Lstepticks = [250, 5, 5, 5]
LaddWhite = [False]*len(Lplot)
fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, 
                                LaddWhite_cm=LaddWhite, Lproj=Lprojection, ax=[], Lcarte=[5, 8.25, 43.2,45.5])
Panel.save_graph(7,fig)

################################################################
#########          PANEL 1  # Horizontal cross-section surface temp
###############################################################
Panel = PanelPlot(3,3, [25,20],'ERA5 prep_real MNH', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=22)

Lplot = [ Dvar['f1']['TG1P1'], Dvar['f1']['TG2P1'], Dvar['f1']['TROOF1'], Dvar['f1']['TWALL1'], Dvar['f1']['TROAD1'], Dvar['f1']['TIBLD1'], Dvar['f1']['TDEEP_TEB']]
lon = [Dvar['f1']['longitude']]*len(Lplot)
lat = [Dvar['f1']['latitude']]*len(Lplot)
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude']*len(Lplot)
Ltitle=['TG1P1','TG2P1','TROOF1','TWALL1','TROAD1','TIBLD1','TDEEP_TEB']
Lcbarlabel = ['K']*len(Lplot)
Lminval = [265, 265, 265, 265, 265, 290, 265]
Lmaxval = [285, 285, 285, 285, 285, 295, 285]
Lstep = [0.5]*len(Lplot)
Lstepticks = [5]*len(Lplot)
Lcolormap = ['RdBu']*len(Lplot)
Lprojection = [ccrs.PlateCarree()]*len(Lplot)
LaddWhite = [False]*len(Lplot)
fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, 
                                LaddWhite_cm=LaddWhite, Lproj=Lprojection, ax=[], Lcarte=[5, 8.25, 43.2,45.5])
Panel.save_graph(8,fig)
fig.tight_layout()

################################################################
#########          PANEL 1  # Horizontal surface water content
###############################################################
Panel = PanelPlot(1,3, [25,20],'ERA5 prep_real MNH', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=22)

Lplot = [ Dvar['f1']['WG1P1'], Dvar['f1']['WG2P1'], Dvar['f1']['WG3P1']]
lon = [Dvar['f1']['longitude']]*len(Lplot)
lat = [Dvar['f1']['latitude']]*len(Lplot)
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude']*len(Lplot)
Ltitle=['WG1P1','WG2P1','WG3P1']
Lcbarlabel = ['m3/m3']*len(Lplot)
Lminval = [0]*len(Lplot)
Lmaxval = [1]*len(Lplot)
Lstep = [0.01]*len(Lplot)
Lstepticks = [0.1]*len(Lplot)
Lcolormap = ['RdBu']*len(Lplot)
Lprojection = [ccrs.PlateCarree()]*len(Lplot)
LaddWhite = [False]*len(Lplot)
fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, 
                                LaddWhite_cm=LaddWhite, Lproj=Lprojection, ax=[], Lcarte=[5, 8.25, 43.2,45.5])
Panel.save_graph(9,fig)
