#!/usr/bin/env python3
"""
@author: Quentin Rodier
Creation : 07/01/2021

Last modifications
"""
import matplotlib as mpl
mpl.use('Agg')
import cartopy.crs as ccrs
try:
	import MNHPy
	from MNHPy.read_MNHfile import read_netcdf
	from MNHPy.Panel_Plot import PanelPlot
except:
	from read_MNHfile import read_netcdf
	from Panel_Plot import PanelPlot
import os

Lpath=["../006_mesonh_283TO3003/"]
LnameFiles=['A2830.1.MEGAD.006.nc']

variables=['COT','O3T','NO2T','NOT','BIOPT','HCHOT','BIOT','latitude','longitude',
           'ETHT', 'CARBOT','PHOT','SO2T','SULFT','DMST','CH4T','XO2T','PANT',
           'DEAERM31CT','DEAERM32CT','DEAERM31RT','DEAERM32RT',
           'SLTM01T','SLTM02T','SLTM03T','SLTM04T','SLTM05T','SLTM06T','SLTM07T','SLTM08T']
Dvar_input = {'f1': variables}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=Lpath, removeHALO=True, get_data_only=True)

os.system('rm -f tempgraph*')

f=Dvar['f1']
nb_subplot=4

lon = [Dvar['f1']['longitude']]*nb_subplot
lat = [Dvar['f1']['latitude']]*nb_subplot
Lxlab = ['longitude']*nb_subplot
Lylab = ['latitude']*nb_subplot
Lprojection = [ccrs.PlateCarree()]*nb_subplot
Llvl = [0]*nb_subplot

################################################################
#########          PANEL 1
###############################################################
Panel = PanelPlot(2,2, [20,20],'KTEST MEGAN Reunion K=0', minmaxpad=1.05)

Lplot=[f['BIOPT'],f['BIOT'],f['COT'],f['HCHOT']]
Ltitle = ['BIOPT','BIOT','COT','HCHOT']
Lminval=[0,      0,    30,    0.001 ]
Lmaxval=[15.0,  1000, 110,    1  ]
Lstep=[0.25,      20,   2,    0.04 ]
Lstepticks=[1,   100,  10,   0.1 ]
Lfacconv=[1E12,1E12,1E9,1E9    ]
Lcbarlabel = ['ppt','ppt','ppb','ppb']*nb_subplot
LaddWhite_cm=[True]*len(Lplot)
Lcolormap = ['gist_rainbow_r']*len(Lplot)

fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, Lcarte=[54.5,56,-22,-20], 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lproj=Lprojection, Lfacconv=Lfacconv, LaddWhite_cm=LaddWhite_cm)

Panel.save_graph(1,fig)

################################################################
#########          PANEL 2
###############################################################
Panel = PanelPlot(2,2, [20,20],'KTEST MEGAN Reunion K=0', minmaxpad=1.05)

Lplot=[f['O3T'],f['NO2T'],f['NOT'],f['ETHT']]
Ltitle = ['O3T','NO2T','NOT','ETHT']
Lminval=[15,      0,    30,    125 ]
Lmaxval=[25.0,  1000, 1000,    175  ]
Lstep=[0.1,      20,   20,    0.25 ]
Lstepticks=[1,   100,  100,   5 ]
Lfacconv=[1E9,1E12,1E12,1E12    ]
Lcbarlabel = ['ppb','ppt','ppt','ppt']*nb_subplot
LaddWhite_cm=[True]*len(Lplot)
Lcolormap = ['gist_rainbow_r']*len(Lplot)

fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, Lcarte=[54.5,56,-22,-20], 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lproj=Lprojection, Lfacconv=Lfacconv, LaddWhite_cm=LaddWhite_cm)

Panel.save_graph(2,fig)

################################################################
#########          PANEL 3
###############################################################
Panel = PanelPlot(2,2, [20,20],'KTEST MEGAN Reunion K=0', minmaxpad=1.05)

Lplot=[f['CARBOT'],f['PHOT'],f['SO2T'],f['SULFT']]
Ltitle = ['CARBOT','PHOT','SO2T','SULFT']
Lminval=[0,      0,    50,    0 ]
Lmaxval=[400,  0.7, 1000,    1  ]
Lstep=[5,      0.01,   10,    0.01 ]
Lstepticks=[100,   0.1,  100,   0.1 ]
Lfacconv=[1E12,1E12,1E12,1E12    ]
Lcbarlabel = ['ppt','ppt','ppt','ppt']*nb_subplot
LaddWhite_cm=[True]*len(Lplot)
Lcolormap = ['gist_rainbow_r']*len(Lplot)

fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, Lcarte=[54.5,56,-22,-20],
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lproj=Lprojection, Lfacconv=Lfacconv, LaddWhite_cm=LaddWhite_cm)

Panel.save_graph(3,fig)


################################################################
#########          PANEL 4
###############################################################
Panel = PanelPlot(2,2, [20,20],'KTEST MEGAN Reunion K=0', minmaxpad=1.05)

Lplot=[f['DMST'],f['CH4T'],f['XO2T'],f['PANT']]
Ltitle = ['DMST','CH4T','XO2T','PANT']
Lminval=[0,      0,    0.1,    0 ]
Lmaxval=[300,  0.1, 0.6,       500  ]
Lstep=[2,      0.001,   0.01,  5 ]
Lstepticks=[50,   0.01,  0.1,   100 ]
Lfacconv=[1E12,1E12,1E12,1E12    ]
Lcbarlabel = ['ppt','ppt','ppt','ppt']*nb_subplot
LaddWhite_cm=[True]*len(Lplot)
Lcolormap = ['gist_rainbow_r']*len(Lplot)

fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, Lcarte=[54.5,56,-22,-20],
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lproj=Lprojection, Lfacconv=Lfacconv, LaddWhite_cm=LaddWhite_cm)

Panel.save_graph(4,fig)

################################################################
#########          PANEL 5
###############################################################
Panel = PanelPlot(2,2, [20,20],'KTEST MEGAN Reunion K=0', minmaxpad=1.05)

Lplot=[f['DEAERM31CT'],f['DEAERM32CT'],f['DEAERM31RT'],f['DEAERM32RT']]
Ltitle = ['DEAERM31CT','DEAERM32CT','DEAERM31RT','DEAERM32RT']
Lminval=[0,      0,    0,    0 ]
Lmaxval=[4,  2, 1,    1  ]
Lstep=[0.005,     ]*4
Lstepticks=[0.5  ]*4
Lfacconv=[1E12,1E12,1E12,1E12    ]
Lcbarlabel = ['ppt','ppt','ppt','ppt']*nb_subplot
LaddWhite_cm=[True]*len(Lplot)
Lcolormap = ['gist_rainbow_r']*len(Lplot)

fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, Lcarte=[54.5,56,-22,-20],
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lproj=Lprojection, Lfacconv=Lfacconv, LaddWhite_cm=LaddWhite_cm)

Panel.save_graph(5,fig)

################################################################
#########          PANEL 6
###############################################################
Panel = PanelPlot(2,2, [20,20],'KTEST MEGAN Reunion K=0', minmaxpad=1.05)

Lplot=[f['SLTM01T'],f['SLTM02T'],f['SLTM03T'],f['SLTM04T']]
Ltitle = ['SLTM01T','SLTM02T','SLTM03T','SLTM04T']
Lminval=[0]*4
Lmaxval=[10]*4
Lstep=[0.01      ]*4
Lstepticks=[1  ]*4
Lfacconv=[1E18,1E18,1E18,1E18    ]
Lcbarlabel = ['1E-6 ppt','1E-6 ppt','1E-6 ppt','1E-6 ppt']*nb_subplot
LaddWhite_cm=[True]*len(Lplot)
Lcolormap = ['gist_rainbow_r']*len(Lplot)

fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, Lcarte=[54.5,56,-22,-20],
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lproj=Lprojection, Lfacconv=Lfacconv, LaddWhite_cm=LaddWhite_cm)

Panel.save_graph(6,fig)

################################################################
#########          PANEL 7
###############################################################
Panel = PanelPlot(2,2, [20,20],'KTEST MEGAN Reunion K=0', minmaxpad=1.05)

Lplot=[f['SLTM05T'],f['SLTM06T'],f['SLTM07T'],f['SLTM08T']]
Ltitle = ['SLTM05T','SLTM06T','SLTM07T','SLTM08T']
Lminval=[0 ]*4
Lmaxval=[1  ]*4
Lstep=[0.01  ]*4
Lstepticks=[0.1  ]*4
Lfacconv=[1E18,1E21,1E21,1E21    ]
Lcbarlabel = ['1E-6 ppt','1E-9 ppt','1E-9 ppt','1E-9 ppt']*nb_subplot
LaddWhite_cm=[True]*len(Lplot)
Lcolormap = ['gist_rainbow_r']*len(Lplot)

fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, Lcarte=[54.5,56,-22,-20],
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lproj=Lprojection, Lfacconv=Lfacconv, LaddWhite_cm=LaddWhite_cm)

Panel.save_graph(7,fig)

