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

Lpath=["../005_mesonh_tst1/","../005_mesonh_tst2/","../005_mesonh_tst3/","../005_mesonh_tst4/"]
LnameFiles=['CHARM.1.SEG01.002.nc']*4

variables=['CO','O3','NO2','NO','ISOP','BIOL','HCHO','BIOH','latitude','longitude']
Dvar_input = {'f1': variables,'f2': variables,'f3': variables,'f4': variables}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=Lpath, removeHALO=True)

os.system('rm -f tempgraph*')


lon = [Dvar['f1']['longitude']]*4
lat = [Dvar['f1']['latitude']]*4
Ltitle = ['LCH_BIO_FLUX=F','LCH_BIO_FLUX=F et CPARAMBVOC=','LCH_BIO_FLUX=T et CPARAMBVOC=SOLMON','LCH_BIO_FLUX=T et CPARAMBVOC=MEGAN']
Lcbarlabel = ['ppb']*4
Lxlab = ['longitude']*4
Lylab = ['latitude']*4
Lcolormap = ['gist_rainbow_r']*4
Lprojection = [ccrs.PlateCarree()]*4
Llvl = [0]*4

var=['BIOH','BIOL','CO','HCHO','ISOP','NO2','NO','O3']
minval=[0,0,96,0.60,0,0.8,0,19.5]
maxval=[10.0,5.,210,2.4,0.005,23,0.33,55.5]
stepval=[0.25,0.01,2,0.04,0.0001,0.4,0.01,0.5]
stepticksval=[1,0.1,12,0.24,0.0002,2.6,0.05,5]
Lfaconvval=[1E12,1E12,1E9,1E9,1E12,1E9,1E9,1E9]


#Dvar['f3']['BIOHT'] = Dvar['f3']['BIOHT']/1000.0
#Dvar['f3']['BIOLT'] = Dvar['f3']['BIOLT']/1000.0
#Dvar['f3']['ISOPT'] = Dvar['f3']['ISOPT']/1000.0

#
for i,v in enumerate(var):
################################################################
#########          PANEL n
###############################################################
	Panel1 = PanelPlot(2,2, [20,20],'KTEST MEGAN, variable = '+v, minmaxpad=1.05)
	Lplot = [ Dvar['f1'][v],Dvar['f2'][v], Dvar['f3'][v],Dvar['f4'][v]]
	Lminval = [minval[i]]*len(Lplot)
	Lmaxval = [maxval[i]]*len(Lplot)
	Lstep = [stepval[i]]*len(Lplot)
	Lstepticks = [stepticksval[i]]*len(Lplot)
	Lfacconv = [Lfaconvval[i]]*len(Lplot)
	fig1 = Panel1.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lcarte=[], Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lproj=Lprojection, Lfacconv=Lfacconv)
	Panel1.save_graph(i,fig1)

