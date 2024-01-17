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
from misc_functions import comp_altitude2DVar
import os

os.system('rm -f tempgraph*')
#
#  User's parameter / Namelist
#
path="../002_mesonh/"

LnameFiles = ['REL3D.1.CEN4T.002.nc']

Dvar_input = {'f1':['ZS', 'UT', 'WT', 'VT','ni_u','nj_u','level','ZTOP', 'ni','nj','level_w','time']}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path, removeHALO=True)

# Compute altitude variable in 3D with a 2D topography
Dvar['f1']['altitude'], Dvar['f1']['ni_u_3D'],  Dvar['f1']['nj_u_3D'] = comp_altitude2DVar(Dvar['f1']['UT'], Dvar['f1']['ZS'],Dvar['f1']['ZTOP'], Dvar['f1']['level'], Dvar['f1']['nj_u'], Dvar['f1']['ni_u'])
Dvar['f1']['altitude_w'], Dvar['f1']['ni_3D'], Dvar['f1']['nj_3D'] = comp_altitude2DVar(Dvar['f1']['WT'], Dvar['f1']['ZS'],Dvar['f1']['ZTOP'], Dvar['f1']['level_w'],Dvar['f1']['nj'], Dvar['f1']['ni'])

orog = Dvar['f1']['ZS'][15,:]

################################################################
#########          PANEL 1
###############################################################
Panel1 = PanelPlot(2,2, [20,20],'3DRelief horizontal sections')
Lplot = [ Dvar['f1']['UT'][2,:,:], Dvar['f1']['UT'][13,:,:], Dvar['f1']['VT'][2,:,:], Dvar['f1']['VT'][13,:,:] ]
LaxeX = [Dvar['f1']['ni_3D'][2,:,:]]*len(Lplot)
LaxeZ = [Dvar['f1']['nj_3D'][13,:,:]]*len(Lplot)
Ltitle = ['u-wind UT K=375m', 'u-wind VT K=3125m','v-wind UT K=375m', 'v-wind VT K=3125m']
Lcbarlabel = ['m/s']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['y (m)']*len(Lplot)
Lminval = [7.75, 5.2, -2.5, -1.4]
Lmaxval = [12.5, 11.6, 2.5, 1.4]
Lstep = [0.05, 0.25, 0.1, 0.1]
Lstepticks = [0.5, 0.5, 0.5, 0.5]
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Ltime = [Dvar['f1']['time']]*len(Lplot)
fig1 = Panel1.psectionH(lon=LaxeX, lat=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel,
                                colorbar=True, Ltime=Ltime )

Panel1.save_graph(1,fig1)

Panel2 = PanelPlot(2,2, [20,20],'3DRelief vertical sections')

Lplot = [ Dvar['f1']['UT'][:,50,:], Dvar['f1']['WT'][:,50,:]]
LaxeX = [Dvar['f1']['ni_u_3D'][:,50,:], Dvar['f1']['ni_3D'][:,50,:]]
LaxeZ = [Dvar['f1']['altitude'][:,50,:], Dvar['f1']['altitude_w'][:,50,:]]
Ltitle = ['u-wind UT', 'v-wind VT',]
Lcbarlabel = ['m/s']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,17500.1)]*len(Lplot)
Lminval = [5, -1.2]
Lmaxval = [16, 0.9]
Lstep = [0.25, 0.1]
Lstepticks = [2, 0.25]
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Ltime = [Dvar['f1']['time']]*len(Lplot)
fig2 = Panel2.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel,
                                orog=orog, Lylim=Lylim, colorbar=True, Ltime=Ltime)

Panel2.save_graph(2,fig2)
