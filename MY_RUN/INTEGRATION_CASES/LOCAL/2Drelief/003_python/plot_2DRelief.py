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
import copy

os.system('rm -f tempgraph*')
#
#  User's parameter / Namelist
#
path="../002_mesonh/"
LnameFiles = ['HYD2D.1.CEN4T.003.nc', 'HYD2D.1.CEN4T.004.nc']

Dvar_input = {
'f1':['ZS', 'UT', 'WT', 'THT', 'RVT','ni_u','level','ZTOP', 'ni','level_w','time'],
'f2':['ZS', 'UT', 'WT', 'THT', 'RVT','ni_u','level','ZTOP', 'ni','level_w','time']
}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path, removeHALO=True)

# Compute altitude variable in 2D with a 1D topography
Dvar['f1']['altitude'], Dvar['f1']['ni_u_2D'] = comp_altitude1DVar(Dvar['f1']['UT'], Dvar['f1']['ZS'],Dvar['f1']['ZTOP'], Dvar['f1']['level'],Dvar['f1']['ni_u'])
Dvar['f1']['altitude_w'], Dvar['f1']['ni_2D'] = comp_altitude1DVar(Dvar['f1']['WT'], Dvar['f1']['ZS'],Dvar['f1']['ZTOP'], Dvar['f1']['level_w'],Dvar['f1']['ni'])

orog = Dvar['f1']['ZS'][101:158]

################################################################
#########          PANEL 1 & 2
###############################################################
Panel1 = PanelPlot(2,2, [20,20],'2DRelief zoomed plots')

Lplot = [ Dvar['f1']['UT'][:,101:158], Dvar['f2']['UT'][:,101:158], Dvar['f1']['THT'][:,101:158], Dvar['f2']['THT'][:,101:158] ,
          Dvar['f1']['WT'][:,101:158], Dvar['f2']['WT'][:,101:158], Dvar['f1']['RVT'][:,101:158], Dvar['f2']['RVT'][:,101:158] ]

LaxeX = [Dvar['f1']['ni_u_2D'][:,101:158]]*2
LaxeX.extend([Dvar['f1']['ni_2D'][:,101:158] for i in range(6)]) #ni for all except UT

LaxeZ = [Dvar['f1']['altitude'][:,101:158]]*4
LaxeZ.extend([Dvar['f1']['altitude_w'][:,101:158] for i in range(2)])  #altitude_w only for WT
LaxeZ.extend([Dvar['f1']['altitude'][:,101:158] for i in range(2)])

Ltitle = ['u-wind UT', 'u-wind UT', 'Potential temperature THT', 'Potential temperature THT', 
          'Vertical velocity WT','Vertical velocity WT','Water vapor mixing ratio RVT', 'Water vapor mixing ratio RVT']
Lcbarlabel = ['m/s', 'm/s', 'K', 'K', 'm/s','m/s','g/kg','g/kg']
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,8000.1)]*len(Lplot)
Lminval = [5.2, 5.2, 284, 284, -0.4, -0.4, 0.2, 0.2]
Lmaxval = [15, 15, 330, 330, 0.28, 0.28, 4.0, 4.0]
Lstep = [0.4, 0.4, 2.5, 2.5, 0.04, 0.04, 0.2, 0.2]
Lstepticks = Lstep
Lfacconv = [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1000.0, 1000.0]
Ltime = [Dvar['f1']['time'], Dvar['f2']['time']]*4
LcolorLine = ['gist_rainbow_r']*len(Lplot)
Lpltype = ['c']*len(Lplot)
fig1 = Panel1.psectionV(Lxx=LaxeX[0:4], Lzz=LaxeZ[0:4], Lvar=Lplot[0:4], Lxlab=Lxlab[0:4], Lylab=Lylab[0:4], Ltitle=Ltitle[0:4],Lpltype=Lpltype,
                        Lminval=Lminval[0:4], Lmaxval=Lmaxval[0:4], Lstep=Lstep[0:4], Lstepticks=Lstepticks[0:4], LcolorLine=LcolorLine[0:4],
                        Lcbarlabel=Lcbarlabel[0:4], Lfacconv=Lfacconv[0:4], orog=orog, Lylim=Lylim[0:4], colorbar=False, Ltime=Ltime[0:4])

Panel1.save_graph(1,fig1)

Panel2 = PanelPlot(2,2, [20,20],'001_2DRelief zoomed plots')
Lcolormap = ['gist_rainbow_r']*len(Lplot)

fig2 = Panel2.psectionV(Lxx=LaxeX[:4], Lzz=LaxeZ[:4], Lvar=Lplot[:4], Lxlab=Lxlab[:4], Lylab=Lylab[:4], Ltitle=Ltitle[:4], 
                        Lminval=Lminval[:4], Lmaxval=Lmaxval[:4], Lstep=Lstep[:4], Lstepticks=Lstepticks[:4], Lcolormap=Lcolormap[:4],
                        Lcbarlabel=Lcbarlabel[:4], Lfacconv=Lfacconv[:4], orog=orog, Lylim=Lylim[:4], colorbar=True, Ltime=Ltime[:4])
Panel2.save_graph(2,fig2)

################################################################
#########          PANEL 3 & 4
###############################################################
Panel3 = PanelPlot(2,2, [20,20],'2DRelief full domain')

Lplot = [ Dvar['f1']['UT'][:,:], Dvar['f2']['UT'][:,:], Dvar['f1']['THT'][:,:], Dvar['f2']['THT'][:,:] ,
          Dvar['f1']['WT'][:,:], Dvar['f2']['WT'][:,:], Dvar['f1']['RVT'][:,:], Dvar['f2']['RVT'][:,:] ]

orog = Dvar['f1']['ZS'][:]
Lylim = [(0,15000.1)]*len(Lplot)
LaxeX = [Dvar['f1']['ni_u_2D'][:,:]]*2
LaxeX.extend([Dvar['f1']['ni_2D'][:,:] for i in range(6)]) #ni for all except UT

LaxeZ = [Dvar['f1']['altitude'][:,:]]*4
LaxeZ.extend([Dvar['f1']['altitude_w'][:,:] for i in range(2)])  #altitude_w only for WT
LaxeZ.extend([Dvar['f1']['altitude'][:,:] for i in range(2)])
Lpltype = ['c']*len(Lplot)

fig3 = Panel3.psectionV(Lxx=LaxeX[0:4], Lzz=LaxeZ[0:4], Lvar=Lplot[0:4], Lxlab=Lxlab[0:4], Lylab=Lylab[0:4], Ltitle=Ltitle[0:4], Lpltype=Lpltype,
                        Lminval=Lminval[0:4], Lmaxval=Lmaxval[0:4], Lstep=Lstep[0:4], Lstepticks=Lstepticks[0:4], LcolorLine=LcolorLine[0:4],
                        Lcbarlabel=Lcbarlabel[0:4], Lfacconv=Lfacconv[0:4], orog=orog, Lylim=Lylim[0:4], colorbar=False, Ltime=Ltime[0:4])

Panel3.save_graph(3,fig3)

Panel4 = PanelPlot(2,2, [20,20],'001_2DRelief full domain')
fig4 = Panel4.psectionV(Lxx=LaxeX[:4], Lzz=LaxeZ[:4], Lvar=Lplot[:4], Lxlab=Lxlab[:4], Lylab=Lylab[:4], Ltitle=Ltitle[:4], 
                        Lminval=Lminval[:4], Lmaxval=Lmaxval[:4], Lstep=Lstep[:4], Lstepticks=Lstepticks[:4], Lcolormap=Lcolormap[:4],
                        Lcbarlabel=Lcbarlabel[:4], Lfacconv=Lfacconv[:4], orog=orog, Lylim=Lylim[:4], colorbar=True, Ltime=Ltime[:4])
Panel4.save_graph(4,fig4)

