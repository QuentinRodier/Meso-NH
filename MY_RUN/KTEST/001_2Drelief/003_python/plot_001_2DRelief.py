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
path=""
LnameFiles = ['EXPER.1.HYD2D.002.nc', 'EXPER.1.HYD2D.003.nc']

Dvar_input = {
'f1':['ZS', 'UT', 'WT', 'THT', 'RVT','LSUM','LSTHM','CFLU','CFLW','ni_u','level','ZTOP', 'ni','level_w','time'],
'f2':['ZS', 'UT', 'WT', 'THT', 'RVT','LSUM','LSTHM','CFLU','CFLW','ni_u','level','ZTOP', 'ni', 'level_w', 'time']
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
Panel1 = PanelPlot(2,2, [20,20],'001_2DRelief zoomed plots')

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
Lminval = [5.2, 5.2, 285, 285, -0.4, -0.4, 0.2, 0.2]
Lmaxval = [12.4, 12.4, 330, 330, 0.28, 0.28, 4.0, 4.0]
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
Panel3 = PanelPlot(2,2, [20,20],'001_2DRelief full domain')

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

################################################################
#########          PANEL 5
###############################################################
Panel5 = PanelPlot(2,2, [20,20],'001_2DRelief')
Lplot = [ Dvar['f1']['CFLU'][:,:], Dvar['f2']['CFLU'][:,:], Dvar['f1']['CFLW'][:,:], Dvar['f2']['CFLW'][:,:]]
LaxeX = [Dvar['f1']['ni_2D'][:,:]]*4
LaxeZ = [Dvar['f1']['altitude'][:,:]]*4
Ltitle = ['CFLU','CFLU','CFLW','CFLW']
Lcbarlabel = ['-']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,15000.1)]*len(Lplot)
Lminval = [0.03,0.03,0.004,0.004]
Lmaxval = [0.078,0.078,0.072,0.072]
Lstep = [0.003, 0.003, 0.004,0.004]
Lstepticks = Lstep
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Ltime = [Dvar['f1']['time'], Dvar['f2']['time']]*2

fig5 = Panel5.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, 
                                orog=orog, Lylim=Lylim, colorbar=True, Ltime=Ltime)
Panel5.save_graph(5,fig5)

################################################################
#########          PANEL 6
###############################################################
Dvar['f1']['altitude'], Dvar['f1']['ni_u_2D'] = comp_altitude1DVar(Dvar['f1']['UT'], Dvar['f1']['ZS'],Dvar['f1']['ZTOP'], Dvar['f1']['level'],Dvar['f1']['ni_u'])
Dvar['f1']['THT-LSTHM'] = copy.deepcopy(Dvar['f1']['THT'])
Dvar['f1']['THT-LSTHM'] = Dvar['f1']['THT'] - Dvar['f1']['LSTHM']

Dvar['f2']['altitude'], Dvar['f2']['ni_u_2D'] = comp_altitude1DVar(Dvar['f2']['UT'], Dvar['f2']['ZS'],Dvar['f2']['ZTOP'], Dvar['f2']['level'],Dvar['f2']['ni_u'])
Dvar['f2']['THT-LSTHM'] = copy.deepcopy(Dvar['f2']['THT'])
Dvar['f2']['THT-LSTHM'] = Dvar['f2']['THT'] - Dvar['f2']['LSTHM']

Dvar['f1']['UT-LSUM'] = copy.deepcopy(Dvar['f1']['UT'])
Dvar['f1']['UT-LSUM'] = Dvar['f1']['UT'] - Dvar['f1']['LSUM']

Dvar['f2']['UT-LSUM'] = copy.deepcopy(Dvar['f2']['UT'])
Dvar['f2']['UT-LSUM'] = Dvar['f2']['UT'] - Dvar['f2']['LSUM']

Panel6 = PanelPlot(2,2, [20,20],'Profiles at the center (top of the topography)')

Lplot = [ Dvar['f1']['WT'][:,127]]
LaxeZ = [Dvar['f1']['altitude'][:,127]]*len(Lplot)
Ltitle = ['WT and THT-LSTHM']*len(Lplot)
Llinelabel = ['WT']
Lxlab = ['velocity (m/s)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,15000.1)]*len(Lplot)
Lxlim = [(-0.3,0.3)]
Llinecolor = ['r']
LaxisColor = Llinecolor
Llvl = [0]*len(Lplot)
Ltime = [Dvar['f1']['time']]
fig6 = Panel6.pXY_lines(Lyy=LaxeZ, Lxx=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, 
                                Llinecolor=Llinecolor, LaxisColor=LaxisColor, Ltime=Ltime)

Lplot = [ Dvar['f1']['THT-LSTHM'][:,127]]
Ltitle = []
Llinelabel = ['THT - LSTHM ']
Lxlab = ['theta perturb (K)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,15000.1)]*len(Lplot)
Lxlim = [(-0.8,0.1)]
Llinecolor = ['g']
LaxisColor = Llinecolor
Llvl = [0]*len(Lplot)
fig7 = Panel6.pXY_lines(Lyy=LaxeZ, Lxx=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle,ax=fig6.axes, id_overlap=1, 
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor, Ltime=Ltime)

Lplot = [ Dvar['f2']['WT'][:,127]]
LaxeZ = [Dvar['f2']['altitude'][:,127]]*len(Lplot)
Ltitle = ['WT and THT-LSTHM']*len(Lplot)
Llinelabel = ['WT']
Lxlab = ['velocity (m/s)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,15000.1)]*len(Lplot)
Lxlim = [(-0.3,0.3)]
Llinecolor = ['r']
LaxisColor = Llinecolor
Llvl = [0]*len(Lplot)
Ltime = [Dvar['f2']['time']]
fig8 = Panel6.pXY_lines(Lyy=LaxeZ, Lxx=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, 
                                Llinecolor=Llinecolor, LaxisColor=LaxisColor, Ltime=Ltime, ax=fig7.axes)

Lplot = [ Dvar['f2']['THT-LSTHM'][:,127]]
Ltitle = []
Llinelabel = ['THT - LSTHM ']
Lxlab = ['theta perturb (K)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,15000.1)]*len(Lplot)
Lxlim = [(-0.1,0.2)]
Llinecolor = ['g']
LaxisColor = Llinecolor
Llvl = [0]*len(Lplot)
fig9 = Panel6.pXY_lines(Lyy=LaxeZ, Lxx=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, ax=fig8.axes, id_overlap=1, 
                        Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor, LaxisColor=LaxisColor, Ltime=Ltime)

# WT and UT-LSUM
Lplot = [ Dvar['f1']['WT'][:,127]]
LaxeZ = [Dvar['f1']['altitude'][:,127]]*len(Lplot)
Ltitle = ['WT and UT-LSUM']*len(Lplot)
Llinelabel = ['WT']
Lxlab = ['velocity (m/s)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,15000.1)]*len(Lplot)
Lxlim = [(-0.3,0.3)]
Llinecolor = ['r']
LaxisColor = Llinecolor
Llvl = [0]*len(Lplot)
Ltime = [Dvar['f1']['time']]
fig10 = Panel6.pXY_lines(Lyy=LaxeZ, Lxx=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, 
                                Llinecolor=Llinecolor, LaxisColor=LaxisColor, Ltime=Ltime, ax=fig9.axes)

Lplot = [ Dvar['f1']['UT-LSUM'][:,127]]
Ltitle = []
Llinelabel = ['UT - LSUM ']
Lxlab = ['u perturb (K)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,15000.1)]*len(Lplot)
Lxlim = [(-0.2,0.8)]
Llinecolor = ['g']
LaxisColor = Llinecolor
Llvl = [0]*len(Lplot)
fig11 = Panel6.pXY_lines(Lyy=LaxeZ, Lxx=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, ax=fig10.axes, id_overlap=1, 
                         Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor,LaxisColor=LaxisColor, Ltime=Ltime)

Lplot = [ Dvar['f2']['WT'][:,127]]
LaxeZ = [Dvar['f2']['altitude'][:,127]]*len(Lplot)
Ltitle = ['WT and UT-LSUM']*len(Lplot)
Llinelabel = ['WT']
Lxlab = ['velocity (m/s)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,15000.1)]*len(Lplot)
Lxlim = [(-0.3,0.3)]
Llinecolor = ['r']
LaxisColor = Llinecolor
Llvl = [0]*len(Lplot)
Ltime = [Dvar['f2']['time']]
fig12 = Panel6.pXY_lines(Lyy=LaxeZ, Lxx=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, 
                                Llinecolor=Llinecolor, LaxisColor=LaxisColor, Ltime=Ltime, ax=fig11.axes)

Lplot = [ Dvar['f2']['UT-LSUM'][:,127]]
Ltitle = []
Llinelabel = ['UT - LSUM ']
Lxlab = ['u perturb (K)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,15000.1)]*len(Lplot)
Lxlim = [(-2.0,2.0)]
Llinecolor = ['g']
LaxisColor = Llinecolor
Llvl = [0]*len(Lplot)
fig13 = Panel6.pXY_lines(Lyy=LaxeZ, Lxx=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle,ax=fig12.axes, id_overlap=1, 
                         Lylim=Lylim, Lxlim=Lxlim, Llinelabel=Llinelabel, Llinecolor=Llinecolor, LaxisColor=LaxisColor, Ltime=Ltime)

Panel6.save_graph(6,fig13)
