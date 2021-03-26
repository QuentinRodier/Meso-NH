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
from misc_functions import comp_altitude2DVar, windvec_verti_proj, mean_operator
import cartopy.crs as ccrs
import numpy as np
import math
import copy
import os

os.system('rm -f tempgraph*')
#
#  User's parameter / Namelist
#
output_name = '004_Reunion.pdf'                 #Name of the output PDF file
path=""

LnameFiles = ['REUNI.1.00A20.004dia.nc', 'REUNI.1.00A20.004.nc']

Dvar_input = {
'f1':['ZS', 'UT', 'VT', 'WT', 'THT', 'ALT_PRESSURE','ALT_U','ALT_V','ALT_THETA','level','ZTOP', 'longitude','latitude','level_w','time'],
'f2':['LSTHM', 'LSVM']}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path, removeHALO=True)

################################################################
#########          PANEL 1  # Horizontal cross-section
###############################################################
Panel1 = PanelPlot(2,2, [20,20],'004_Reunion horizontal sections')

Dvar['f1']['WIND'] = np.sqrt(Dvar['f1']['UT']**2 + Dvar['f1']['VT']**2)
Lplot = [ Dvar['f1']['ZS'][:,:], Dvar['f1']['WIND'][0,:,:], Dvar['f1']['ALT_THETA'][0,:,:], Dvar['f1']['ALT_PRESSURE'][0,:,:]]

LaxeX = [Dvar['f1']['longitude']]*len(Lplot)
LaxeY = [Dvar['f1']['latitude']]*len(Lplot)
Ltitle = ['Orography', 'Wind speed ','Potential temperature at z = 1500m', 'Pressure']
Lcbarlabel = ['m', 'm/s','K','hPa']
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude']*len(Lplot)
Lminval = [0, 8, 301.8, 831]
Lmaxval = [3000, 26, 305, 838]
Lstep = [50,1, 0.1, 0.1, 0.1]
Lstepticks = [500, 2,1,0.5]
Lfacconv = [1, 1, 1, 1./100.]
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Ltime = [Dvar['f1']['time']]*len(Lplot)
Lpltype = ['cf']*len(Lplot)
LaddWhite_cm = [True, False, False, False]
Lprojection = [ccrs.PlateCarree()]*len(Lplot)

fig1 = Panel1.psectionH(lon=LaxeX, lat=LaxeY, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                colorbar=True, Ltime=Ltime, LaddWhite_cm=LaddWhite_cm, Lproj=Lprojection)

Lplot1 = [ Dvar['f1']['UT'], Dvar['f1']['ALT_U']]
Lplot2 = [ Dvar['f1']['VT'], Dvar['f1']['ALT_V']]
Ltitle = ['wind vectors at K=2', 'wind vectors at z = 1500m ']
Lxlab = ['longitude']*len(Lplot1)
Lylab = ['latitude']*len(Lplot1)
Llegendval = [25,25]
Lcbarlabel = ['(m/s)']*len(Lplot1)
Larrowstep = [4]*len(Lplot1)
Lwidth = [0.003]*len(Lplot1)
Lcolor = ['black']*len(Lplot1)
Lprojection = [ccrs.PlateCarree()]*len(Lplot1)
Llvl = [0]*len(Lplot1)
Lscale = [400]*len(Lplot1)
fig2 = Panel1.pvector(Lxx=LaxeX, Lyy=LaxeY, Llevel=Llvl, Lvar1=Lplot1, Lvar2=Lplot2, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lwidth=Lwidth, Larrowstep=Larrowstep, 
                      Llegendval=Llegendval, Lcbarlabel=Lcbarlabel, Lproj=Lprojection, Lid_overlap=[2,6], ax=fig1.axes, Lscale=Lscale)



################################################################
#########          PANEL 2  # Vertical cross-section
###############################################################
#  Compute wind into mass point
tomass = mean_operator()
Dvar['f1']['WM'] = tomass.MZM(Dvar['f1']['WT'])
Dvar['f1']['VM'] = tomass.MYM(Dvar['f1']['VT'])

Panel2 = PanelPlot(2,2, [20,20],'004_Reunion vertical sections at i=35')
i_slice = 33

#  Black line
Panel1.addLine(fig2.axes[0],[Dvar['f1']['longitude'][0,i_slice],Dvar['f1']['latitude'][0,i_slice]],[Dvar['f1']['longitude'][-1,i_slice],Dvar['f1']['latitude'][-1,i_slice]],'black',3)
Panel1.save_graph(1,fig2)

# Compute altitude variable in 3D with a 2D topography
Dvar['f1']['altitude'] , Dvar['f1']['nx_3D'],  Dvar['f1']['ny_3D'] = comp_altitude2DVar(Dvar['f2']['LSTHM'], Dvar['f1']['ZS'],Dvar['f1']['ZTOP'], Dvar['f1']['level'], Dvar['f1']['latitude'],  Dvar['f1']['longitude'])
Dvar['f1']['altitude_w'], Dvar['f1']['nx_3D'], Dvar['f1']['ny_3D'] = comp_altitude2DVar(Dvar['f1']['WM'], Dvar['f1']['ZS'],Dvar['f1']['ZTOP'], Dvar['f1']['level_w'], Dvar['f1']['latitude'],  Dvar['f1']['longitude'])
Dvar['f1']['THT-LSTHM'] = copy.deepcopy(Dvar['f1']['THT'])
Dvar['f1']['THT-LSTHM'] = Dvar['f1']['THT'] - Dvar['f2']['LSTHM']
Dvar['f1']['VT-LSVM'] = copy.deepcopy(Dvar['f1']['VM'])
Dvar['f1']['VT-LSVM'] = Dvar['f1']['VM'] - Dvar['f2']['LSVM']

Lplot = [ Dvar['f1']['THT'][:,:,i_slice], Dvar['f1']['THT-LSTHM'][:,:,i_slice],Dvar['f1']['VT-LSVM'][:,:,i_slice],Dvar['f1']['WT'][:,:,i_slice]]
Ltitle = ['Potential Temperature', 'Anomalie de théta (THT-LSTHM)', 'Anomalie de V (VT-LSVM)', 'WT vertical velocity']
LaxeZ = [Dvar['f1']['altitude'][:,:,i_slice], Dvar['f1']['altitude'][:,:,i_slice],Dvar['f1']['altitude'][:,:,i_slice],Dvar['f1']['altitude_w'][:,:,i_slice]]
LaxeX = [Dvar['f1']['ny_3D'][:,:,i_slice]]*len(Lplot)
Lcbarlabel = ['K', 'K','m/s', 'm/s']
Lxlab = ['longitude']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,16000)]*len(Lplot)
Lminval = [300, -1.9, -10.5, -6.75]
Lmaxval = [355, 1.9, 10.5, 6.75]
Lstep = [2.5, 0.2, 1, 0.5]
Lstepticks = Lstep
Lcolormap=['gist_rainbow_r','seismic','seismic','seismic']
orog = Dvar['f1']['ZS'][:,i_slice]

fig3 = Panel2.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, Lylim=Lylim,
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel,
                                orog=orog, colorbar=True, Ltime=Ltime)

#  Wind vector on last panel
Lplot1 = [ Dvar['f1']['VM'][:,:,i_slice]]
Lplot2 = [ Dvar['f1']['WM'][:,:,i_slice]]
Ltitle = ['Wind']
Llegendval = [15]
Lcbarlabel = ['m/s']*len(Lplot)
Lxlab = ['longitude']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Larrowstep = [1]*len(Lplot)
Lwidth = [0.002]*len(Lplot)
Lscale = [800]*len(Lplot)
Lylim=[(0,3000)]
Lxlim = [(-21.3,-20.9)]*len(Lplot)
Lcolor=['lightgray']

fig4 = Panel2.pvector(Lxx=LaxeX, Lyy=LaxeZ, Lvar1=Lplot1, Lvar2=Lplot2, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lwidth=Lwidth, Larrowstep=Larrowstep, 
                        Llegendval=Llegendval, Lcbarlabel=Lcbarlabel, Lid_overlap=[6], ax=fig3.axes, Lscale=Lscale, Lylim=Lylim, Lxlim=Lxlim, Lcolor=Lcolor)

Panel2.save_graph(2,fig4)
