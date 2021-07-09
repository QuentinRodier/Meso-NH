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
from misc_functions import oblique_proj, windvec_verti_proj, mean_operator
import os

os.system('rm -f tempgraph*')
#
#  User's parameter / Namelist
#
path=""
LnameFiles = ['KWRAI.1.SEGCH.004.nc','KWRAI.1.SEGCH.004dg.nc' ]

Dvar_input = {
'f1':['ZS', 'UT','VT', 'WT','THT',
      'ni_u','nj_u','level','ZTOP', 'ni','nj','level_w','time',
      'INPRR','ACPRR','PABST','RCT','RVT','RRT','RGT', 'LSTHM',
      'COT','O3T','HCHOT','WC_HCHOT','IC_HCHOT', 'WR_HCHOT','SO2T','WC_SO2T','WR_SO2T',
      'IC_SO2T','HNO3T','WC_HNO3T','WR_HNO3T','IC_HNO3T','PHC','PHR',],  
'f2':['ALT_CLOUD', 'ALT_U', 'ALT_V', 'ni','nj']
}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path, removeHALO=True)

################################################################
#########          PANEL 1
###############################################################
Panel1 = PanelPlot(2,3, [25,14],'', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.01, lateralminmaxpad=0.9)

Lplot = [ Dvar['f1']['INPRR'], Dvar['f1']['ACPRR'], Dvar['f1']['PABST'],Dvar['f2']['ALT_CLOUD'],Dvar['f2']['ALT_CLOUD'] ]

LaxeX = [Dvar['f1']['ni']]*len(Lplot)
LaxeY = [Dvar['f1']['nj']]*len(Lplot)
Ltitle = ['Instantaneous precipitation INPRR', 'Accumulated precipitation ACPRR','Absolute pressure','Mixing ratio of liquid droplets at z=3000m','Mixing ratio of liquid droplets at z=5000m' ]
Lcbarlabel = ['mm/h', 'mm','hPa','g/kg','g/kg']
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['y (m)']*len(Lplot)
Lminval = [0, 0, 982.25, 0, 0]
Lmaxval = [13.5, 0.36, 983.45, 2.5, 1.8]
Lstep = [0.5, 0.005, 0.05, 0.05, 0.05]
Lstepticks = [0.5, 0.02, 0.2, 0.5, 0.5]
Lfacconv = [1, 1, 1./100.0,1,1]
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Llvl = [0,0,0,0,1]
Ltime = [Dvar['f1']['time']]*len(Lplot)
LaddWhite = [True]*len(Lplot)
Lpltype = ['cf']*len(Lplot)

fig1 = Panel1.psectionH(lon=LaxeX, lat=LaxeY, Lvar=Lplot, Llevel=Llvl, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                Ltime=Ltime, LaddWhite_cm=LaddWhite)
fig1.tight_layout()

# Wind vectors
Lplot1 = [ Dvar['f1']['UT'],  Dvar['f2']['ALT_U'], Dvar['f2']['ALT_U']]
Lplot2 = [ Dvar['f1']['VT'], Dvar['f2']['ALT_V'], Dvar['f2']['ALT_V']]
Ltitle = ['Wind at K=2', 'Wind at 3000m', 'Wind at 5000m']
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['y (m)']*len(Lplot)
Llegendval = [10,10,10]
Lcbarlabel = ['m/s']*len(Lplot)
Larrowstep = [1]*len(Lplot)
Lwidth = [0.002]*len(Lplot)
Lcolor = ['black']*len(Lplot)
Llvl = [0,0,1]
lon = [Dvar['f1']['ni_u'], Dvar['f2']['ni'], Dvar['f2']['ni'] ]
lat = [Dvar['f1']['nj_u'], Dvar['f2']['nj'],  Dvar['f2']['nj'] ]
Lscale = [200]*len(Lplot)
fig2 = Panel1.pvector(Lxx=lon, Lyy=lat, Lvar1=Lplot1, Lvar2=Lplot2, Lcarte=[500,23500,500,23500], Llevel=Llvl, 
                      Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lwidth=Lwidth, Larrowstep=Larrowstep, 
                      Lcolor=Lcolor, Llegendval=Llegendval, Lcbarlabel=Lcbarlabel, Lid_overlap=[4,6,8], ax=fig1.axes, Lscale=Lscale)
#  Oblique projection
i_beg, j_beg = (2,0)
i_end, j_end = (21,22)
#  Black line
Panel1.addLine(fig2.axes[6],[Dvar['f1']['ni'][i_beg],Dvar['f1']['nj'][j_beg]],[Dvar['f1']['ni'][i_end],Dvar['f1']['nj'][j_end]],'black',2)
Panel1.save_graph(1,fig2)

################################################################
#########          PANEL 2
###############################################################
Panel2 = PanelPlot(2,2, [20,20],'', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.01, colorbaraspect=40, labelcolorbarpad = 13)

# Interpoler COT','O3T à 3000 et 5000m avec une moyenne sur 2 niveaux
Dvar['f1']['COT3000m'] = (Dvar['f1']['COT'][6,:,:] + Dvar['f1']['COT'][5,:,:])/2.0
Dvar['f1']['O3T3000m'] = (Dvar['f1']['O3T'][6,:,:] + Dvar['f1']['O3T'][5,:,:])/2.0
Dvar['f1']['COT5000m'] = (Dvar['f1']['COT'][10,:,:] + Dvar['f1']['COT'][9,:,:])/2.0
Dvar['f1']['O3T5000m'] = (Dvar['f1']['O3T'][10,:,:] + Dvar['f1']['O3T'][9,:,:])/2.0
Lplot = [Dvar['f1']['COT3000m'], Dvar['f1']['O3T3000m'],Dvar['f1']['COT5000m'], Dvar['f1']['O3T5000m'] ]

LaxeX = [Dvar['f1']['ni']]*len(Lplot)
LaxeY = [Dvar['f1']['nj']]*len(Lplot)
Ltitle = ['CO at z = 3000m', 'Ozone O3 at z = 3000m ', 'CO at z = 5000m', 'Ozone O3 at z = 5000m ']
Lcbarlabel = ['kg/kg']*len(Lplot)
Lxlab = ['x (m)']*len(Lplot)
Lylab = ['y (m)']*len(Lplot)
Lminval = [0, 0, 0, 0]
Lmaxval = [1E-7, 0.4E-7, 0.95E-7, 0.4E-7]
Lstep = [0.05E-7, 0.02E-7, 0.05E-7, 0.02E-7]
Lstepticks = Lstep
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Ltime = [Dvar['f1']['time']]*len(Lplot)
LaddWhite = [True]*len(Lplot)

fig2 = Panel2.psectionH(lon=LaxeX, lat=LaxeY, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, 
                                Ltime=Ltime, LaddWhite_cm=LaddWhite)

Panel2.save_graph(2,fig2)

################################################################
#########          PANEL 3 : Oblique projection
###############################################################
Panel3 = PanelPlot(2,2, [17,17],'Oblique section (angle = 47)', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.01, lateralminmaxpad=0.9)

Dvar['f1']['THT-LSTHM'] = Dvar['f1']['THT'] - Dvar['f1']['LSTHM']

tomass = mean_operator()
Dvar['f1']['UM'] = tomass.MXM(Dvar['f1']['UT'])
Dvar['f1']['VM'] = tomass.MYM(Dvar['f1']['VT'])
Dvar['f1']['WM'] = tomass.MZM(Dvar['f1']['WT'])

angle_sec1, RVT_sec1, axe_m1 = oblique_proj(Dvar['f1']['RVT'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
WIND_proj = windvec_verti_proj(Dvar['f1']['UM'], Dvar['f1']['VM'], Dvar['f1']['level'], angle_sec1)
angle_sec1, WIND_sec1, axe_m1 = oblique_proj(WIND_proj, Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec1, WT_sec1, axe_m1 = oblique_proj(Dvar['f1']['WM'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec1, RCT_sec1, axe_m1 = oblique_proj(Dvar['f1']['RCT'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec1, RRT_sec1, axe_m1 = oblique_proj(Dvar['f1']['RRT'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec1, anoTHT_sec1, axe_m1 = oblique_proj(Dvar['f1']['THT-LSTHM'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)

Lplot = [RVT_sec1, RCT_sec1, anoTHT_sec1, WT_sec1]
LaxeX = [axe_m1]*len(Lplot)
LaxeZ = [Dvar['f1']['level'], Dvar['f1']['level'], Dvar['f1']['level'],Dvar['f1']['level_w']]
Ltitle = ['Water vapor mixing ratio', 'Liquid cloud mixing ratio', 'Potential temperature anomaly', 'Vertical velocity']
Lcbarlabel = ['g/kg','g/kg', 'K', 'm/s']
Lxlab = ['distance (m)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lminval = [0., 0., -5, -18]
Lmaxval = [15, 3.0, 5, 18]
Lstep = [0.5, 0.05, 0.2, 0.5]
Lstepticks = [1, 0.2, 1, 2.5]
Lfacconv = [1000, 1000, 1, 1]
Lcolormap = ['gist_rainbow_r','gist_rainbow_r', 'seismic','seismic']
Ltime = [Dvar['f1']['time']]*len(Lplot)
Lpltype = ['cf']*len(Lplot)
LaddWhite = [True, True, False, False]

fig3 = Panel3.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                Ltime=Ltime, Lpltype=Lpltype, LaddWhite_cm=LaddWhite)

Lplot1 = [ WIND_sec1]
Lplot2 = [ WT_sec1]
Ltitle = ['Wind']
Llegendval = [25]
Lcbarlabel = ['m/s']*len(Lplot)
Larrowstep = [1]*len(Lplot)
Lwidth = [0.004]*len(Lplot)
Lscale = [200]*len(Lplot)

fig4 = Panel3.pvector(Lxx=LaxeX, Lyy=LaxeZ, Lvar1=Lplot1, Lvar2=Lplot2, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lwidth=Lwidth, Larrowstep=Larrowstep, 
                        Llegendval=Llegendval, Lcbarlabel=Lcbarlabel, Lid_overlap=[0], ax=fig3.axes, Lscale=Lscale)

Lplot = [RRT_sec1]
LaxeX = [axe_m1]
LaxeZ = [Dvar['f1']['level']]
Ltitle = ['precipitation']
Lcbarlabel = ['g/kg']*len(Lplot)
Lxlab = ['distance (m)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,10000.1)]*len(Lplot)
Lminval = [0.]
Lmaxval = [3.5]
Lstep = [0.5]
Lstepticks = [0.5]
Lfacconv = [1000]*len(Lplot)
LcolorLine = ['black']*len(Lplot)
Ltime = [Dvar['f1']['time']]
Lpltype = ['c']*len(Lplot)
LaddWhite = [True]*len(Lplot)

fig5 = Panel3.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, LcolorLine=LcolorLine, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                Ltime=Ltime, Lpltype=Lpltype, LaddWhite_cm=LaddWhite, ax=fig4.axes,Lid_overlap=[2],colorbar=False)
Panel3.save_graph(3,fig5)

################################################################
#########          PANEL 4 : Oblique projection of chemical variables
###############################################################
Panel4 = PanelPlot(2,3, [25,14],'Oblique projection of chemical variables', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.01, colorbaraspect=40, labelcolorbarpad = 13)

angle_sec1, RGT_sec1, axe_m1 = oblique_proj(Dvar['f1']['RGT'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec1, COT_sec1, axe_m1 = oblique_proj(Dvar['f1']['COT'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec1, O3T_sec1, axe_m1 = oblique_proj(Dvar['f1']['O3T'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec1, HCHOT_sec1, axe_m1 = oblique_proj(Dvar['f1']['HCHOT'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec1, WC_HCHOT_sec1, axe_m1 = oblique_proj(Dvar['f1']['WC_HCHOT'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec1, WR_HCHOT_sec1, axe_m1 = oblique_proj(Dvar['f1']['WR_HCHOT'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec1, SO2T_sec1, axe_m1 = oblique_proj(Dvar['f1']['SO2T'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec1, WC_SO2T_sec1, axe_m1 = oblique_proj(Dvar['f1']['WC_SO2T'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec1, WR_SO2T_sec1, axe_m1 = oblique_proj(Dvar['f1']['WR_SO2T'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec1, IC_SO2T_sec1, axe_m1 = oblique_proj(Dvar['f1']['IC_SO2T'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec1, IC_HCHOT_sec1, axe_m1 = oblique_proj(Dvar['f1']['IC_HCHOT'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec1, HNO3T_sec1, axe_m1 = oblique_proj(Dvar['f1']['HNO3T'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec1, WC_HNO3T_sec1, axe_m1 = oblique_proj(Dvar['f1']['WC_HNO3T'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec1, WR_HNO3T_sec1, axe_m1 = oblique_proj(Dvar['f1']['WR_HNO3T'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec1, IC_HNO3T_sec1, axe_m1 = oblique_proj(Dvar['f1']['IC_HNO3T'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec1, PHC_sec1, axe_m1 = oblique_proj(Dvar['f1']['PHC'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)
angle_sec1, PHR_sec1, axe_m1 = oblique_proj(Dvar['f1']['PHR'], Dvar['f1']['ni'], Dvar['f1']['nj'], Dvar['f1']['level'], i_beg, j_beg, i_end, j_end)

Lplot = [COT_sec1, HCHOT_sec1, SO2T_sec1, HNO3T_sec1, PHC_sec1, PHR_sec1 ]

LaxeX = [axe_m1]*len(Lplot)
LaxeZ = [Dvar['f1']['level']]*len(Lplot)
Ltitle = ['CO', 'HCHO', 'SO2', 'HNO3', 'pH in cloud','pH in rain']
Lcbarlabel = ['ppp', 'ppp', 'ppp','ppp', '', '' ]
Lxlab = ['distance (m)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lminval = [0., 0., 0., 0., 0., 0.]
Lmaxval = [1E-7, 0.23E-9, 0.51E-10, 0.13E-9, 6, 6]
Lstep = [0.05E-7, 0.1E-10, 0.25E-11, 0.01E-9, 0.25, 0.25]
Lstepticks = Lstep
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Ltime = [Dvar['f1']['time']]*len(Lplot)
Lpltype = ['cf']*len(Lplot)
LaddWhite = [True]*len(Lplot)

fig6 = Panel4.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, 
                                Ltime=Ltime, Lpltype=Lpltype, LaddWhite_cm=LaddWhite)
fig1.tight_layout()
Panel4.save_graph(4,fig6)

################################################################
#########          PANEL 5 : Oblique projection of chemical variables
###############################################################
Panel5 = PanelPlot(3,3, [25,14],'', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.01, colorbaraspect=40, labelcolorbarpad = 13)

Lplot = [WC_HCHOT_sec1, WR_HCHOT_sec1, IC_HCHOT_sec1, WC_SO2T_sec1, WR_SO2T_sec1, IC_SO2T_sec1, WC_HNO3T_sec1, WR_HNO3T_sec1, IC_HNO3T_sec1]

LaxeX = [axe_m1]*len(Lplot)
LaxeZ = [Dvar['f1']['level']]*len(Lplot)
Ltitle = ['WC_HCHO', 'WR_HCHO', 'IC_HCHO','WC_SO2', 'WR_SO2', 'IC_SO2','WC_HNO3', 'WR_HNO3', 'IC_HNO3']
Lcbarlabel = ['ppp']*len(Lplot)
Lxlab = ['distance (m)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lminval = [0., 0., 0., 0., 0., 0., 0., 0., 0.]
Lmaxval = [0.6E-10, 1E-10, 0.17E-9, 0.9E-11, 0.68E-11, 0.62E-12, 0.85E-10, 0.12E-9, 0.18E-9]
Lstep = [0.3E-11, 0.05E-10, 0.1E-10, 0.5E-12, 0.04E-11, 0.25E-13, 0.05E-10, 0.05E-10, 0.1E-10]
Lstepticks = Lstep
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Ltime = [Dvar['f1']['time']]*len(Lplot)
Lpltype = ['cf']*len(Lplot)
LaddWhite = [True]*len(Lplot)

fig7 = Panel5.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, 
                                Ltime=Ltime, Lpltype=Lpltype, LaddWhite_cm=LaddWhite)
fig7.tight_layout()

Lplot = [RCT_sec1, RRT_sec1, RGT_sec1, RCT_sec1, RRT_sec1, RGT_sec1, RCT_sec1, RRT_sec1, RGT_sec1 ]
LaxeX = [axe_m1]*len(Lplot)
LaxeZ = [Dvar['f1']['level']]*len(Lplot)
Ltitle = ['cloud mixing ratio (g/kg)', 'precipitation mixing ratio (g/kg)', 'graupel mixing ratio (g/kg)']*3
Lcbarlabel = ['g/kg']*len(Lplot)
Lxlab = ['distance (m)']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,10000.1)]*len(Lplot)
Lminval = [0.]*len(Lplot)
Lmaxval = [6]*len(Lplot)
Lstep = [0.5]*len(Lplot)
Lstepticks = [0.5]*len(Lplot)
Lfacconv = [1000]*len(Lplot)
LcolorLine = ['black']*len(Lplot)
Lpltype = ['c']*len(Lplot)

fig8 = Panel5.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, LcolorLine=LcolorLine, Lcbarlabel=Lcbarlabel, Lfacconv=Lfacconv, 
                                Lpltype=Lpltype, ax=fig7.axes,Lid_overlap=[0, 2, 4, 6, 8, 10, 12, 14, 16],colorbar=False)

Panel5.save_graph(5,fig8)