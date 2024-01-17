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
from misc_functions import *
import cartopy.crs as ccrs
import numpy as np
import os

os.system('rm -f tempgraph*')
#
#  User's parameter / Namelist
#
path=""

LnameFiles = ['REFid.1.D70Ca.001type.nc', 'REFid.1.D70Ca.002type.nc', 'REFid.1.D70Ca.003type.nc',
              'REFid.1.D70Ca.004type.nc', 'REFid.1.D70Ca.005type.nc', 'REFid.1.D70Ca.006type.nc',
              'REFid.1.D70Ca.007type.nc', 'REFid.1.D70Ca.008type.nc', 'REFid.1.D70Ca.009type.nc',
              'REFid.1.D70Ca.010type.nc', 
              'REFid.1.D70Cb.001type.nc', 'REFid.1.D70Cb.002type.nc', 'REFid.1.D70Cb.003type.nc',
              'REFid.1.D70Cb.004type.nc', 'REFid.1.D70Cb.005type.nc', 'REFid.1.D70Cb.006type.nc',
              'REFid.1.D70Cb.007type.nc', 'REFid.1.D70Cb.008type.nc']

Dvar_input = {
'f1':['SST','MSLP','UT','VT','ZS','RARE','latitude','longitude','level','ZTOP','ni_u','nj_u'],
'f2':['SST','MSLP','UT','VT','ZS','RARE','latitude','longitude','level'],
'f3':['SST','MSLP','UT','VT','ZS','RARE','latitude','longitude','level'],
'f4':['SST','MSLP','UT','VT','ZS','RARE','latitude','longitude','level'],
'f5':['SST','MSLP','UT','VT','ZS','RARE','latitude','longitude','level'],
'f6':['SST','MSLP','UT','VT','ZS','RARE','latitude','longitude','level'],
'f7':['SST','MSLP','UT','VT','ZS','RARE','latitude','longitude','level'],
'f8':['SST','MSLP','UT','VT','ZS','RARE','latitude','longitude','level'],
'f9':['SST','MSLP','UT','VT','ZS','RARE','latitude','longitude','level'],
'f10':['SST','MSLP','UT','VT','ZS','RARE','latitude','longitude','level'],
'f11':['SST','MSLP','UT','VT','ZS','RARE','latitude','longitude','level'],
'f12':['SST','MSLP','UT','VT','ZS','RARE','latitude','longitude','level'],
'f13':['SST','MSLP','UT','VT','ZS','RARE','latitude','longitude','level'],
'f14':['SST','MSLP','UT','VT','ZS','RARE','latitude','longitude','level'],
'f15':['SST','MSLP','UT','VT','ZS','RARE','latitude','longitude','level'],
'f16':['SST','MSLP','UT','VT','ZS','RARE','latitude','longitude','level'],
'f17':['SST','MSLP','UT','VT','ZS','RARE','latitude','longitude','level'],
'f18':['SST','MSLP','UT','VT','ZS','RARE','latitude','longitude','level']
}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path=path, removeHALO=True)

for ifile in Dvar_input.keys():
  Dvar[ifile]['WIND'] = np.sqrt(Dvar[ifile]['UT']**2 + Dvar[ifile]['VT']**2)

################################################################
#########          PANEL 1
###############################################################
Panel = PanelPlot(3,3, [25,25],'Pmer contours on SST shading', titlepad=20, minmaxpad=1.04, timepad=-0.10, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=35, lateralminmaxpad=1.02)

Lplot = [ Dvar['f1']['SST']-273.15,Dvar['f2']['SST']-273.15, Dvar['f3']['SST']-273.15,
          Dvar['f4']['SST']-273.15,Dvar['f5']['SST']-273.15, Dvar['f6']['SST']-273.15,
          Dvar['f7']['SST']-273.15,Dvar['f8']['SST']-273.15, Dvar['f9']['SST']-273.15]

lon = [Dvar['f1']['longitude']]*len(Lplot)
lat = [Dvar['f1']['latitude']]*len(Lplot)
Ltitle = ['SST']*len(Lplot)
Lcbarlabel = ['K']*len(Lplot)
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude']*len(Lplot)
Lminval = [22]*len(Lplot)
Lmaxval = [30.25]*len(Lplot)
Lstep = [0.25]*len(Lplot)
Lstepticks = Lstep
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Lprojection = [ccrs.PlateCarree()]*len(Lplot)
LaddWhite = [False]*len(Lplot)
Ltime = [Dvar['f1']['date'], Dvar['f2']['date'], Dvar['f3']['date'], Dvar['f4']['date'], Dvar['f5']['date'], Dvar['f6']['date'],
         Dvar['f7']['date'], Dvar['f8']['date'], Dvar['f9']['date']]

fig1 = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, 
                                Ltime=Ltime, LaddWhite_cm=LaddWhite, Lproj=Lprojection)

# over Pmer
Lplot = [ Dvar['f1']['MSLP'],Dvar['f2']['MSLP'], Dvar['f3']['MSLP'],
          Dvar['f4']['MSLP'],Dvar['f5']['MSLP'], Dvar['f6']['MSLP'],
          Dvar['f7']['MSLP'],Dvar['f8']['MSLP'], Dvar['f9']['MSLP']]

Lminval = [940]*len(Lplot)
Lmaxval = [1010]*len(Lplot)
Lstep = [5]*len(Lplot)
Ltitle = ['Pmer']*len(Lplot)
Lstepticks = Lstep
Lpltype = ['c']*len(Lplot)
LcolorLine=['black']*len(Lplot)
fig2 = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,colorbar=False, 
                                Lstep=Lstep, Lstepticks=Lstepticks, LcolorLine=LcolorLine, Lpltype=Lpltype,
                                Ltime=Ltime, LaddWhite_cm=LaddWhite, Lproj=Lprojection, Lid_overlap=[0, 2, 4, 6, 8, 10, 12, 14, 16], ax=fig1.axes)
Panel.save_graph(1,fig2)

################################################################
#########          PANEL 2
###############################################################
Panel = PanelPlot(3,3, [25,25],'Pmer contours on SST shading', titlepad=20, minmaxpad=1.04, timepad=-0.10, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=35, lateralminmaxpad=1.02)

Lplot = [ Dvar['f10']['SST']-273.15,Dvar['f11']['SST']-273.15, Dvar['f12']['SST']-273.15,
          Dvar['f13']['SST']-273.15,Dvar['f14']['SST']-273.15, Dvar['f15']['SST']-273.15,
          Dvar['f16']['SST']-273.15,Dvar['f17']['SST']-273.15, Dvar['f18']['SST']-273.15]

lon = [Dvar['f1']['longitude']]*len(Lplot)
lat = [Dvar['f1']['latitude']]*len(Lplot)
Ltitle = ['SST']*len(Lplot)
Lcbarlabel = ['K']*len(Lplot)
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude']*len(Lplot)
Lminval = [22]*len(Lplot)
Lmaxval = [30.25]*len(Lplot)
Lstep = [0.25]*len(Lplot)
Lstepticks = Lstep
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Lprojection = [ccrs.PlateCarree()]*len(Lplot)
LaddWhite = [False]*len(Lplot)
Ltime = [Dvar['f10']['date'], Dvar['f11']['date'], Dvar['f12']['date'], Dvar['f13']['date'], Dvar['f14']['date'], Dvar['f15']['date'],
         Dvar['f16']['date'], Dvar['f17']['date'], Dvar['f18']['date']]

fig1 = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval, 
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel, 
                                Ltime=Ltime, LaddWhite_cm=LaddWhite, Lproj=Lprojection)

# over Pmer
Lplot = [ Dvar['f10']['MSLP'],Dvar['f11']['MSLP'], Dvar['f12']['MSLP'],
          Dvar['f13']['MSLP'],Dvar['f14']['MSLP'], Dvar['f15']['MSLP'],
          Dvar['f16']['MSLP'],Dvar['f17']['MSLP'], Dvar['f18']['MSLP']]

Lminval = [940]*len(Lplot)
Lmaxval = [1010]*len(Lplot)
Lstep = [5]*len(Lplot)
Ltitle = ['Pmer']*len(Lplot)
Lstepticks = Lstep
Lpltype = ['c']*len(Lplot)
LcolorLine=['black']*len(Lplot)
fig2 = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,colorbar=False, 
                                Lstep=Lstep, Lstepticks=Lstepticks, LcolorLine=LcolorLine, Lpltype=Lpltype,
                                Ltime=Ltime, LaddWhite_cm=LaddWhite, Lproj=Lprojection, Lid_overlap=[0, 2, 4, 6, 8, 10, 12, 14, 16], ax=fig1.axes)
Panel.save_graph(2,fig2)

################################################################
#########          PANEL 3
###############################################################

Panel = PanelPlot(3,3, [25,25],'Reflectivity at 533m', titlepad=20, minmaxpad=1.04, timepad=-0.10, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=35)

Lplot = [ Dvar['f1']['RARE'],Dvar['f2']['RARE'], Dvar['f3']['RARE'],
          Dvar['f4']['RARE'],Dvar['f5']['RARE'], Dvar['f6']['RARE'],
          Dvar['f7']['RARE'],Dvar['f8']['RARE'], Dvar['f9']['RARE']]

lon = [Dvar['f1']['longitude']]*len(Lplot)
lat = [Dvar['f1']['latitude']]*len(Lplot)
Ltitle = ['RARE at 533m']*len(Lplot)
Lcbarlabel = ['dBZ']*len(Lplot)
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude']*len(Lplot)
Lminval = [0]*len(Lplot)
Lmaxval = [60]*len(Lplot)
Lstep = [2.5]*len(Lplot)
Lstepticks = Lstep
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Lprojection = [ccrs.PlateCarree()]*len(Lplot)
LaddWhite = [True]*len(Lplot)
Llevel = [12]*len(Lplot)
Ltime = [Dvar['f1']['date'], Dvar['f2']['date'], Dvar['f3']['date'], Dvar['f4']['date'], Dvar['f5']['date'], Dvar['f6']['date'],
         Dvar['f7']['date'], Dvar['f8']['date'], Dvar['f9']['date']]

fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel,
                                Ltime=Ltime, LaddWhite_cm=LaddWhite, Lproj=Lprojection, Llevel = Llevel)

Panel.save_graph(3,fig)

################################################################
#########          PANEL 4
###############################################################

Panel = PanelPlot(3,3, [25,25],'Reflectivity at 533m', titlepad=20, minmaxpad=1.04, timepad=-0.10, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=35)

Lplot = [ Dvar['f10']['RARE'],Dvar['f11']['RARE'], Dvar['f12']['RARE'],
          Dvar['f13']['RARE'],Dvar['f14']['RARE'], Dvar['f15']['RARE'],
          Dvar['f16']['RARE'],Dvar['f17']['RARE'], Dvar['f18']['RARE']]

lon = [Dvar['f1']['longitude']]*len(Lplot)
lat = [Dvar['f1']['latitude']]*len(Lplot)
Ltitle = ['RARE at 533m']*len(Lplot)
Lcbarlabel = ['dBZ']*len(Lplot)
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude']*len(Lplot)
Lminval = [0]*len(Lplot)
Lmaxval = [60]*len(Lplot)
Lstep = [2.5]*len(Lplot)
Lstepticks = Lstep
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Lprojection = [ccrs.PlateCarree()]*len(Lplot)
LaddWhite = [True]*len(Lplot)
Llevel = [12]*len(Lplot)
Ltime = [Dvar['f10']['date'], Dvar['f11']['date'], Dvar['f12']['date'], Dvar['f13']['date'], Dvar['f14']['date'], Dvar['f15']['date'],
         Dvar['f16']['date'], Dvar['f17']['date'], Dvar['f18']['date']]

fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel,
                                Ltime=Ltime, LaddWhite_cm=LaddWhite, Lproj=Lprojection, Llevel = Llevel)

Panel.save_graph(4,fig)

################################################################
#########          PANEL 5
###############################################################

Panel = PanelPlot(3,3, [25,25],'Wind speed at 533m', titlepad=20, minmaxpad=1.04, timepad=-0.10, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=35)

Lplot = [ Dvar['f1']['WIND'],Dvar['f2']['WIND'], Dvar['f3']['WIND'],
          Dvar['f4']['WIND'],Dvar['f5']['WIND'], Dvar['f6']['WIND'],
          Dvar['f7']['WIND'],Dvar['f8']['WIND'], Dvar['f9']['WIND']]

lon = [Dvar['f1']['longitude']]*len(Lplot)
lat = [Dvar['f1']['latitude']]*len(Lplot)
Ltitle = ['Wind speed']*len(Lplot)
Lcbarlabel = ['m/s']*len(Lplot)
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude']*len(Lplot)
Lminval = [0]*len(Lplot)
Lmaxval = [60]*len(Lplot)
Lstep = [2.5]*len(Lplot)
Lstepticks = Lstep
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Lprojection = [ccrs.PlateCarree()]*len(Lplot)
LaddWhite = [True]*len(Lplot)
Llevel = [12]*len(Lplot)
Ltime = [Dvar['f1']['date'], Dvar['f2']['date'], Dvar['f3']['date'], Dvar['f4']['date'], Dvar['f5']['date'], Dvar['f6']['date'],
         Dvar['f7']['date'], Dvar['f8']['date'], Dvar['f9']['date']]

fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel,
                                Ltime=Ltime, LaddWhite_cm=LaddWhite, Lproj=Lprojection, Llevel = Llevel)

Panel.save_graph(5,fig)

################################################################
#########          PANEL 6
###############################################################

Panel = PanelPlot(3,3, [25,25],'Wind speed at 533m', titlepad=20, minmaxpad=1.04, timepad=-0.10, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=35)

Lplot = [ Dvar['f10']['WIND'],Dvar['f11']['WIND'], Dvar['f12']['WIND'],
          Dvar['f13']['WIND'],Dvar['f14']['WIND'], Dvar['f15']['WIND'],
          Dvar['f16']['WIND'],Dvar['f17']['WIND'], Dvar['f18']['WIND']]

lon = [Dvar['f1']['longitude']]*len(Lplot)
lat = [Dvar['f1']['latitude']]*len(Lplot)
Ltitle = ['Wind speed']*len(Lplot)
Lcbarlabel = ['m/s']*len(Lplot)
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude']*len(Lplot)
Lminval = [0]*len(Lplot)
Lmaxval = [60]*len(Lplot)
Lstep = [2.5]*len(Lplot)
Lstepticks = Lstep
Lcolormap = ['gist_rainbow_r']*len(Lplot)
Lprojection = [ccrs.PlateCarree()]*len(Lplot)
LaddWhite = [True]*len(Lplot)
Llevel = [12]*len(Lplot)
Ltime = [Dvar['f10']['date'], Dvar['f11']['date'], Dvar['f12']['date'], Dvar['f13']['date'], Dvar['f14']['date'], Dvar['f15']['date'],
         Dvar['f16']['date'], Dvar['f17']['date'], Dvar['f18']['date']]

fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel,
                                Ltime=Ltime, LaddWhite_cm=LaddWhite, Lproj=Lprojection, Llevel = Llevel)

Panel.save_graph(6,fig)
#Lplot1 = [Dvar['f1']['UT'],Dvar['f2']['UT'], Dvar['f3']['UT'],
#          Dvar['f4']['UT'],Dvar['f5']['UT'], Dvar['f6']['UT'],
#          Dvar['f7']['UT'],Dvar['f8']['UT'], Dvar['f9']['UT']]

#Lplot2 = [Dvar['f1']['VT'],Dvar['f2']['VT'], Dvar['f3']['VT'],
#          Dvar['f4']['VT'],Dvar['f5']['VT'], Dvar['f6']['VT'],
#          Dvar['f7']['VT'],Dvar['f8']['VT'], Dvar['f9']['VT']]

#  StreamLine pas terrible : peu de lignes et > 2h de calcul !
#Ldensity = [50]*len(Lplot1)
#Llinewidth = [1]*len(Lplot1)
#fig = Panel.pstreamline(Lxx=lon, Lyy=lat, Llevel=Llevel, Lvar1=Lplot1, Lvar2=Lplot2, Lxlab=Lxlab, Lylab=Lylab, 
#                        Ltitle=Ltitle, Ldensity=Ldensity, Lproj=Lprojection, Llinewidth=Llinewidth, Ltime=Ltime)


################################################################
#########          PANEL 7
###############################################################
Dvar['f1']['altitude'], Dvar['f1']['latitude_3D'],  Dvar['f1']['longitude_3D'] = comp_altitude2DVar(Dvar['f1']['UT'], Dvar['f1']['ZS'],Dvar['f1']['ZTOP'], Dvar['f1']['level'], Dvar['f1']['latitude'], Dvar['f1']['longitude'])

Panel = PanelPlot(3,3, [25,25],'Coupe verticale Sud-Nord i=120', titlepad=20, minmaxpad=1.03, lateralminmaxpad=0.95, timepad=-0.10, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=35)

dim_i2 = int(Dvar['f1']['WIND'].shape[2]/2)

Lplot = [ Dvar['f1']['VT'][:,:,dim_i2],Dvar['f2']['VT'][:,:,dim_i2], Dvar['f3']['VT'][:,:,dim_i2],
          Dvar['f4']['VT'][:,:,dim_i2-1],Dvar['f5']['VT'][:,:,dim_i2-2], Dvar['f6']['VT'][:,:,dim_i2-3],
          Dvar['f7']['VT'][:,:,dim_i2-4],Dvar['f8']['VT'][:,:,dim_i2-5], Dvar['f9']['VT'][:,:,dim_i2-6]]
# Le decalage de 2 points permet de centrer un peu plus la coupe sur le centre du cyclone qui se decale avec le temps

LaxeZ = [Dvar['f1']['altitude'][:,:,dim_i2]]*len(Lplot)
LaxeX = [Dvar['f1']['longitude_3D'][:,:,dim_i2]]*len(Lplot)

Lxlab = ['latitude']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,22000)]*len(Lplot)
Ltitle=['Meridional Wind']*len(Lplot)
Lcbarlabel = ['m/s']*len(Lplot)
Lminval = [-35.25]*len(Lplot)
Lmaxval = [35.25]*len(Lplot)
Lstep = [2.5]*len(Lplot)
Lcolormap = ['seismic']*len(Lplot)
LaddWhite=[False]*len(Lplot)
fig = Panel.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                      Lylim=Lylim, Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, colorbar=True, 
                      LaddWhite_cm=LaddWhite, Ltime=Ltime, Lcbarlabel=Lcbarlabel)

Lplot = [ Dvar['f1']['RARE'][:,:,dim_i2],Dvar['f2']['RARE'][:,:,dim_i2], Dvar['f3']['RARE'][:,:,dim_i2],
          Dvar['f4']['RARE'][:,:,dim_i2-1],Dvar['f5']['RARE'][:,:,dim_i2-2], Dvar['f6']['RARE'][:,:,dim_i2-3],
          Dvar['f7']['RARE'][:,:,dim_i2-4],Dvar['f8']['RARE'][:,:,dim_i2-5], Dvar['f9']['RARE'][:,:,dim_i2-6]]

Lminval = [0]*len(Lplot)
Lmaxval = [60]*len(Lplot)
Lstep = [10]*len(Lplot)
Lpltype=['c']*len(Lplot)
LcolorLine=['black']*len(Lplot)
Ltitle=['Reflectivity isoline']*len(Lplot)
fig = Panel.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                      Lylim=Lylim, Lstep=Lstep, colorbar=False, LcolorLine=LcolorLine, Lpltype=Lpltype, Ltime=Ltime,
                      Lid_overlap=[0, 2, 4, 6, 8, 10, 12, 14, 16], ax=fig.axes)

fig.tight_layout()
Panel.save_graph(7,fig)

################################################################
#########          PANEL 8
###############################################################
Dvar['f1']['altitude'], Dvar['f1']['latitude_3D'],  Dvar['f1']['longitude_3D'] = comp_altitude2DVar(Dvar['f1']['UT'], Dvar['f1']['ZS'],Dvar['f1']['ZTOP'], Dvar['f1']['level'], Dvar['f1']['latitude'], Dvar['f1']['longitude'])

Panel = PanelPlot(3,3, [25,25],'Coupe verticale Sud-Nord i=120', titlepad=20, minmaxpad=1.03, lateralminmaxpad=0.95, timepad=-0.10, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=35)

dim_i2 = int(Dvar['f1']['WIND'].shape[2]/2)

Lplot = [ Dvar['f10']['VT'][:,:,dim_i2-7],Dvar['f11']['VT'][:,:,dim_i2-8], Dvar['f12']['VT'][:,:,dim_i2-9],
          Dvar['f13']['VT'][:,:,dim_i2-10],Dvar['f14']['VT'][:,:,dim_i2-11], Dvar['f15']['VT'][:,:,dim_i2-12],
          Dvar['f16']['VT'][:,:,dim_i2-13],Dvar['f17']['VT'][:,:,dim_i2-14], Dvar['f18']['VT'][:,:,dim_i2-15]]

LaxeZ = [Dvar['f1']['altitude'][:,:,dim_i2]]*len(Lplot)
LaxeX = [Dvar['f1']['longitude_3D'][:,:,dim_i2]]*len(Lplot)
Ltime = [Dvar['f10']['date'], Dvar['f11']['date'], Dvar['f12']['date'], Dvar['f13']['date'], Dvar['f14']['date'], Dvar['f15']['date'],
         Dvar['f16']['date'], Dvar['f17']['date'], Dvar['f18']['date']]
Lxlab = ['latitude']*len(Lplot)
Lylab = ['altitude (m)']*len(Lplot)
Lylim = [(0,22000)]*len(Lplot)
Ltitle=['Meridional Wind']*len(Lplot)
Lcbarlabel = ['m/s']*len(Lplot)
Lminval = [-35.25]*len(Lplot)
Lmaxval = [35.25]*len(Lplot)
Lstep = [2.5]*len(Lplot)
Lstepticks=Lstep*len(Lplot)
Lcolormap = ['seismic']*len(Lplot)
LaddWhite=[False]*len(Lplot)
fig = Panel.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                      Lylim=Lylim, Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, colorbar=True, 
                      LaddWhite_cm=LaddWhite, Ltime=Ltime, Lcbarlabel=Lcbarlabel)

Lplot = [ Dvar['f10']['RARE'][:,:,dim_i2-7],Dvar['f11']['RARE'][:,:,dim_i2-8], Dvar['f12']['RARE'][:,:,dim_i2-9],
          Dvar['f13']['RARE'][:,:,dim_i2-10],Dvar['f14']['RARE'][:,:,dim_i2-11], Dvar['f15']['RARE'][:,:,dim_i2-12],
          Dvar['f16']['RARE'][:,:,dim_i2-13],Dvar['f17']['RARE'][:,:,dim_i2-14], Dvar['f18']['RARE'][:,:,dim_i2-15]]

Lminval = [0]*len(Lplot)
Lmaxval = [60]*len(Lplot)
Lstep = [10]*len(Lplot)
Lstepticks=Lstep*len(Lplot)
Lpltype=['c']*len(Lplot)
LcolorLine=['black']*len(Lplot)
Ltitle=['Reflectivity isoline']*len(Lplot)
fig = Panel.psectionV(Lxx=LaxeX, Lzz=LaxeZ, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                      Lylim=Lylim, Lstep=Lstep, colorbar=False, LcolorLine=LcolorLine, Lpltype=Lpltype, Ltime=Ltime,
                      Lid_overlap=[0, 2, 4, 6, 8, 10, 12, 14, 16], ax=fig.axes)

fig.tight_layout()
Panel.save_graph(8,fig)
