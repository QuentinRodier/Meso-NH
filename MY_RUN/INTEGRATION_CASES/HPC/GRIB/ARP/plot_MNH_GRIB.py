#!/usr/bin/env python3
"""

@author: Quentin Rodier
Creation : 07/01/2021

Last modifications
"""
import matplotlib as mpl
mpl.use('Agg')
from read_MNHfile import read_netcdf, read_withEPY
from Panel_Plot import PanelPlot
import cartopy.crs as ccrs
import os
import epygram
import numpy as np
from epygram.geometries.VGeometry import *

epygram.init_env()

import math
def find_nearest(array,value):
    idx = np.searchsorted(array, value, side="left")
    if idx==105 or idx==104:
        return 103
    else:
        if idx > 0 and (idx == len(array) or math.fabs(value - array[idx-1]) < math.fabs(value - array[idx])):
            return idx-1
        else:
            return idx

os.system('rm -f tempgraph*')
#
#  User's parameter / Namelist
#
LnameFiles = ['PREP_ARPEGE.nc','PREP_ARPEGEdg.nc']

Dvar_input = {
'f1':['ZS', 'SST', 'PABST','longitude','latitude'],
'f2':['THT950HPA','UT950HPA','VT950HPA',
      'THT700HPA','UT700HPA','VT700HPA',
      'THT500HPA','UT500HPA','VT500HPA',
      'THT300HPA','UT300HPA','VT300HPA']
}

var = [[0,2,119,3,i+1,'v'+str(i+1)] for i in range(104)]
u = [[0,2,119,2,i+1,'u'+str(i+1)] for i in range(104)]
t = [[0,0,119,0,i+1,'t'+str(i+1)] for i in range(104)]

var.append([0,3,1,0,0,'surfpressure'])
var.extend(u)
var.extend(t)
var.append([2,0,1,0,0,'landsea'])
var.append([0,193,1,5,0,'orog'])
LfilesEPY = ['arpifs.AN.20180513.00']
Dvar_inputEPY = {'fGRIB': var
#  GRIB2 list [discipline, parameterCategory, typeOfFirstFixedSurface, parameterNumber, level, casual name]
#  GRIB1 list [indicatorOfParameter, paramId, indicatorOfTypeOfLevel, level, casual name]
}

#  Read the variables in the files
Dvar = {}
Dvar = read_netcdf(LnameFiles, Dvar_input, path="", removeHALO=True)
Dvar = read_withEPY(LfilesEPY, Dvar_inputEPY, Dvar)

MNH_T950HPA = Dvar['f2']['THT950HPA']*((95000./100000.)**(287./1004.))
MNH_T700HPA = Dvar['f2']['THT700HPA']*((70000./100000.)**(287./1004.))
MNH_T500HPA = Dvar['f2']['THT500HPA']*((50000./100000.)**(287./1004.))
MNH_T300HPA = Dvar['f2']['THT300HPA']*((30000./100000.)**(287./1004.))

latcity=[45.16667, 43.70313]
loncity=[5.71667, 7.26608]
label=['Grenoble','Nice']

#  Surface pressure is in log in ARPEGE GRIB file
Dvar['fGRIB']['surfpressure'].data = np.exp(Dvar['fGRIB']['surfpressure'].data)
Dvar['fGRIB']['orog'].data = Dvar['fGRIB']['orog'].data/9.81

P=hybridP_coord_and_surfpressure_to_3D_pressure_field(Dvar['fGRIB']['v1'].geometry.vcoordinate, Dvar['fGRIB']['surfpressure'], 'geometric','mass')

level_cible_nearest=np.zeros([P.data.shape[1],P.data.shape[2]])
v950hPa_epy = Dvar['fGRIB']['v1'].data
u950hPa_epy = Dvar['fGRIB']['u2'].data
t950hPa_epy = Dvar['fGRIB']['t3'].data
v700hPa_epy = Dvar['fGRIB']['v4'].data
u700hPa_epy = Dvar['fGRIB']['u5'].data
t700hPa_epy = Dvar['fGRIB']['t6'].data
v500hPa_epy = Dvar['fGRIB']['v7'].data
u500hPa_epy = Dvar['fGRIB']['u8'].data
t500hPa_epy = Dvar['fGRIB']['t9'].data
v300hPa_epy = Dvar['fGRIB']['v10'].data
u300hPa_epy = Dvar['fGRIB']['u11'].data
t300hPa_epy = Dvar['fGRIB']['t12'].data

def interpol(P, idnear,Pcible, valnear, valnearmoinsun, valnearplusun):
  if P[idnear] < Pcible:
    valcible = valnear + (valnearplusun - valnear)*(Pcible-P[idnear]/(P[idnear+1]-P[idnear]))
  else:
    valcible = valnearmoinsun + (valnearmoinsun - valnear)*(P[idnear-1]-Pcible)/(P[idnear]-P[idnear-1])
  return valcible 

for i,x in enumerate(level_cible_nearest):
  for j,y in enumerate(x):
    #print(i)
    #print(j)
    Pcible=950
    level_cible_nearest[i,j]=find_nearest(P.data[:,i,j],Pcible)
    u950hPa_epy[i,j] = interpol(P.data[:,i,j], int(level_cible_nearest[i,j]), Pcible, Dvar['fGRIB']['u'+str(int(level_cible_nearest[i,j]))].data[i,j], Dvar['fGRIB']['u'+str(int(level_cible_nearest[i,j]-1))].data[i,j], Dvar['fGRIB']['u'+str(int(level_cible_nearest[i,j]+1))].data[i,j])
    v950hPa_epy[i,j] = interpol(P.data[:,i,j], int(level_cible_nearest[i,j]), Pcible, Dvar['fGRIB']['v'+str(int(level_cible_nearest[i,j]))].data[i,j], Dvar['fGRIB']['v'+str(int(level_cible_nearest[i,j]-1))].data[i,j], Dvar['fGRIB']['v'+str(int(level_cible_nearest[i,j]+1))].data[i,j])
    t950hPa_epy[i,j] = interpol(P.data[:,i,j], int(level_cible_nearest[i,j]), Pcible, Dvar['fGRIB']['t'+str(int(level_cible_nearest[i,j]))].data[i,j], Dvar['fGRIB']['t'+str(int(level_cible_nearest[i,j]-1))].data[i,j], Dvar['fGRIB']['t'+str(int(level_cible_nearest[i,j]+1))].data[i,j])
    
    Pcible=700
    level_cible_nearest[i,j]=find_nearest(P.data[:,i,j],Pcible)
    u700hPa_epy[i,j] = interpol(P.data[:,i,j], int(level_cible_nearest[i,j]), Pcible, Dvar['fGRIB']['u'+str(int(level_cible_nearest[i,j]))].data[i,j], Dvar['fGRIB']['u'+str(int(level_cible_nearest[i,j]-1))].data[i,j], Dvar['fGRIB']['u'+str(int(level_cible_nearest[i,j]+1))].data[i,j])
    v700hPa_epy[i,j] = interpol(P.data[:,i,j], int(level_cible_nearest[i,j]), Pcible, Dvar['fGRIB']['v'+str(int(level_cible_nearest[i,j]))].data[i,j], Dvar['fGRIB']['v'+str(int(level_cible_nearest[i,j]-1))].data[i,j], Dvar['fGRIB']['v'+str(int(level_cible_nearest[i,j]+1))].data[i,j])
    t700hPa_epy[i,j] = interpol(P.data[:,i,j], int(level_cible_nearest[i,j]), Pcible, Dvar['fGRIB']['t'+str(int(level_cible_nearest[i,j]))].data[i,j], Dvar['fGRIB']['t'+str(int(level_cible_nearest[i,j]-1))].data[i,j], Dvar['fGRIB']['t'+str(int(level_cible_nearest[i,j]+1))].data[i,j])
    
    Pcible=500
    level_cible_nearest[i,j]=find_nearest(P.data[:,i,j],Pcible)
    u500hPa_epy[i,j] = interpol(P.data[:,i,j], int(level_cible_nearest[i,j]), Pcible, Dvar['fGRIB']['u'+str(int(level_cible_nearest[i,j]))].data[i,j], Dvar['fGRIB']['u'+str(int(level_cible_nearest[i,j]-1))].data[i,j], Dvar['fGRIB']['u'+str(int(level_cible_nearest[i,j]+1))].data[i,j])
    v500hPa_epy[i,j] = interpol(P.data[:,i,j], int(level_cible_nearest[i,j]), Pcible, Dvar['fGRIB']['v'+str(int(level_cible_nearest[i,j]))].data[i,j], Dvar['fGRIB']['v'+str(int(level_cible_nearest[i,j]-1))].data[i,j], Dvar['fGRIB']['v'+str(int(level_cible_nearest[i,j]+1))].data[i,j])
    t500hPa_epy[i,j] = interpol(P.data[:,i,j], int(level_cible_nearest[i,j]), Pcible, Dvar['fGRIB']['t'+str(int(level_cible_nearest[i,j]))].data[i,j], Dvar['fGRIB']['t'+str(int(level_cible_nearest[i,j]-1))].data[i,j], Dvar['fGRIB']['t'+str(int(level_cible_nearest[i,j]+1))].data[i,j])
    
    Pcible=300
    level_cible_nearest[i,j]=find_nearest(P.data[:,i,j],Pcible)
    u300hPa_epy[i,j] = interpol(P.data[:,i,j], int(level_cible_nearest[i,j]), Pcible, Dvar['fGRIB']['u'+str(int(level_cible_nearest[i,j]))].data[i,j], Dvar['fGRIB']['u'+str(int(level_cible_nearest[i,j]-1))].data[i,j], Dvar['fGRIB']['u'+str(int(level_cible_nearest[i,j]+1))].data[i,j])
    v300hPa_epy[i,j] = interpol(P.data[:,i,j], int(level_cible_nearest[i,j]), Pcible, Dvar['fGRIB']['v'+str(int(level_cible_nearest[i,j]))].data[i,j], Dvar['fGRIB']['v'+str(int(level_cible_nearest[i,j]-1))].data[i,j], Dvar['fGRIB']['v'+str(int(level_cible_nearest[i,j]+1))].data[i,j])
    t300hPa_epy[i,j] = interpol(P.data[:,i,j], int(level_cible_nearest[i,j]), Pcible, Dvar['fGRIB']['t'+str(int(level_cible_nearest[i,j]))].data[i,j], Dvar['fGRIB']['t'+str(int(level_cible_nearest[i,j]-1))].data[i,j], Dvar['fGRIB']['t'+str(int(level_cible_nearest[i,j]+1))].data[i,j])

(lonGRIB,latGRIB) = Dvar['fGRIB']['surfpressure'].geometry.get_lonlat_grid()
(lonMNH,latMNH) = (Dvar['f1']['longitude'], Dvar['f1']['latitude'])
################################################################
#########          PANEL 1  
###############################################################
Panel = PanelPlot(2,3, [27,14],'General variables', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=22)

Lplot = [ Dvar['f1']['SST'], Dvar['fGRIB']['landsea'].data, Dvar['f1']['ZS'], Dvar['fGRIB']['orog'].data, Dvar['f1']['PABST'][0,:,:],Dvar['fGRIB']['surfpressure'].data]
lon = [lonMNH, lonGRIB, lonMNH, lonGRIB, lonMNH, lonGRIB]
lat = [latMNH, latGRIB, latMNH, latGRIB, latMNH, latGRIB]
Ltitle = ['SST MesoNH','Land-Sea mask ARPEGE', 'Orography MNH','Orography ARPEGE','MNH Ps','ARPEGE Ps']
Lcbarlabel = ['K','','m','m','hPa','hPa']
Lxlab = ['longitude']*len(Lplot)
Lylab = ['latitude']*len(Lplot)
Lminval = [287, 0, 0, 0, 700, 700]
Lmaxval = [295, 1, 4000, 4000,1020,1020]
Lstep = [0.2, 0.5, 50, 50, 1, 1]
Lstepticks = [1, 0.5, 500, 500, 50, 50]
Lcolormap = ['gist_rainbow_r','gist_rainbow_r','terrain','terrain','gist_rainbow_r','gist_rainbow_r']
Lprojection = [ccrs.PlateCarree()]*len(Lplot)
LaddWhite = [False]*len(Lplot)
Lfacconv = [1, 1, 1, 1, 1./100, 1./100.]
fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel,
                                LaddWhite_cm=LaddWhite, Lproj=Lprojection, ax=[], Lcarte=[5, 8.25, 43.2,45.5], Lfacconv=Lfacconv)

#Add departements + label cities point to each axes
for i in range(6):
  fig.axes[i*2].scatter(loncity,latcity, alpha=0.8, c='black', edgecolors='none')
  for lab, txt in enumerate(label):
    fig.axes[i*2].annotate(label[lab], (loncity[lab], latcity[lab]), color='black',size=10, weight="bold")

Panel.save_graph(1,fig)
################################################################
#########          PANEL 2 
###############################################################
Panel = PanelPlot(2,4, [30,14],'Temperature at isobaric surface', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=22)

Lplot = [ MNH_T950HPA, MNH_T700HPA, MNH_T500HPA, MNH_T300HPA, t950hPa_epy, t700hPa_epy, t500hPa_epy, t300hPa_epy]
lon = [lonMNH, lonMNH, lonMNH, lonMNH, lonGRIB, lonGRIB, lonGRIB, lonGRIB]
lat = [latMNH, latMNH, latMNH, latMNH, latGRIB, latGRIB, latGRIB, latGRIB]
Ltitle = ['MNH T 950hPa', 'MNH T700hPa', 'MNH T 500hPa', 'MNH T 300hPa', 'ARPEGE T 950hPa', 'ARPEGE T 700hPa', 'ARPEGE T 500hPa', 'ARPEGE T 300hPa']
Lcbarlabel = ['K']*len(Lplot)
Lxlab = ['']*len(Lplot)
Lylab = ['']*len(Lplot)
Lminval = [270, 265, 250, 220, 270, 265, 250, 220]
Lmaxval = [290, 275, 260, 230, 290, 275, 260, 230]
Lstep = [0.25]*len(Lplot)
Lstepticks = [2]*len(Lplot)
Lcolormap = ['gist_rainbow_r','seismic','seismic','seismic','gist_rainbow_r','seismic','seismic','seismic']
Lprojection = [ccrs.PlateCarree()]*len(Lplot)
LaddWhite = [False]*len(Lplot)
Lfacconv = [1]*len(Lplot)
fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel,
                                LaddWhite_cm=LaddWhite, Lproj=Lprojection, ax=[], Lcarte=[5, 8.25, 43.2,45.5], Lfacconv=Lfacconv)

Panel.save_graph(2,fig)
################################################################
#########          PANEL 3
###############################################################
Panel = PanelPlot(2,4, [30,14],'Zonal wind at isobaric surface', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=22)

Lplot = [ Dvar['f2']['UT950HPA'], Dvar['f2']['UT700HPA'], Dvar['f2']['UT500HPA'], Dvar['f2']['UT300HPA'], u950hPa_epy, u700hPa_epy, u500hPa_epy, u300hPa_epy]
Ltitle = ['MNH u 950hPa', 'MNH u 700hPa', 'MNH u 500hPa', 'MNH u 300hPa', 'ARPEGE u 950hPa', 'ARPEGE u 700hPa', 'ARPEGE u 500hPa', 'ARPEGE u 300hPa']
Lcbarlabel = ['m/s']*len(Lplot)
Lminval = [-10]*len(Lplot)
Lmaxval = [10]*len(Lplot)
Lstep = [0.25]*len(Lplot)
Lstepticks = [5]*len(Lplot)
Lcolormap = ['seismic']*len(Lplot)
fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel,
                                LaddWhite_cm=LaddWhite, Lproj=Lprojection, ax=[], Lcarte=[5, 8.25, 43.2,45.5], Lfacconv=Lfacconv)

Panel.save_graph(3,fig)
################################################################
#########          PANEL 4
###############################################################
Panel = PanelPlot(2,4, [30,14],'Meridionnal wind at isobaric surface', titlepad=25, minmaxpad=1.04, timepad=-0.07, colorbarpad=0.03, labelcolorbarpad = 13, colorbaraspect=22)

Lplot = [ Dvar['f2']['VT950HPA'], Dvar['f2']['VT700HPA'], Dvar['f2']['VT500HPA'],Dvar['f2']['VT300HPA'], v950hPa_epy, v700hPa_epy, v500hPa_epy, v300hPa_epy]
Ltitle = ['MNH v 950hPa', 'MNH v 700hPa', 'MNH v 500hPa', 'MNH v 300hPa', 'ARPEGE v 950hPa', 'ARPEGE v 700hPa', 'ARPEGE v 500hPa', 'ARPEGE v 300hPa']
Lcbarlabel = ['m/s']*len(Lplot)
Lminval = [-10, 0, 0, 0, -10, 0, 0, 0]
Lmaxval = [10, 20, 30, 30, 10, 20, 30, 30]
Lstep = [0.25]*len(Lplot)
Lstepticks = [5]*len(Lplot)
Lcolormap = ['seismic', 'gist_rainbow_r','seismic','seismic']*2
fig = Panel.psectionH(lon=lon, lat=lat, Lvar=Lplot, Lxlab=Lxlab, Lylab=Lylab, Ltitle=Ltitle, Lminval=Lminval, Lmaxval=Lmaxval,
                                Lstep=Lstep, Lstepticks=Lstepticks, Lcolormap=Lcolormap, Lcbarlabel=Lcbarlabel,
                                LaddWhite_cm=LaddWhite, Lproj=Lprojection, ax=[], Lcarte=[5, 8.25, 43.2,45.5], Lfacconv=Lfacconv)
Panel.save_graph(4,fig)

