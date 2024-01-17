#!/usr/bin/env python3
"""

@author: Quentin Rodier
Creation : 07/01/2021

Last modifications
"""

import epygram
import matplotlib as mpl
mpl.use('Agg')
import cartopy.crs as ccrs
import matplotlib.pyplot as plt
import numpy as np
import os

epygram.init_env()

def plot2D_epy(nb_l, nb_c, Lsfig, Lvar, Ltitle, Llonlat, minval, maxval, colormap, cstep, proj, id_plt,center_color,plt_meth,bigtitle=""):
  fig = plt.figure(figsize=(Lsfig[0],Lsfig[1]))
  fig.set_dpi(400)
  fig.suptitle(bigtitle,fontsize=14)
  for i,var in enumerate(Lvar):
    ax = fig.add_subplot(nb_l,nb_c,i+1, projection = proj)
    fig,ax = var.cartoplot(fig, ax=ax, minmax=[minval,maxval],colormap=colormap,colorstep=cstep,plot_method=plt_meth,center_cmap_on_0=center_color)
    ax.set_title(Ltitle[i])
    if(Llonlat != []): #  [] means global plot (no zoom)
      ax.set_extent(Llonlat, crs=ccrs.PlateCarree())
  id_plt+=1
  fig.savefig('tempgraph'+str(id_plt)) #png is out and then convert to PDF at the end
  return id_plt
  
def read_files(LnameFiles,Dvar_input):
  Dvar = {}
  for i,nameFiles in enumerate(LnameFiles):
    f_nb = 'f' + str(i+1)
    print('Reading file ' + f_nb)
    theFile = epygram.formats.resource(nameFiles,'r')
    Dvar[f_nb] = {} #initialize dic for each files 
    for var in Dvar_input[f_nb]: #For each files
      #  Read variables
      if(theFile.format == 'FA'):
        Dvar[f_nb][var] = theFile.readfield(var)
      elif(theFile.format == 'LFI'):
        if(var[1]==None or var[1]==0): # 2D Field
          Dvar[f_nb][var[0]] = theFile.readfield(var)
        else: # 3D Field
          Dvar[f_nb][var[0]+str(var[1])] = theFile.readfield(var).getlevel(k=var[1])
      elif(theFile.format == 'netCDFMNH'):
        if(var[1]==None or var[1]==0): # 2D Field
          Dvar[f_nb][var[0]] = theFile.readfield(var[0])
        else:
          Dvar[f_nb][var[0]+str(var[1])] = theFile.readfield(var[0]).getlevel(k=var[1])
      elif(theFile.format == 'GRIB'):
        Dvar[f_nb][var[5]] =  theFile.readfield({'discipline': var[0], 'parameterCategory': var[1], 'typeOfFirstFixedSurface': var[2],'parameterNumber': var[3], 'level': var[4]})
      else:
       raise epygramError("Unknown format file, please use FA, LFI, GRIB2 or MNH NetCDF")
    theFile.close()
  return Dvar #Return the dic of [files][variables]

#
#
#  User's parameter / Namelist
#
#
output_name = 'graphs.pdf'                 #Name of the output PDF file
LnameFiles = ['INIT_SURF.20180513.00.lfi','INI_ARO_GRIB2.lfi','INI_ARO_GRIB2.nc','analyse','analyse.grb'] #List of the different files
Dvar_input = {
'f2':[('THT',1),('THT',15),('UT',1)], #  NetCDF tuple (variable, k level). For 2D field : k=0
'f3':[('THT',1),('THT',15),('UT',1)], #  NetCDF tuple (variable, k level). For 2D fiel :, k=0
'f1':[('SST',0) ], #  LFI tuple (variable, k level). For 2D field, k=0
'f4':['S090WIND.U.PHYS'],   #  FA string variable
'f5':[[0,19,119,11,90,'tke'],[0,2,119,2,90,'u']]  #  GRIB2 list (discipline, parameterCategory, typeOfFirstFixedSurface, parameterNumber, level, casual name to use here)
}
projection = ccrs.PlateCarree() #Projection type of cartopy

#  Read the variables in the files
Dvar = {}
Dvar = read_files(LnameFiles, Dvar_input)

#  Transform spectral data to physics space
for f in Dvar:
  for var in Dvar[f]:
    if(Dvar[f][var].spectral):
      Dvar[f][var].sp2gp()

#  Manipulate data
#Dvar['f1']['S090TKE'].setdata(Dvar['f1']['S090TKE'].data +1.0)

#  Create new variable (i.e. Difference variables)
Dvar['f1']['diffU'] = Dvar['f2']['UT1'].deepcopy()
Dvar['f1']['diffU'].setdata(Dvar['f2']['UT1'].data - Dvar['f3']['UT1'].data)

#  Plot
id_plt = 0  #  Initialisation (do not change)

Lplot = [ Dvar['f1']['SST'] ]
Ltitle = ['SST from INIT_SURF.lfi AROME']
id_plt = plot2D_epy(1,1, [16,16], Lplot, Ltitle,[], 280, 295, 'OrRd', 0.25,projection, id_plt,False,'contourf')

Lplot = [ Dvar['f3']['THT1'], Dvar['f3']['THT15'] ]
Ltitle = ['THT from NC']*2
id_plt = plot2D_epy(1,2, [16,16], Lplot, Ltitle,[], 260, 310, 'OrRd', 0.5,projection, id_plt,False,'contourf')

Lplot = [ Dvar['f2']['UT1'], Dvar['f3']['UT1'],Dvar['f4']['S090WIND.U.PHYS'], Dvar['f5']['u'] ]
Ltitle = ['U-wind from PREP_REAL lfi MNH', 'U-wind from PREP_REAL nc MNH','U-wind from FA AROME','U-wind from GRIB2 EPY1.4.8 AROME']
id_plt = plot2D_epy(2,2, [16,16], Lplot, Ltitle,[-5,10,42.5,52], -15, 15, 'seismic', 0.25,projection, id_plt,True,'contourf',"U-wind speed at 1st level")

Lplot = [ Dvar['f1']['diffU'] ]
Ltitle = ['Diff UT lfi - nc MNH']
id_plt = plot2D_epy(1,1, [16,16], Lplot, Ltitle,[], -1.0, 1.0, 'seismic', 0.05,projection, id_plt, True,'contourf')

#  Convert all png images to one PDF
#os.system('rm -f tempgraph*')
os.system('rm -f ' + output_name)
os.system('convert tempgraph* '+ output_name)
