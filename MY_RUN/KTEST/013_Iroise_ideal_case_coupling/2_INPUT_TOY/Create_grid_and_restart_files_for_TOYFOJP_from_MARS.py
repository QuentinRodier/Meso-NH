#!/usr/bin/python
# -*- coding: utf-8 -*-
#
###################################################
#=================================================#
#  Creating grid and restart file for toy model
#  Author : J. Pianezze
#  Date   :        2015
#  Modification  Avril 2022 :Françoise Orain et Joris Pianezze  pour python3 et utiliser H0(bathy de MARC)
#=================================================#
###################################################
#
import netCDF4
import numpy as np
import scipy
import matplotlib.pyplot as plt
import math
from   pylab import *
import os
#
curdir_path=os.path.abspath(os.curdir)+'/'
#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# To be defined by the user
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#
#-- Limit of the grid (from etopo)
lat_domain=[ 46.681,  50.092]
lon_domain=[-6.6348, -1.3652]
print ('londomaine[0]',lon_domain[0])
print ('londomaine[1]',lon_domain[1])

#-- Type of forcing to create the restart file 
#   for the toy : CNSTE or SINUS

# CNSTE
value_CNSTE=290.0

# SINUS
value_SINUS_COEF=0.011
value_SINUS_LENGTH=1000.
#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

file_topo = netCDF4.Dataset('bathy_MARC.nc')

#------ Read variables
lon_full  = file_topo.variables['longitude'][:,:]
lat_full  = file_topo.variables['latitude'][:,:]
topo_full = file_topo.variables['H0'][:,:]

ind_min_lon=np.where(lon_full[0,:]>lon_domain[0])[0][0] ; print ("ind_min_lon = ",ind_min_lon)
ind_max_lon=-1 # np.where(lon_full[0,:]>lon_domain[1])[0][0] ; print ("ind_max_lon = ",ind_max_lon)
ind_min_lat=np.where(lat_full[:,0]>lat_domain[0])[0][0] ; print ("ind_min_lat = ",ind_min_lat)
ind_max_lat=np.where(lat_full[:,0]>lat_domain[1])[0][0] ; print ("ind_max_lat = ",ind_max_lat)

print ('---- topo')
topo  = topo_full[ind_min_lat:ind_max_lat,ind_min_lon:ind_max_lon]

print ('---- longitude/latitude')
lon = lon_full[ind_min_lat:ind_max_lat,ind_min_lon:ind_max_lon]
lat = lat_full[ind_min_lat:ind_max_lat,ind_min_lon:ind_max_lon]

nlon=np.size(lon[0,:]) ;  print ('nlon =',  nlon)
nlat=np.size(lat[:,0]) ;  print ('nlat =',  nlat)
ncorn=4           ;  print ('ncorn=', ncorn)

print ('---- corners longitude/latitude')
clo=np.zeros((ncorn,nlat,nlon))
cla=np.zeros((ncorn,nlat,nlon))

deltax=lon[0,1]-lon[0,0] ; print ('deltax=', deltax)
clo[0,:,:]=lon[:,:]+deltax/2.0
clo[1,:,:]=lon[:,:]-deltax/2.0
clo[2,:,:]=lon[:,:]-deltax/2.0
clo[3,:,:]=lon[:,:]+deltax/2.0

deltay=lat[1]-lat[0] ; print ('deltay=', deltay)
cla[0,:,:]=lat[:,:]+deltay/2.0
cla[1,:,:]=lat[:,:]+deltay/2.0
cla[2,:,:]=lat[:,:]-deltay/2.0
cla[3,:,:]=lat[:,:]-deltay/2.0

print ('---- surface')
surface=np.zeros((nlat,nlon))
surface[:,:]=deltax*deltay


print ('---- mask and var send by toy')
mask=np.zeros((nlat,nlon))
toyvarcnste=np.zeros((nlat,nlon))
toyvarsinus=np.zeros((nlat,nlon))

for ind_lon in range(nlon):
  for ind_lat in range(nlat):
    if topo[ind_lat,ind_lon] > 0.0 :
      if ind_lon >= 132 and ind_lat >= 23 and ind_lat <= 25:
        mask     [ind_lat,ind_lon] = 1
      else:      
        mask[ind_lat,ind_lon]=0
      toyvarcnste[ind_lat,ind_lon] = value_CNSTE
      toyvarsinus[ind_lat,ind_lon] = value_SINUS_COEF*math.sin(lat[ind_lat,0]*math.pi/180.0*value_SINUS_LENGTH)
    else:
      mask     [ind_lat,ind_lon] = 1
      toyvarcnste[ind_lat,ind_lon] = value_CNSTE
      toyvarsinus[ind_lat,ind_lon] = value_SINUS_COEF*math.sin(lat[ind_lat,0]*math.pi/180.0*value_SINUS_LENGTH)

# -----------------------------------------------------------------------
#  Inverse du mask car la bathy est positive en mer et negative sur terre
# -----------------------------------------------------------------------
mask = 1 - mask

##################################################
print ('------------------------------------------')
print (' Creating netcdf file : grid_toy_modelMARC.nc')

grid_file=netCDF4.Dataset(curdir_path+'grid_toy_modelMARC.nc','w',format='NETCDF3_64BIT')
grid_file.Description='Grid file for OASIS coupling'

# ----------------------------------
# Create the dimensions of the files
# ----------------------------------
grid_file.createDimension ('nlon', nlon)
grid_file.createDimension ('nlat', nlat)
grid_file.createDimension ('ncorner', 4 )

# ----------------------------------
# Create the variables of the files
# ----------------------------------
varout=grid_file.createVariable('lon','d',('nlat','nlon'))
varout=grid_file.createVariable('lat','d',('nlat','nlon'))
varout=grid_file.createVariable('clo','d',('ncorner','nlat','nlon'))
varout=grid_file.createVariable('cla','d',('ncorner','nlat','nlon'))
varout=grid_file.createVariable('srf','d',('nlat','nlon'))
varout=grid_file.createVariable('imask','d',('nlat','nlon'))

# ---------------------------------------
# Write out the data arrays into the file
# ---------------------------------------
grid_file.variables['lon'][:,:] = lon[:,:]
grid_file.variables['lat'][:,:] = lat[:,:]
grid_file.variables['clo'][:,:] = clo[:,:,:]
grid_file.variables['cla'][:,:] = cla[:,:,:]
grid_file.variables['srf'][:,:] = surface[:,:]
grid_file.variables['imask'][:,:] = mask[:,:]

# ---------------------------------------
# close the file
# ---------------------------------------
grid_file.close()

print (' Closing netcdf file : grid_toy_modelMARC.nc')
print ('------------------------------------------')
##################################################

##################################################
print ('------------------------------------------')
print (' Creating netcdf file : rstrt_TOYMARC.nc')

rstrt_file=netCDF4.Dataset(curdir_path+'rstrt_TOYMARC.nc','w',format='NETCDF3_64BIT')
rstrt_file.Description='Restart file for OASIS coupling'

# ----------------------------------
# Create the dimensions of the files
# ----------------------------------
rstrt_file.createDimension ('nlon', nlon)
rstrt_file.createDimension ('nlat', nlat)

# ----------------------------------
# Create the variables of the files
# ----------------------------------
varout=rstrt_file.createVariable('VARCNSTE','d',('nlat','nlon'))
varout=rstrt_file.createVariable('VARSIN01','d',('nlat','nlon'))
varout=rstrt_file.createVariable('VARSIN02','d',('nlat','nlon'))

# ---------------------------------------
# Write out the data arrays into the file
# ---------------------------------------
rstrt_file.variables['VARCNSTE'][:,:] = toyvarcnste[:,:]
rstrt_file.variables['VARSIN01'][:,:] = toyvarsinus[:,:]
rstrt_file.variables['VARSIN02'][:,:] = toyvarsinus[:,:]

# ---------------------------------------
# close the file
# ---------------------------------------
rstrt_file.close()

print (' Closing netcdf file : rstrt_TOYMARC.nc  ')
print ('-----------------------------------------')
#####################################################
