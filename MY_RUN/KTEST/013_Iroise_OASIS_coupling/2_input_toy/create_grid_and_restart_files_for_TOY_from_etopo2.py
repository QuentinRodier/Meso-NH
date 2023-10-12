#!/usr/bin/python3
# -*- coding: utf-8 -*-
#
# ---------------------------------------------------------
#
#                 Author  (    date    ) :
#             J. Pianezze ( 03.10.2023 )
#
#                    ~~~~~~~~~~~~~~~
#         Script used to create grids from etopo2.nc
#             and restart file for TOY model
#                    ~~~~~~~~~~~~~~~
#
# ---------------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
import os
import netCDF4
import numpy as np
import math
curdir_path = os.getcwd()+'/'
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# #########################################################
# ###           To be defined by user                   ###
# #########################################################

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - -           Add debug informations                  - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cfg_debug   = False

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - -          Extension of the TOY grid                - -
# - -             (from etopo2)                         - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
lon_domain = [-6.2, -4.0]
lat_domain = [47.0, 49.5]

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - -     Type of send fields in TOYNAMELIST.nam        - -
# - -   &NAM_FCT_SEND CTYPE_FCT = 'CNSTE' or 'SINUS'    - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# if CTYPE_FCT='CNSTE'
value_CNSTE        = 290.0

# if CTYPE_FCT='SINUS'
value_SINUS_COEF   = 0.011
value_SINUS_LENGTH = 1000.

# #########################################################

if cfg_debug : print('++++++++++++++++++++++++++++++++++++++++++++++')
if cfg_debug : print('+++                                           ')
if cfg_debug : print('+++   0. Read variables from etopo2.nc        ')
if cfg_debug : print('+++                                           ')
if cfg_debug : print('++++++++++++++++++++++++++++++++++++++++++++++')

# ---------------------------------------
#   Get full etopo2 data
# ---------------------------------------
file_topo   = netCDF4.Dataset('topo.nc')

lon_full    = file_topo.variables['lon'] [:]
lat_full    = file_topo.variables['lat'] [:]
topo_full   = file_topo.variables['topo'][:,:]

# ---------------------------------------
#   Reduce etopo2 data
# ---------------------------------------
ind_min_lon = np.where(lon_full[:]>lon_domain[0])[0][0]
ind_max_lon = np.where(lon_full[:]>lon_domain[1])[0][0]
ind_min_lat = np.where(lat_full[:]>lat_domain[0])[0][0]
ind_max_lat = np.where(lat_full[:]>lat_domain[1])[0][0]

lon         = lon_full [ind_min_lon:ind_max_lon]
lat         = lat_full [ind_min_lat:ind_max_lat]
topo        = topo_full[ind_min_lat:ind_max_lat,ind_min_lon:ind_max_lon]
lon, lat    = np.meshgrid(lon, lat)

# ---------------------------------------
#   Compute corners
# ---------------------------------------
nlon        = np.shape(lon)[1]
nlat        = np.shape(lon)[0]
ncorn       = 4

clo=np.zeros((ncorn,nlat,nlon))
cla=np.zeros((ncorn,nlat,nlon))

deltax=lon[0,1]-lon[0,0]
clo[0,:,:]=lon[:,:]+deltax/2.0
clo[1,:,:]=lon[:,:]-deltax/2.0
clo[2,:,:]=lon[:,:]-deltax/2.0
clo[3,:,:]=lon[:,:]+deltax/2.0

deltay=lat[1,0]-lat[0,0]
cla[0,:,:]=lat[:,:]+deltay/2.0
cla[1,:,:]=lat[:,:]+deltay/2.0
cla[2,:,:]=lat[:,:]-deltay/2.0
cla[3,:,:]=lat[:,:]-deltay/2.0

# ---------------------------------------
#   Compute surface
# ---------------------------------------
surface=np.zeros((nlat,nlon))
surface[:,:]=deltax*deltay

# ---------------------------------------
#   Compute mask and var send by toy
# ---------------------------------------
mask=np.zeros((nlat,nlon))
toyvarcnste=np.zeros((nlat,nlon))
toyvarsinus=np.zeros((nlat,nlon))

for ind_lon in range(nlon):
  for ind_lat in range(nlat):
    if topo[ind_lat,ind_lon] > 0.0 :
      mask[ind_lat,ind_lon]=0
      toyvarcnste[ind_lat,ind_lon] = value_CNSTE
      toyvarsinus[ind_lat,ind_lon] = value_SINUS_COEF*math.sin(lat[ind_lat,0]*math.pi/180.0*value_SINUS_LENGTH)
    else:
      mask[ind_lat,ind_lon]=1
      toyvarcnste[ind_lat,ind_lon]=value_CNSTE
      toyvarsinus[ind_lat,ind_lon]= value_SINUS_COEF*math.sin(lat[ind_lat,0]*math.pi/180.0*value_SINUS_LENGTH)

if cfg_debug : print('++++++++++++++++++++++++++++++++++++++++++++++')
if cfg_debug : print('+++                                           ')
if cfg_debug : print('+++   1. Create grid_toy_model.nc             ')
if cfg_debug : print('+++                                           ')
if cfg_debug : print('++++++++++++++++++++++++++++++++++++++++++++++')

grid_file=netCDF4.Dataset(curdir_path+'grid_toy_model.nc','w')
grid_file.Description='Grid file for TOY model'

# ----------------------------------
#   Create the dimensions of the files
# ----------------------------------
grid_file.createDimension ('nlon',   nlon)
grid_file.createDimension ('nlat',   nlat)
grid_file.createDimension ('ncorner',   4)

# ----------------------------------
#   Create the variables of the files
# ----------------------------------
varout=grid_file.createVariable('lon', 'd',(          'nlat','nlon'))
varout=grid_file.createVariable('lat', 'd',(          'nlat','nlon'))
varout=grid_file.createVariable('clo', 'd',('ncorner','nlat','nlon'))
varout=grid_file.createVariable('cla', 'd',('ncorner','nlat','nlon'))
varout=grid_file.createVariable('srf', 'd',(          'nlat','nlon'))
varout=grid_file.createVariable('mask','d',(          'nlat','nlon'))

# ---------------------------------------
#   Write out the data arrays into the file
# ---------------------------------------
grid_file.variables['lon'] [  :,:] = lon    [  :,:]
grid_file.variables['lat'] [  :,:] = lat    [  :,:]
grid_file.variables['clo'] [:,:,:] = clo    [:,:,:]
grid_file.variables['cla'] [:,:,:] = cla    [:,:,:]
grid_file.variables['srf'] [  :,:] = surface[  :,:]
grid_file.variables['mask'][  :,:] = mask   [  :,:]

# ---------------------------------------
#   Close the file
# ---------------------------------------
grid_file.close()

if cfg_debug : print('++++++++++++++++++++++++++++++++++++++++++++++')
if cfg_debug : print('+++                                           ')
if cfg_debug : print('+++   2. Create rstrt_TOY.nc                  ')
if cfg_debug : print('+++                                           ')
if cfg_debug : print('++++++++++++++++++++++++++++++++++++++++++++++')

rstrt_file=netCDF4.Dataset(curdir_path+'rstrt_TOY.nc','w')
rstrt_file.Description='OASIS restart file for TOY model'

# ----------------------------------
#   Create the dimensions of the files
# ----------------------------------
rstrt_file.createDimension ('nlon', nlon)
rstrt_file.createDimension ('nlat', nlat)

# ----------------------------------
#   Create the variables of the files
# ----------------------------------
varout=rstrt_file.createVariable('VARCNSTE','d',('nlat','nlon'))
varout=rstrt_file.createVariable('VARSIN01','d',('nlat','nlon'))
varout=rstrt_file.createVariable('VARSIN02','d',('nlat','nlon'))

# ---------------------------------------
#   Write out the data arrays into the file
# ---------------------------------------
rstrt_file.variables['VARCNSTE'][:,:] = toyvarcnste[:,:]
rstrt_file.variables['VARSIN01'][:,:] = toyvarsinus[:,:]
rstrt_file.variables['VARSIN02'][:,:] = toyvarsinus[:,:]

# ---------------------------------------
#   Close the file
# ---------------------------------------
rstrt_file.close()
