#!/usr/bin/python3
# -*- coding: utf-8 -*-
#
# --------------------------------------------------------
#
#                 Author  (    date    ) :
#             J. Pianezze ( 29.09.2023 )
#
#                    ~~~~~~~~~~~~~~~
#         Script used to create restart file from
#                    PREP_IDEAL_CASE
#                    ~~~~~~~~~~~~~~~
#
# --------------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
import os
import netCDF4
import numpy as np
curdir_path = os.getcwd()+'/'
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cfg_debug   = False

if cfg_debug : print('++++++++++++++++++++++++++++++++++++++++++++++')
if cfg_debug : print('+++                                           ')
if cfg_debug : print('+++   0. Read variables from PREP_IDEAL_CASE  ')
if cfg_debug : print('+++                                           ')
if cfg_debug : print('++++++++++++++++++++++++++++++++++++++++++++++')

file_RSTRT = netCDF4.Dataset(curdir_path+'IROISE_5KM_201109_02_00.nc')

LON_MNH    = file_RSTRT.variables['LON']  [    1:-1,1:-1] ; nlon_MNH=len(LON_MNH[0,:])
LAT_MNH    = file_RSTRT.variables['LAT']  [    1:-1,1:-1] ; nlat_MNH=len(LAT_MNH[:,0])

U10_MNH    = file_RSTRT.variables['UT']   [0,2,1:-1,1:-1]
V10_MNH    = file_RSTRT.variables['VT']   [0,2,1:-1,1:-1]
PRES_MNH   = file_RSTRT.variables['PABST'][0,2,1:-1,1:-1]

try:
  EVAP_MNH = file_RSTRT.variables['EVAP3D'][0,1:-1,1:-1]
except KeyError:
  print('EVAP3D not found... imposed at 0!')
  EVAP_MNH = np.zeros((nlat_MNH, nlon_MNH))

try:
  RAIN_MNH = file_RSTRT.variables['INPRR3D'][0,1:-1,1:-1]
except KeyError:
  print('INPRR3D not found... imposed at 0!')
  RAIN_MNH = np.zeros((nlat_MNH, nlon_MNH))

try:
  FMU_MNH = file_RSTRT.variables['FMU'][0,1:-1,1:-1]
  FMV_MNH = file_RSTRT.variables['FMV'][0,1:-1,1:-1]
  H_MNH   = file_RSTRT.variables['H']  [0,1:-1,1:-1]
  RN_MNH  = file_RSTRT.variables['RN'] [0,1:-1,1:-1]
except KeyError:
  print('Turbulent fluxes FMU, FMV, H and LE not found... imposed at 0!')
  FMU_MNH = np.zeros((nlat_MNH, nlon_MNH))
  FMV_MNH = np.zeros((nlat_MNH, nlon_MNH))
  H_MNH   = np.zeros((nlat_MNH, nlon_MNH))
  RN_MNH  = np.zeros((nlat_MNH, nlon_MNH))

if cfg_debug : print('++++++++++++++++++++++++++++++++++++++++++++++')
if cfg_debug : print('+++                                           ')
if cfg_debug : print('+++   1. Create netcdf file                   ')
if cfg_debug : print('+++                                           ')
if cfg_debug : print('++++++++++++++++++++++++++++++++++++++++++++++')

# ---------------------------------------
#   Open the file
# ---------------------------------------
fout=netCDF4.Dataset(curdir_path+'rstrt_MNH.nc','w',format='NETCDF3_64BIT')
fout.Description='Restart file for MNH coupling'

# ----------------------------------
#   Create the dimensions
# ----------------------------------
fout.createDimension ('nlon', nlon_MNH)
fout.createDimension ('nlat', nlat_MNH)

# ----------------------------------
#   Create the variables
# ----------------------------------
varout=fout.createVariable('MNH_TAUX','d',('nlat','nlon'),fill_value=999.)
varout=fout.createVariable('MNH_TAUY','d',('nlat','nlon'),fill_value=999.)
varout=fout.createVariable('MNH_HEAT','d',('nlat','nlon'),fill_value=999.)
varout=fout.createVariable('MNH_SNET','d',('nlat','nlon'),fill_value=999.)
varout=fout.createVariable('MNH_EVAP','d',('nlat','nlon'),fill_value=999.)
varout=fout.createVariable('MNH_RAIN','d',('nlat','nlon'),fill_value=999.)
varout=fout.createVariable('MNH_WATF','d',('nlat','nlon'),fill_value=999.)
varout=fout.createVariable('MNH_PRES','d',('nlat','nlon'),fill_value=999.)
varout=fout.createVariable('MNH__U10','d',('nlat','nlon'),fill_value=999.)
varout=fout.createVariable('MNH__V10','d',('nlat','nlon'),fill_value=999.)

# ----------------------------------
#   Write data arrays 
# ----------------------------------
fout.variables['MNH_TAUX'][:,:] = FMU_MNH[:,:]
fout.variables['MNH_TAUY'][:,:] = FMV_MNH[:,:]
fout.variables['MNH_HEAT'][:,:] = H_MNH[:,:]
fout.variables['MNH_SNET'][:,:] = RN_MNH[:,:]
fout.variables['MNH_EVAP'][:,:] = EVAP_MNH[:,:]
fout.variables['MNH_RAIN'][:,:] = RAIN_MNH[:,:]
fout.variables['MNH_WATF'][:,:] = RAIN_MNH[:,:]-EVAP_MNH[:,:]
fout.variables['MNH_PRES'][:,:] = RAIN_MNH[:,:]
fout.variables['MNH__U10'][:,:] = U10_MNH[:,:]
fout.variables['MNH__V10'][:,:] = V10_MNH[:,:]

# ---------------------------------------
#   Close the file
# ---------------------------------------
fout.close()
