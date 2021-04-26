#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb 25 11:25:31 2021

@author: rodierq
"""
import copy
from scipy.interpolate import RectBivariateSpline
import numpy as np
import math

def convert_date(datesince, time_in_sec):
  print(type(datesince))
  print(type(time_in_sec))
  return str(time_in_sec) + datesince[:33]

class mean_operator():
    def MYM(self,var):
        ny = var.shape[1]
        out = copy.deepcopy(var)
        for j in range(ny-1):
            out[:,j,:] = (var[:,j,:] + var[:,j+1,:])*0.5
        return out
    
    def MXM(self,var):
        nx = var.shape[2]
        out = copy.deepcopy(var)
        for i in range(nx-1):
            out[:,:,i] = (var[:,:,i] + var[:,:,i+1])*0.5
        return out
    
    def MZM(self,var):
        nz = var.shape[0]
        out = copy.deepcopy(var)
        for k in range(nz-1):
            out[k,:,:] = (var[k,:,:] + var[k+1,:,:])*0.5
        return out

def windvec_verti_proj(u, v, lvl, angle):
  """
     Compute the projected horizontal wind vector on an axis with a given angle w.r.t. the x/ni axes (West-East)
     Arguments :
            - u : U-wind component
            - v : V-wind component
            - angle (radian) of the new axe w.r.t the x/ni axes (West-East). angle = 0 for (z,x) sections, angle=pi/2 for (z,y) sections
     Returns :
            - a 3D wind component projected on the axe to be used with Panel_Plot.vector as Lvar1
  """ 
  out = copy.deepcopy(u)
  for k in range(len(lvl)):
      out[k,:,:] = u[k,:,:]*math.cos(angle) + v[k,:,:]*math.sin(angle)
  return out

def oblique_proj(var, ni, nj, lvl, i_beg, j_beg, i_end, j_end):
  """
     Compute an oblique projection of a 3D variable w.r.t. its axes
     Arguments :
            - var          : 3D var to project (example THT)
            - ni           : 1D x-axe of the 3D dimension
            - nj           : 1D y-axe of the 3D dimension
            - level        : 1D level variable (level or level_w)
            - i_beg, j_beg : coordinate of the begin point of the new axe
            - i_end, j_end : coordinate of the end point of the new axe
     Returns :
            - a 2D (z,m) variable projected on the oblique axe
            - a 1D m new axe (distance from the beggining point)
            - the angle (radian) of the new axe w.r.t the x/ni axes (West-East)
  """
  dist_seg=np.sqrt((i_end-i_beg)**2.0 + (j_end-j_beg)**2.0) #  Distance de la section oblique  m
  out_var = np.zeros((len(lvl),int(dist_seg)+1)) # Initialisation du nouveau champs projeté dans la coupe (z,m)
  axe_m = np.zeros(int(dist_seg)+1) #Axe des abscisses qui sera tracé selon la coupe
  axe_m_coord = [] #Coordonnées x,y des points qui composent l'axe
  axe_m_coord.append( (ni[i_beg],nj[j_beg]) ) #Le premier point est celui donné par l'utilisateur
  for m in range(int(dist_seg)): #Discrétisation selon distance de la coupe / int(distance_de_la_coupe)
    axe_m_coord.append( (axe_m_coord[0][0] + (ni[i_end]-ni[i_beg])/(int(dist_seg))*(m+1), 
                         axe_m_coord[0][1] + (nj[j_end]-nj[j_beg])/(int(dist_seg))*(m+1) ))
    axe_m[m+1] = np.sqrt((ni[i_beg]-axe_m_coord[m+1][0])**2 + (nj[j_beg]-axe_m_coord[m+1][1])**2)
  
  for k in range(len(lvl)):
    a=RectBivariateSpline(ni, nj,var[k,:,:],kx=1,ky=1) #Interpolation par niveau à l'ordre 1 pour éviter des valeurs négatives de champs strictement > 0
    for m in range(int(dist_seg)+1):
      out_var[k,m] = a.ev(axe_m_coord[m][0],axe_m_coord[m][1]) # La fonction ev de RectBivariate retourne la valeur la plus proche du point considéré
      
  angle_proj = math.acos((ni[i_end]-ni[i_beg])/axe_m[-1])
  return angle_proj, out_var, axe_m

def comp_altitude1DVar(oneVar2D, orography, ztop, level, n_xory):
  """
     Compute and returns an altitude and x or y grid mesh variable in 2D following the topography in 1D
     To be used with 2D simulations
     Arguments :
            - oneVar2D  : a random netCDF 2D var (example UT, THT)
            - orography : 1D orography (ZS)
            - ztop      : scalar of the top height of the model (ZTOP)
            - level     : 1D level variable (level or level_w)
            - n_xory:   : 1D directionnal grid variable (ni_u, nj_u, ni_v or nj_v)
     Returns :
            - a 2D altitude variable with topography taken into account
            - a 2D directionnal variable
  """
  n_xory_2D = copy.deepcopy(oneVar2D)
  altitude  =  copy.deepcopy(oneVar2D)
  for i in range(len(level)):
    n_xory_2D[i,:] = n_xory
  for j in range(len(n_xory)):
    for k in range(len(level)):
        altitude[k,j] = orography[j] + level[k]*((ztop-orography[j])/ztop)
  return altitude, n_xory_2D

def comp_altitude2DVar(oneVar3D, orography, ztop, level, n_y, n_x):
  """
     Compute and returns an altitude and x or y grid mesh variable in 3D following the topography in 2D
     To be used with 3D simulations in cartesian coordinates
     Arguments :
            - oneVar3D  : a random netCDF 3D var (example UT, THT)
            - orography : 2D orography (ZS)
            - ztop      : scalar of the top height of the model (ZTOP)
            - level     : 1D level variable (level or level_w)
            - n_xory:   : 1D directionnal grid variable (ni_u, nj_u, ni_v or nj_v)
     Returns :
            - a 3D altitude variable with topography taken into account
            - a 3D directionnal variable
  """
  n_x3D = copy.deepcopy(oneVar3D)
  n_y3D = copy.deepcopy(oneVar3D)
  altitude  =  copy.deepcopy(oneVar3D)
  for i in range(len(level)):
    n_y3D[i,:] = n_y
    n_x3D[i,:] = n_x
  for i in range(oneVar3D.shape[2]):
    for j in range(oneVar3D.shape[1]):
      for k in range(len(level)):
          altitude[k,j,i] = orography[j,i] + level[k]*((ztop-orography[j,i])/ztop)
  return altitude, n_x3D, n_y3D
