#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 22 10:29:13 2021

@author: rodierq
"""

import netCDF4 as nc
import numpy as np

def read_netcdf(LnameFiles, Dvar_input, path='.', removeHALO=True):
  Dvar = {}
  for i,nameFiles in enumerate(LnameFiles):
    f_nb = 'f' + str(i+1)
    print('Reading file ' + f_nb)
    print(path + nameFiles)
    if '000' in nameFiles[-6:-3]: #time series file (diachronic)
        theFile = nc.Dataset(path + nameFiles,'r')
        if theFile['MASDEV'][0] <= 54:
            read_TIMESfiles_54(theFile, f_nb, Dvar_input, Dvar)
        else : # 55 groups variables
            read_TIMESfiles_55(theFile, f_nb, Dvar_input, Dvar, removeHALO)
        theFile.close()
    else:
        read_BACKUPfile(nameFiles, f_nb, Dvar_input, Dvar, path=path, removeHALO=removeHALO)
  return Dvar #Return the dic of [files][variables]

def read_BACKUPfile(nameF, ifile, Dvar_input, Dvar_output, path='.', removeHALO=True):
  theFile = nc.Dataset(path + nameF,'r')
  Dvar_output[ifile] = {} #initialize dic for each files 
       
  #  Reading date since beginning of the model run
  Dvar_output[ifile]['date'] =  theFile.variables['time'].units
  Dvar_output[ifile]['time'] =  theFile.variables['time'][0]

  for var in Dvar_input[ifile]: #For each files
    #  Read variables
    n_dim = theFile.variables[var].ndim
    name_dim = theFile.variables[var].dimensions
 #   print(var + "  n_dim  = " + str(n_dim))
    
    if (n_dim ==0) or (n_dim == 1 and 'time' in name_dim):  #Scalaires or Variable time
      Dvar_output[ifile][var] = theFile.variables[var][0].data
    else:      
        if removeHALO:
          if n_dim == 1:
            Dvar_output[ifile][var] = theFile.variables[var][1:-1] #Variables 1D
          elif n_dim == 2:
            if theFile.variables[var].shape[0] == 1 and 'size' in name_dim[1]: #Variables 2D with the second dimension not a coordinate (--> list of strings : chemicals)
                Dvar_output[ifile][var] = theFile.variables[var][0,:] #Variables 2D
            elif theFile.variables[var].shape[0] == 1: #Variables 2D with first dim = 0
                Dvar_output[ifile][var] = theFile.variables[var][0,1:-1] #Variables 2D
            else:
              Dvar_output[ifile][var] = theFile.variables[var][1:-1,1:-1] #Variables 2D
          elif n_dim == 5: #Variables time, sizeXX, level, nj, ni (ex: chemical budgets in 3D)
              Dvar_output[ifile][var] = theFile.variables[var][0, :, 1:-1,:1:-1,1:-1]
          elif n_dim == 4 and 'time' in name_dim and ('level' in name_dim or 'level_w' in name_dim): # time,z,y,x
              if theFile.variables[var].shape[1] == 1: #Variables 4D time,z,y,x with time=0 z=0
                Dvar_output[ifile][var] = theFile.variables[var][0,0,1:-1,1:-1] #Variables 2D y/x
              elif theFile.variables[var].shape[2] == 1: #Variable 2D  (0,zz,0,xx)
                Dvar_output[ifile][var] = theFile.variables[var][0,1:-1,0,1:-1] #Variables 2D z/y
              elif theFile.variables[var].shape[3] == 1: #Variable 2D  (0,zz,yy,0)
                Dvar_output[ifile][var] = theFile.variables[var][0,1:-1,1:-1,0] #Variables 2D z/x
              ## ATTENTION VARIABLE 1D codé en 4D non faite
              else: #Variable 3D simple
                Dvar_output[ifile][var] = theFile.variables[var][0,1:-1,1:-1,1:-1] #Variables time + 3D     
          elif n_dim == 4 and 'time' in name_dim and 'level' not in name_dim and 'level_w' not in name_dim: # time,nb_something,y,x
               Dvar_output[ifile][var] = theFile.variables[var][0,:,1:-1,1:-1] #Variables 2D y/x
          elif n_dim == 3 and 'time' in name_dim: # time, y, x 
            Dvar_output[ifile][var] = theFile.variables[var][0,1:-1,1:-1]
          else :
            Dvar_output[ifile][var] = theFile.variables[var][1:-1,1:-1,1:-1]  #Variables 3D
        else:
          if n_dim == 1:
            Dvar_output[ifile][var] = theFile.variables[var][:] #Variables 1D  
          elif n_dim == 2:
            if theFile.variables[var].shape[0] == 1 and 'size' in name_dim[1]: #Variables 2D with the second dimension not a coordinate (--> list of strings : chemicals)
              Dvar_output[ifile][var] = theFile.variables[var][0,:] #Variables 2D
            elif theFile.variables[var].shape[0] == 1: #Variables 2D with first dim = 0
              Dvar_output[ifile][var] = theFile.variables[var][0,:] #Variables 2D
            else:
              Dvar_output[ifile][var] = theFile.variables[var][:,:] #Variables 2D
          elif n_dim == 5: #Variables time, sizeXX, level, nj, ni (ex: chemical budgets in 3D)
              Dvar_output[ifile][var] = theFile.variables[var][0,:,:,:,:]
          elif n_dim == 4: # time,z,y,x
            if theFile.variables[var].shape[1] == 1: #Variables 4D time,z,y,x with time=0 z=0
              Dvar_output[ifile][var] = theFile.variables[var][0,0,:,:] #Variables 2D y/x
            elif theFile.variables[var].shape[2] == 1: #Variable 2D  (0,zz,0,xx)
              Dvar_output[ifile][var] = theFile.variables[var][0,:,0,:] #Variables 2D z/y
            elif theFile.variables[var].shape[3] == 1: #Variable 2D  (0,zz,yy,0)
              Dvar_output[ifile][var] = theFile.variables[var][0,:,:,0] #Variables 2D z/x
            ## ATTENTION VARIABLE 1D codé en 4D non faite
            else: #Variable 3D simple
              Dvar_output[ifile][var] = theFile.variables[var][0,:,:,:] #Variables time + 3D
          elif n_dim ==3 and name_dim in var.dimensions: # time, y, x
            Dvar_output[ifile][var] = theFile.variables[var][0,:,:]
          else:
            Dvar_output[ifile][var] = theFile.variables[var][:,:,:]  #Variables 3D
        #  For all variables except scalars, change Fill_Value to NaN
        Dvar_output[ifile][var]= np.where(Dvar_output[ifile][var] != -99999.0, Dvar_output[ifile][var], np.nan)
        Dvar_output[ifile][var]= np.where(Dvar_output[ifile][var] != 999.0, Dvar_output[ifile][var], np.nan)

  theFile.close()
  return Dvar_output #Return the dic of [files][variables]

def read_TIMESfiles_54(theFile, ifile, Dvar_input, Dvar_output):
    Dvar_output[ifile] = {} #initialize dic for each files 

    #  Level variable is automatically read without the Halo
    Dvar_output[ifile]['level'] = theFile.variables['level'][1:-1]
    
    #  Time variable is automatically read (time since begging of the run) from the 1st variable of the asked variable to read
    suffix, name_first_var = remove_PROC(Dvar_input[ifile][0])
    try: #  It is possible that there is only one value (one time) in the .000 file, as such time series are not suitable and the following line can't be executed. The time variable is then not written
        increment = theFile.variables[name_first_var+'___DATIM'][1,-1] - theFile.variables[name_first_var+'___DATIM'][0,-1] #-1 is the last entry of the date (current UTC time in seconds)
        length_time = theFile.variables[name_first_var+'___DATIM'].shape[0]
        Dvar_output[ifile]['time'] = np.arange(increment,increment*(length_time+1),increment)
    except:
        pass
    
    for var in Dvar_input[ifile]: #For each files
        suffix, var_name = remove_PROC(var)
        n_dim = theFile.variables[var].ndim
        name_dim = theFile.variables[var].dimensions

        #  First, test if the variable is a dimension/coordinate variable
        if (n_dim ==0):  #  Scalaires variable
             Dvar_output[ifile][var] = theFile.variables[var][0].data
             pass
        elif n_dim == 1:
            Dvar_output[ifile][var_name] = theFile.variables[var][1:-1]  #  By default, the Halo is always removed because is not in the other variables in any .000 variable
            pass
        elif n_dim == 2:
            Lsize1 = list_size1(n_dim, name_dim)
            if Lsize1 == [True, False]: Dvar_output[ifile][var_name] = theFile.variables[var][0,1:-1] 
            pass
        
        Lsize1 = list_size1(n_dim, name_dim)
        if Lsize1 == [True, False, False, True, True]: Dvar_output[ifile][var_name] = theFile.variables[var][0,:,:,0,0].T # Need to be transposed here
        if Lsize1 == [True, True, False, True, False]: Dvar_output[ifile][var_name] = theFile.variables[var][0,0,:,0,:]

    return Dvar_output #Return the dic of [files][variables]

def read_TIMESfiles_55(theFile, ifile, Dvar_input, Dvar_output, removeHALO=False):
    """
        Read variables from MNH MASDEV >= 5.5.0 
        Parameters :
            - Dvar_input : dictionnary of {file : var}. var can be either 
                - a string = the variable name 
                - or a tuple of ('group_name','var_name')
                If the variable desired is in a group_name and the group_name is not specified, it is assumed group_name = variable_name
            except for specific variable as (cart, neb, clear, cs1, cs2, cs3) type
        Return :
        Dvar_output : dictionnary of Dvar_output[ifile][variables or tuple (group,variables) if the user specified a tuple]
    """
    Dvar_output[ifile] = {} #initialize dic for each files 
    def read_var(theFile, Dvar_output, var):
        suffix, var_name = remove_PROC(var)
        try: #  NetCDF4 Variables
            n_dim = theFile.variables[var_name].ndim
            #  First, test if the variable is a dimension/coordinate variable
            if (n_dim ==0):  #  Scalaires variable
                Dvar_output[var_name] = theFile.variables[var_name][0].data
            else:
                if(removeHALO):
                    if n_dim == 1:
                        Dvar_output[var_name] = theFile.variables[var_name][1:-1]
                    elif n_dim == 2:
                        Dvar_output[var_name] = theFile.variables[var_name][1:-1,1:-1] 
                    else: 
                        raise NameError("Lecture des variables de dimension sup a 2 pas encore implementees pour fichiers .000")
                else:
                    if n_dim == 1:
                        Dvar_output[var_name] = theFile.variables[var_name][:]
                    elif n_dim == 2:
                        Dvar_output[var_name] = theFile.variables[var_name][:,:] 
                    else: 
                        raise NameError("Lecture des variables de dimension sup a 2 pas encore implementees pour fichiers .000")
        except KeyError: # NetCDF4 Group not specified by the user
            if '(cart)' in var_name or '(neb)' in var_name or '(clear)' in var_name or '(cs1)' in var_name or '(cs2)' in var_name or '(cs3)' in var_name:
            # If users specify the complete variable name with averaging type
              group_name = get_group_from_varname(var_name)
            else:
              group_name = var_name
            read_from_group(theFile, Dvar_output, var, var)
        return Dvar_output

    def read_from_group(theFile, Dvar_output, group_name, var):
        suffix, var_name = remove_PROC(var)
        if group_name == 'TSERIES': #always 1D
            Dvar_output[(group_name,var)] = theFile.groups['TSERIES'].variables[var][:]
        elif group_name == 'ZTSERIES': #always 2D 
            Dvar_output[(group_name,var)] = theFile.groups['ZTSERIES'].variables[var][:,:].T
        elif 'XTSERIES' in group_name: #always 2D
            Dvar_output[(group_name,var)] = theFile.groups[group_name].variables[var][:,:].T
        elif theFile.groups[group_name].type == 'TLES' : #  LES type
            try: #By default, most variables read are 2D cart and user does not specify it in the variable name
              whites = ' '*(17 - len('(cart)') - len(var_name))
              Dvar_output[var] = theFile.groups[var].variables[var + whites + '(cart)'][:,:].T
            except:
              try: #Variable 3D sv,time_les, level_les
                  Dvar_output[var] = theFile.groups[group_name].variables[var][:,:,:]
              except:
                try: #Variable 2D with type of variable specified (cart, neb, clear, cs1, cs2, cs3) 
                  Dvar_output[var] = theFile.groups[group_name].variables[var][:,:].T
                except ValueError: #  Variable 1D
                  Dvar_output[var] = theFile.groups[group_name].variables[var][:]
        elif theFile.groups[group_name].type == 'CART':  #  Budget CART type
            shapeVar = theFile.groups[group_name].variables[suffix].shape
            Ltosqueeze=[] #  Build a tuple with the number of the axis which are 0 dimensions to be removed by np.squeeze
            if shapeVar[0]==1: Ltosqueeze.append(0)
            if shapeVar[1]==1: Ltosqueeze.append(1)
            if shapeVar[2]==1: Ltosqueeze.append(2)
            if shapeVar[3]==1: Ltosqueeze.append(3)
            Ltosqueeze=tuple(Ltosqueeze)
            Dvar_output[group_name] = np.squeeze(theFile.groups[group_name].variables[suffix][:,:,:,:], axis=Ltosqueeze) 
        else:
            raise NameError("Type de groups variables not implemented in read_MNHfile.py")
        return Dvar_output
    for var in Dvar_input[ifile]: #For each var
        if type(var) == tuple:
            Dvar_output[ifile] = read_from_group(theFile, Dvar_output, var[0], var[1])
        else:
            Dvar_output[ifile] = read_var(theFile, Dvar_output, var)
    
    return Dvar_output #Return the dic of [files][variables]

def list_size1(n_dim, named_dim):
    Lsize1 = []
    for i in range(n_dim):
        if 'size1' == named_dim[i]:
            Lsize1.append(True)
        else:
            Lsize1.append(False)
    return Lsize1
    
def remove_PROC(var):
    if '___PROC' in var:
        var_name = var[:-8]
        suffix = "" # No need of suffix for MNHVERSION < 550 (suffix is for NetCDF4 group)
    elif  '___ENDF' in var or '___INIF' in var or '___AVEF' in var:
        var_name = var[:-7]
        suffix = var[-4:]
    else:
        var_name = var
        suffix = ''
    return suffix, var_name

def get_group_from_varname(var):
    group_name=''
    for i in range(len(var)):
      if var[i] != ' ':
        group_name+=var[i]
      else: # As soon as the caracter is a blank, the group variable is set
        break
    return group_name 
