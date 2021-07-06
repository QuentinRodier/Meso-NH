#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
MNH_LIC Copyright 1994-2021 CNRS, Meteo-France and Universite Paul Sabatier
MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
MNH_LIC for details. version 1.

@author: 07/2021 Quentin Rodier
"""
import netCDF4 as nc
import numpy as np

def read_netcdf(LnameFiles, Dvar_input, path='.', get_data_only=True, del_empty_dim=True, removeHALO=True):
    """Read a netCDF4 Meso-NH file
    For each file, call functions to read diachronic or synchronous file
    
    Parameters
    ----------
    LnameFiles : list of str
        list of Meso-NH netCDF4 file (diachronic or synchronous)
    
    Dvar_input : Dict{'fileNumber' : 'var_name',('group_name','var_name')}
        where
        'fileNumber' is a str corresponding to 'f' + the file number in LnameFiles (by order)
        'var_name' is the exact str of the netCDF4 variable name
        ('group_name','var_name') is the exact tuple of the (sub-)groups name and the netCDF4 variable name
        e.g. : {'f1':['ZS', 'WT','ni', 'level'],
                'f2':[('/LES_budgets/Cartesian/Not_time_averaged/Not_normalized/cart/',MEAN_TH'),('/Budgets/RI','AVEF')]
                }
    
    path : str
        unique path of the files
    
    get_data_only : bool, default: True
        if True,  the function returns Dvar as masked_array (only data)
        if False, the function returns Dvar as netCDF4._netCDF4.Variable
    
    del_empty_dim : bool, default: True
        if get_data_only=True and del_empty_dim=True, returns Dvar as an array without dimensions with size 1 and 0
        e.g. : an array of dimensions (time_budget, cart_level, cart_nj, cart_ni) with shape (180,1,50,1) is returned (180,50)
        
    removeHALO : bool, default: True
        if True, remove first and last (NHALO=1) point [1:-1] if get_data_only=True on each 
        level, level_w, ni, ni_u, ni_v, nj, nj_u, nj_v dimensions
           
    Returns
    -------
    Dvar : Dict 
        Dvar[ifile]['var_name']                if the group contains only one variable
        Dvar[ifile][('group_name','var_name')] if the group contains more than one variable
    """
    Dvar = {}
    for i,nameFiles in enumerate(LnameFiles):
        f_nb = 'f' + str(i+1)
        print('Reading file ' + f_nb)
        print(path + nameFiles)
        theFile = nc.Dataset(path + nameFiles,'r')
        Dvar[f_nb] = {}
        if '000' in nameFiles[-6:-3]: 
            if theFile['MASDEV'][0] <= 54:
                raise TypeError('The python lib is available for MNH >= 5.5')
            else:
                Dvar[f_nb] = read_TIMESfiles_55(theFile, Dvar_input[f_nb], Dvar[f_nb], get_data_only, del_empty_dim, removeHALO)
        else:
            Dvar[f_nb]= read_BACKUPfile(theFile, Dvar_input[f_nb], Dvar[f_nb], get_data_only, del_empty_dim, removeHALO)
        theFile.close()
    return Dvar

def read_var(theFile, Dvar, var_name, get_data_only=True, del_empty_dim=True, removeHALO=True):
    """Read a netCDF4 variable
    
    Parameters
    ----------
    theFile : netCDF4._netCDF4.Dataset
        a Meso-NH diachronic netCDF4 file
        
    var_name : str
        a Meso-NH netCDF4 variable name
        
    get_data_only : bool, default: True
        if True,  the function returns Dvar as masked_array (only data)
        if False, the function returns Dvar as netCDF4._netCDF4.Variable
    
    del_empty_dim : bool, default: True
        if get_data_only=True and del_empty_dim=True, returns Dvar as an array without dimensions with size 1 and 0
        e.g. : an array of dimensions (time_budget, cart_level, cart_nj, cart_ni) with shape (180,1,50,1) is returned (180,50)
        
    removeHALO : bool, default: True
        if True, remove first and last (NHALO=1) point [1:-1] if get_data_only=True on each 
        level, level_w, ni, ni_u, ni_v, nj, nj_u, nj_v dimensions
           
    Returns
    -------
    Dvar : Dict 
        Dvar['var_name']                if the group contains only one variable
        Dvar[('group_name','var_name')] if the group contains more than one variable
    """
    try:
        var_dim = theFile.variables[var_name].ndim
        var_dim_name = theFile.variables[var_name].dimensions
    except:
        raise KeyError("Group and variable name not found in the file, check the group/variable name with ncdump -h YourMNHFile.000.nc. You asked for variable : " + var_name)
    
    if not get_data_only:
        Dvar[var_name] = theFile.variables[var_name]
    else: 
        if var_dim == 0:
            Dvar[var_name] = theFile.variables[var_name][0].data
        elif var_dim == 1:
            Dvar[var_name] = theFile.variables[var_name][:]
        elif var_dim == 2:
            Dvar[var_name] = theFile.variables[var_name][:,:]
        elif var_dim == 3:
            Dvar[var_name] = theFile.variables[var_name][:,:,:]
        elif var_dim == 4:
            Dvar[var_name] = theFile.variables[var_name][:,:,:,:]
        elif var_dim == 5:
            Dvar[var_name] = theFile.variables[var_name][:,:,:,:,:]
        elif var_dim == 6:
            Dvar[var_name] = theFile.variables[var_name][:,:,:,:,:,:]
        elif var_dim == 7:
            Dvar[var_name] = theFile.variables[var_name][:,:,:,:,:,:,:]
        if removeHALO:
            for i in range(8):
                try:
                    if var_dim_name[i]=='level' or var_dim_name[i]=='level_w' or \
                    var_dim_name[i]=='ni' or var_dim_name[i]=='ni_u' or var_dim_name[i]=='ni_v' or \
                    var_dim_name[i]=='nj' or var_dim_name[i]=='nj_u' or var_dim_name[i]=='nj_v':
                        if var_dim != 0:
                            Dvar[var_name] = removetheHALO(i+1, Dvar[var_name])
                except:
                    break
        if del_empty_dim:
            Ldimtosqueeze=[]
            var_shape = theFile.variables[var_name].shape
            for i in range(8):
                try:
                    if var_shape[i]==1: Ldimtosqueeze.append(i)
                except IndexError:
                    break
            Ldimtosqueeze=tuple(Ldimtosqueeze)
            Dvar[var_name] = np.squeeze(Dvar[var_name], axis=Ldimtosqueeze) 
        
    return Dvar

def read_from_group(theFile, Dvar, group_name, var_name, get_data_only=True, del_empty_dim=True,removeHALO=True):
    """Read a variable from a netCDF4 group 
    
    Parameters
    ----------
    theFile : netCDF4._netCDF4.Dataset
        a Meso-NH diachronic netCDF4 file
    
    group_name : str
        a Meso-NH netCDF4 group name
        
    var_name : str
        a Meso-NH netCDF4 variable name
        
    get_data_only : bool, default: True
        if True,  the function returns Dvar as masked_array (only data)
        if False, the function returns Dvar as netCDF4._netCDF4.Variable
    
    del_empty_dim : bool, default: True
        if get_data_only=True and del_empty_dim=True, returns Dvar as an array without dimensions with size 1 and 0
        e.g. : an array of dimensions (time_budget, cart_level, cart_nj, cart_ni) with shape (180,1,50,1) is returned (180,50)
        
    removeHALO : bool, default: True
        if True, remove first and last (NHALO=1) point [1:-1] if get_data_only=True on each 
        level, level_w, ni, ni_u, ni_v, nj, nj_u, nj_v dimensions
        
    Returns
    -------
    Dvar : Dict 
        Dvar['var_name']                if the group contains only one variable
        Dvar[('group_name','var_name')] if the group contains more than one variable
    """
    try:
        var_dim = theFile[group_name].variables[var_name].ndim
        var_dim_name = theFile[group_name].variables[var_name].dimensions
    except:
        raise KeyError("Group and variable name not found in the file, check the group/variable name with ncdump -h YourMNHFile.000.nc. You asked for group/variable : " + group_name + var_name)
    
    if not get_data_only:
        Dvar[(group_name,var_name)] = theFile[group_name].variables[var_name]
    else:
        if var_dim == 0:
            Dvar[(group_name,var_name)] = theFile[group_name].variables[var_name][0].data
        if var_dim == 1:
            Dvar[(group_name,var_name)] = theFile[group_name].variables[var_name][:]
        elif var_dim == 2:
            Dvar[(group_name,var_name)] = theFile[group_name].variables[var_name][:,:]
        elif var_dim == 3:
            Dvar[(group_name,var_name)] = theFile[group_name].variables[var_name][:,:,:]
        elif var_dim == 4:
            Dvar[(group_name,var_name)] = theFile[group_name].variables[var_name][:,:,:,:]
        elif var_dim == 5:
            Dvar[(group_name,var_name)] = theFile[group_name].variables[var_name][:,:,:,:,:]
        elif var_dim == 6:
            Dvar[(group_name,var_name)] = theFile[group_name].variables[var_name][:,:,:,:,:,:]
        elif var_dim == 7:
            Dvar[(group_name,var_name)] = theFile[group_name].variables[var_name][:,:,:,:,:,:,:]
        if removeHALO:
            for i in range(8):
                try:
                    if var_dim_name[i]=='level' or var_dim_name[i]=='level_w' or \
                    var_dim_name[i]=='ni' or var_dim_name[i]=='ni_u' or var_dim_name[i]=='ni_v' or \
                    var_dim_name[i]=='nj' or var_dim_name[i]=='nj_u' or var_dim_name[i]=='nj_v':
                        if var_dim != 0:
                            Dvar[var_name] = removetheHALO(i+1, Dvar[var_name])
                except:
                    break
        if del_empty_dim:
            Ldimtosqueeze=[]
            var_shape = Dvar[(group_name,var_name)].shape
            for i in range(8):
                try:
                    if var_shape[i]==1: Ldimtosqueeze.append(i)
                except IndexError:
                    break
            Ldimtosqueeze=tuple(Ldimtosqueeze)                
            Dvar[(group_name,var_name)] = np.squeeze(Dvar[(group_name,var_name)], axis=Ldimtosqueeze)
            
        # LES budget needs to be transposed to use psection functions without specifying .T each time
        if 'LES_budgets' in group_name: 
            Dvar[(group_name,var_name)] = Dvar[(group_name,var_name)].T   
    return Dvar

def read_BACKUPfile(theFile, Dvar_input, Dvar, get_data_only=True, del_empty_dim=True, removeHALO=True):
    """Read variables from Meso-NH MASDEV >= 5.5.0 synchronous file
    For all variables in Dvar_input of one file, call functions to read the variable of the group+variable
    
    Parameters
    ----------
    theFile : netCDF4._netCDF4.Dataset
        a Meso-NH diachronic netCDF4 file
        
    Dvar_input : Dict{'var_name',('group_name','var_name')}
        with
        'var_name' is the exact str of the netCDF4 variable name
        ('group_name','var_name') is the exact tuple of the (sub-)groups name and the netCDF4 variable name
        e.g. : {'f1':['ZS', 'WT','ni', 'level'],
                'f2':[('/LES_budgets/Cartesian/Not_time_averaged/Not_normalized/cart/',MEAN_TH'),('/Budgets/RI','AVEF')]
                }
    
    get_data_only: bool, default: True
        if True,  the function returns Dvar as masked_array (only data)
        if False, the function returns Dvar as netCDF4._netCDF4.Variable
    
    del_empty_dim: bool, default: True
        if get_data_only=True and del_empty_dim=True, returns Dvar as masked_array without dimensions with size 1 and 0
        e.g. : an array of dimensions (time_budget, cart_level, cart_nj, cart_ni) with shape (180,1,50,1) is returned (180,50)
    
    Returns
    -------
    Dvar : Dict 
    Dvar['var_name']                if the group contains only one variable
    Dvar[('group_name','var_name')] if the group contains more than one variable
    """     
    #  Reading date since beginning of the model run
    Dvar['time'] = theFile.variables['time'][0]
    Dvar['date'] = nc.num2date(Dvar['time'],units=theFile.variables['time'].units, calendar = theFile.variables['time'].calendar)
           
    for var in Dvar_input:
        if type(var) == tuple:
            Dvar = read_from_group(theFile, Dvar, var[0], var[1], get_data_only, del_empty_dim, removeHALO)
        else:
            Dvar = read_var(theFile, Dvar, var, get_data_only, del_empty_dim, removeHALO)
            
            #  For all variables except scalars, change Fill_Value to NaN
            Dvar[var]= np.where(Dvar[var] != -99999.0, Dvar[var], np.nan)
            Dvar[var]= np.where(Dvar[var] != 999.0, Dvar[var], np.nan)
    return Dvar

def read_TIMESfiles_55(theFile, Dvar_input, Dvar, get_data_only=True, del_empty_dim=True, removeHALO=True):
    """Read variables from Meso-NH MASDEV >= 5.5.0 diachronic file
    For all variables in Dvar_input of one file, call functions to read the variable of the group+variable

    Parameters
    ----------
    theFile : netCDF4._netCDF4.Dataset
        a Meso-NH diachronic netCDF4 file
        
    Dvar_input : Dict{'var_name',('group_name','var_name')}
        with
        'var_name' is the exact str of the netCDF4 variable name
        ('group_name','var_name') is the exact tuple of the (sub-)groups name and the netCDF4 variable name
        e.g. : {'f1':['ZS', 'WT','ni', 'level'],
                'f2':[('/LES_budgets/Cartesian/Not_time_averaged/Not_normalized/cart/',MEAN_TH'),('/Budgets/RI','AVEF')]
                }
    
    get_data_only: bool, default: True
        if True,  the function returns Dvar as masked_array (only data)
        if False, the function returns Dvar as netCDF4._netCDF4.Variable
    
    del_empty_dim: bool, default: True
        if get_data_only=True and del_empty_dim=True, returns Dvar as masked_array without dimensions with size 1 and 0
        e.g. : an array of dimensions (time_budget, cart_level, cart_nj, cart_ni) with shape (180,1,50,1) is returned (180,50)
    
    Returns
    -------
    Dvar : Dict 
    Dvar[ifile]['var_name']                if the group contains only one variable
    Dvar[ifile][('group_name','var_name')] if the group contains more than one variable
    """  
    for var in Dvar_input:
        print(var)
        if type(var) == tuple:
            Dvar = read_from_group(theFile, Dvar, var[0], var[1], get_data_only, del_empty_dim, removeHALO)
        else:
            Dvar = read_var(theFile, Dvar, var, get_data_only, del_empty_dim, removeHALO)
    return Dvar

def removetheHALO(idim, var):
    """Remove a NHALO=1 point [1:-1] at a given dimension idim of a variable var
    
    Parameters
    ----------
    idim: int
        the dimension over which remove the first and last point
    
    var: array
        a Meso-NH netCDF4 variable name
    
    Returns
    -------
    var : array 
    """  
    if idim == 1:
        var = var[1:-1]
    elif idim == 2:
        var = var[:,1:-1]
    elif idim == 3:
        var = var[:,:,1:-1]
    elif idim == 4:
        var = var[:,:,:,1:-1]
    elif idim == 5:
        var = var[:,:,:,:,1:-1]
    elif idim == 6:
        var = var[:,:,:,:,:,1:-1]
    elif idim == 7:
        var = var[:,:,:,:,:,:,1:-1]
    return var