#!/usr/bin/python3
# -*- coding: utf-8 -*-

import numpy as np
from netCDF4 import num2date
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cartopy.feature as cpf


#################
# Read MNH output
#################

def read_MNH_output_conf(file):
    nctime=file.variables['time'][0]
    t_unit=file.variables['time'].units
    valid_time=num2date(nctime,t_unit)
    lat=file.variables['ni'][:]
    lon=file.variables['nj'][:]
    tmp1=file.variables['THT']
    tmp2=tmp1[0,:,:,:]
    nx=tmp2.shape[2]
    ny=tmp2.shape[1]
    nz=tmp2.shape[0]
    return valid_time,lat,lon

def read_MNH_output_var(variables,file,fields,units):
    MesoNH_names={'ZS':'orography', 'ACPRT':'acc_precip', 'ACPRR':'acc_rain', 'ACPRS':'acc_snow', 'ACPRG':'acc_graupel', 'INPRT':'precip_rate', 'INPRR':'rain_rate', 'INPRS':'snow_rate', 'INPRG':'graupel_rate', 'PABST':'P', 'THT':'theta', 'RVT':'rv', 'RCT':'rc', 'RRT':'rr', 'RIT':'ri', 'RST':'rs', 'RGT':'rg', 'RHT':'rh', 'CCLOUDT':'Nc', 'CRAINT':'Nr', 'CICET':'Ni', 'UT':'u', 'VT':'v', 'WT':'w'}
    computable=['alt','T','T_K','RH_liq','RH_ice','LWC','IWC','Dc','Dr']
    for variable in variables:
        if not (variable in fields):
            if variable in ['ZS']:
                var=file.variables[variable]
                fields[MesoNH_names[variable]]=var[:,:]
                units[MesoNH_names[variable]]=var.units
            elif variable in ['ACPRT', 'ACPRR', 'ACPRS', 'ACPRG', 'INPRT', 'INPRR', 'INPRS', 'INPRG']:
                var=file.variables[variable]
                fields[MesoNH_names[variable]]=var[0,:,:]
                units[MesoNH_names[variable]]=var.units
            elif variable in MesoNH_names:
                try:
                    var=file.variables[variable]
                    fields[MesoNH_names[variable]]=var[0,:,:,:]
                    units[MesoNH_names[variable]]=var.units
                except:
                    var=file.variables['THT']
                    fields[MesoNH_names[variable]]=var[0,:,:,:]*0.
                    units[MesoNH_names[variable]]='none'
            elif variable == 'allMNH':
                read_MNH_output_var(MesoNH_names.keys(),file,fields,units)
            elif variable == 'hydrometeors':
                read_MNH_output_var(['RCT', 'RRT', 'RIT', 'RST', 'RGT', 'CCLOUDT', 'CRAINT', 'CICET'],file,fields,units)
            elif variable == 'wind':
                read_MNH_output_var(['UT','VT','WT'],file,fields,units)
            elif variable == 'precipitation':
                read_MNH_output_var(['ACPRT', 'ACPRR', 'ACPRS', 'ACPRG', 'INPRT', 'INPRR', 'INPRS', 'INPRG'],file,fields,units)
            elif variable == 'alt':
                tmp1=file.variables['RCT']
                tmp2=tmp1[0,:,:,:]
                nx=tmp2.shape[2]
                ny=tmp2.shape[1]
                nz=tmp2.shape[0]
                Z=file.variables["ZHAT"][:]
                ZHAT_M=np.zeros((nz),dtype='f')
                for k in range(0,nz-1):
                    ZHAT_M[k]=(Z[k]+Z[k+1])/2.
                ZHAT_M[nz-1]=2*Z[nz-1]-Z[nz-2] # a verifier
                alt=np.zeros((nz,ny,nx),dtype='f')
                zs=file.variables['ZS'][:,:] 
                ZCOEF=np.zeros((ny,nx),dtype='f')
                ZCOEF[:,:]=1-zs[:,:]/ZHAT_M[nz-1]
                for k in range(0,nz-1):
                    alt[k,:,:]=ZHAT_M[k]*ZCOEF[:,:]+zs[:,:]
                fields['alt']=alt[:,:,:]
                units['alt']='m'
                del Z, ZHAT_M, zs, ZCOEF,nx,ny,nz,tmp1,tmp2
            elif variable == 'T':
                read_MNH_output_var(['THT', 'PABST'],file,fields,units)
                fields['T']=fields['theta']*(fields['P']/100000.)**(2./7.)-273.15
                units['T']='C'
            elif variable == 'T_K':
                read_MNH_output_var(['THT', 'PABST'],file,fields,units)
                fields['T_K']=fields['theta']*(fields['P']/100000.)**(2./7.)
                units['T_K']='K'
            elif variable == 'RH_liq':
                read_MNH_output_var(['T_K', 'PABST', 'RVT'],file,fields,units)
                e_sat_w=np.exp(60.223 - 6822.4/fields['T_K'] - 5.1393*np.log(fields['T_K']))
                r_sat_w=0.62198 * e_sat_w / (fields['P'] - e_sat_w)
                rh_w=100 * fields['rv'] / r_sat_w
                fields['RH_liq']=rh_w[:,:,:]
                units['RH_liq']='%'
            elif variable == 'RH_ice':
                read_MNH_output_var(['T_K', 'PABST', 'RVT'],file,fields,units)
                e_sat_i=np.exp(32.621 - 6295.4/fields['T_K'] - 0.56313*np.log(fields['T_K']))
                r_sat_i=0.62198 * e_sat_i / (fields['P'] - e_sat_i)
                rh_i=100 * fields['rv'] / r_sat_i
                rh_i[np.where(fields['T']>0.)]=float('nan')
                fields['RH_ice']=rh_i[:,:,:]
                units['RH_ice']='%'
            elif variable == 'LWC':
                read_MNH_output_var(['RCT', 'RRT'],file,fields,units)
                fields['LWC']=fields['rc']+fields['rr']
                units['LWC']=units['rc']
            elif variable == 'IWC':
                read_MNH_output_var(['RIT', 'RST', 'RGT'],file,fields,units)
                fields['IWC']=fields['ri']+fields['rs']+fields['rg']
                units['IWC']=units['ri']
            elif variable == 'Dc':
                read_MNH_output_var(['RCT', 'CCLOUDT'],file,fields,units)
                fields['Dc']=1.e6*(6*fields['rc']/1000/3.14159/fields['Nc'])**0.33
                units['Dc']='µm'                
            elif variable == 'Dr':
                read_MNH_output_var(['RRT', 'CRAINT'],file,fields,units)
                fields['Dr']=1000.*(6*fields['rr']/1000/3.14159/fields['Nr'])**0.33
                units['Dr']='mm'
            elif variable == 'all':
                computable.append('allMNH')
                read_MNH_output_var(computable,file,fields,units)

def read_MNH_budgets_conf(file):
#    nctime=file.variables['time']
#    t_unit=file.variables['time'].units
#    valid_time=[num2date(t,t_unit) for t in nctime]
#    budget_length=nctime[0]
    tmp1=file.variables['RJS__0001___PROC1']
    tmp2=tmp1[0,0,:,:,:]
    nx=tmp2.shape[2]
    ny=tmp2.shape[1]
    nz=tmp2.shape[0]
    return nx,ny,nz

def read_MNH_budgets_var(var,step,procs,file,fields,units):
    proc_dict = {name:value for name, value in file.variables.items() if name.startswith(var+"{:04d}".format(step)+'___PROC')}
    proc_comment = {name:value.comment.split(' ',1)[0] for name, value in proc_dict.items()}
    proc_unit={}
    for name, value in proc_dict.items():
        try:
            proc_unit[name]=value.unit
        except AttributeError:
            proc_unit[name]=''
    for v in proc_dict.keys():
        nx = proc_dict[v][0,0,:,:,:].shape[2]
        ny = proc_dict[v][0,0,:,:,:].shape[1]
        dx = 1 if nx > 2 else 0
        dy = 1 if ny > 2 else 0
        fields[proc_comment[v]]=proc_dict[v][0,0,:,dy:ny-dy,dx:nx-dx]
#        fields[proc_comment[v]]=proc_dict[v][0,0,:,0:1,5:175]
        units[proc_comment[v]]=proc_unit[v]

#######################################################
# Interpolation on selected pressure or altitude levels
#######################################################

def pre_interp_pres(pres,plevel):
    interp_pres={}
    IIE=pres.shape[2]
    IJE=pres.shape[1]
    IKE=pres.shape[0]
    nlev=len(plevel)    
    PREF=np.log10(plevel*100)
    for i in plevel:
        interp_pres[i]={}
        interp_pres[i]['below']=np.zeros((IJE,IIE),dtype='i')
        interp_pres[i]['above']=np.zeros((IJE,IIE),dtype='i')
        interp_pres[i]['slope']=np.zeros((IJE,IIE),dtype='f')
    for i in range(0,IIE):
        for j in range(0,IJE):
            n=0
            for k in range (0,IKE):
                p=PREF[n]
                if p > np.log10(pres[k,j,i]): 
                    if k!=0:
                        zslope=(p-np.log10(pres[k-1,j,i]))/(np.log10(pres[k,j,i])-np.log10(pres[k-1,j,i]))
                        interp_pres[plevel[n]]['below'][j,i] = k-1
                        interp_pres[plevel[n]]['above'][j,i] = k
                        interp_pres[plevel[n]]['slope'][j,i] = zslope
                    n=n+1
                    if n==nlev:
                       break    
    return interp_pres


def pre_interp_alt(alt,alevel):
    interp_alt={}
    IIE=alt.shape[2]
    IJE=alt.shape[1]
    IKE=alt.shape[0]
    nlev=len(alevel)    
    for i in alevel:
        interp_alt[i]={}
        interp_alt[i]['below']=np.zeros((IJE,IIE),dtype='i')
        interp_alt[i]['above']=np.zeros((IJE,IIE),dtype='i')
        interp_alt[i]['slope']=np.zeros((IJE,IIE),dtype='f')
    for i in range(0,IIE):
        for j in range(0,IJE):
            n=0
            for k in range (0,IKE):
                a=alevel[n]
                if a < alt[k,j,i]: 
                    if k!=0:
                        zslope=(a-alt[k-1,j,i])/(alt[k,j,i]-alt[k-1,j,i])
                        interp_alt[alevel[n]]['below'][j,i] = k-1
                        interp_alt[alevel[n]]['above'][j,i] = k
                        interp_alt[alevel[n]]['slope'][j,i] = zslope
                    n=n+1
                    if n==nlev:
                       break    
    return interp_alt


def interpolate(var3D,lev,interp):
    var2D=np.zeros((var3D.shape[1],var3D.shape[2]),dtype='f')
    for i in range(0,var3D.shape[2]):
        for j in range(0,var3D.shape[1]):
            var2D[j,i] = var3D[interp[lev]['below'][j,i],j,i]+interp[lev]['slope'][j,i]*(var3D[interp[lev]['above'][j,i],j,i]-var3D[interp[lev]['below'][j,i],j,i])
    return var2D



###################################################################
# Basic plots (plot2D + plot on pressure, altitude or model levels)
###################################################################

def plot_2D(output,sampling,var2D,lat,lon,title,unit,mini=None,colors=None,levels=None,extend='neither',temp=None):
    print(title)
    m=plt.axes()
    if colors != None:
        cmap=None
    else:
        cmap=plt.get_cmap("rainbow")
    if mini != None:
        var2D[np.where(var2D<mini)]=float('nan')
    cs = m.contourf(var2D[::sampling,::sampling],cmap=cmap,levels=levels,extend=extend,colors=colors)
    cb = plt.colorbar(cs,format='%.1e')
    cb.set_label(unit, labelpad=-40, y=1.05, rotation=0)
    if temp != None:
        over=m.contour(temp[::sampling,::sampling],levels=[-30,-25,-20,-15,-10,-5,0], cmap=get_cmap("winter_r"), linewidths=1.)
        plt.clabel(over,fmt="%1.0f", fontsize=10)
    plt.title(title+' - max : '+'%.2e'%np.nanmax(var2D),fontsize=12)
    plt.savefig(output,format='pdf')
    plt.close()



def plot_mod(output,sampling,var3D,lat,lon,mlev,title,unit,mini=None,levels=None,temp=None):
    var2D=var3D[mlev,:,:]
    if temp != None:
        temp2D=temp[mlev,:,:]
    else:
        temp2D=None
    if mini != None:
        plot_2D(output,sampling,var2D,lat,lon,title+' at level '+str(mlev),unit,mini,levels=levels,temp=temp2D)
    else:
        plot_2D(output,sampling,var2D,lat,lon,title+' at level '+str(mlev),unit,levels=levels,temp=temp2D)



def plot_pres(output,sampling,var3D,lat,lon,lev,interp,title,unit,mini=None,levels=None,colors=None,temp=None):
    var2D=interpolate(var3D,lev,interp)
    if temp != None:
        temp2D=interpolate(temp,lev,interp)
    else:
        temp2D=None
    if mini != None:
        plot_2D(output,sampling,var2D,lat,lon,title+' at '+str(lev)+' hPa',unit,mini=mini,levels=levels,colors=colors,temp=temp2D)
    else:
        plot_2D(output,sampling,var2D,lat,lon,title+' at '+str(lev)+' hPa',unit,levels=levels,colors=colors,temp=temp2D)
        

def plot_alt(output,sampling,var3D,lat,lon,lev,interp,title,unit,mini=None,levels=None,temp=None):
    var2D=interpolate(var3D,lev,interp)
    if temp != None:
        temp2D=interp_alt(temp,lev,interp)
    else:
        temp2D=None
    if mini != None:
        plot_2D(output,sampling,var2D,lat,lon,title+' at '+str(lev)+' m',unit,mini=mini,levels=levels,temp=temp2D)
    else:
        plot_2D(output,sampling,var2D,lat,lon,title+' at '+str(lev)+' m',unit,levels=levels,temp=temp2D)


def plot_vert_integ(output,sampling,var3D,lat,lon,alt,title,unit,mini=None,levels=None,temp=None):
    var2D=np.zeros((var3D.shape[1],var3D.shape[2]),dtype='f')
    if temp != None:
        temp2D=temp[0,:,:]
    else:
        temp2D=None
    for k in range(0,var3D.shape[0]-1):
        var2D[:,:]=var2D[:,:]+var3D[k,:,:]*(alt[k+1,:,:]-alt[k,:,:])
    if mini != None:
        plot_2D(output,sampling,var2D,lat,lon,title,unit,mini=mini,levels=levels,temp=temp2D)
    else:
        plot_2D(output,sampling,var2D,lat,lon,title,unit,levels=levels,temp=temp2D)



########################
# Plot several variables
########################



def plot_2D_multi(output,sampling,var2D,lat,lon,title,unit,mini=None,colors=None,levels=None,extend='neither',temp=None):
    print(title)
    m=plt.axes(projection=ccrs.Mercator())
    m.set_extent([np.nanmin(lon),np.nanmax(lon),np.nanmin(lat),np.nanmax(lat)])
    m.coastlines()
    m.add_feature(cpf.STATES, linewidth=0.5)
    m.gridlines()
    x,y = m(lon[::sampling,::sampling],lat[::sampling,::sampling])
    if colors != None:
        cmap=None
    else:
        cmap=plt.get_cmap("rainbow")
    if mini != None:
        var2D[np.where(var2D<mini)]=float('nan')
    cs = m.contourf(x,y,var2D[::sampling,::sampling],cmap=cmap,levels=levels,extend=extend,colors=colors)
    cb=plt.colorbar(cs,format='%.1e')
    cb.set_label(unit, labelpad=-40, y=1.05, rotation=0)
    if temp != None:
        over=m.contour(x,y,temp[::sampling,::sampling],levels=[-30,-25,-20,-15,-10,-5,0], cmap=get_cmap("winter_r"), linewidths=1.)
        plt.clabel(over,fmt="%1.0f", fontsize=10)
    plt.title(title+' - max : '+'%.2e'%np.nanmax(var2D),fontsize=12)
    plt.savefig(output,format='pdf')
    plt.close()



