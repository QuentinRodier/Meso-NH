#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Feb 25 11:31:30 2020

@author: wurtzj / vie
"""

import warnings
warnings.filterwarnings("ignore")

import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.colors as colors
import matplotlib.ticker as mticker
from matplotlib.backends.backend_pdf import PdfPages

import numpy as np

import cartopy.crs as ccrs
from cartopy.mpl.gridliner import LONGITUDE_FORMATTER, LATITUDE_FORMATTER

import netCDF4 as nc4

import os

lines = {
    'INIF':('gray','solid'),
    'ENDF':('gray','dashed'),
    'AVEF':('gray','dotted'),
    
    'ASSE':('pink','solid'),
    'FRC':('pink','dashed'),
    'REL':('pink','dotted'),
    'DIF':('pink','dashdot'),
    
    'VTURB':('plum','solid'),
    'HTURB':('plum','dashed'),
    'NETUR':('plum','dotted'),
    
    'ADV':('mediumpurple','solid'),
    'NEADV':('mediumpurple','dotted'),
    
    'NEGA':('burlywood','dotted'),
    
    'ADJU':('darkblue','solid'),
    'CDEPI':('darkblue','dashed'),
    'DEPI':('darkblue','dashed'),
    'CEDS':('darkblue','dashed'),
    'BERFI':('darkblue','dotted'),
    
    'CORR':('lightgray','solid'),
    
    'SEDI':('lightgray','dashed'),
    'DEPO':('lightgray','dotted'),
    
    'R2C1':('lightskyblue','solid'),
    'CVRC':('lightskyblue','dashed'),
    
    'HENU':('cyan','solid'),
    
    'AUTO':('cyan','dashed'),
    'SELF':('cyan','dotted'),
    'ACCR':('darkcyan','solid'),
    'REVA':('darkcyan','dashed'),
    'SCBU':('darkcyan','dotted'),
    'BRKU':('darkcyan','dashdot'),
    
    'HON':('palegreen','solid'),
    'HIND':('palegreen','dashed'),
    
    'DEPS':('forestgreen','dashed'),
    'DEPG':('forestgreen','dotted'),
    
    'CNVI':('limegreen','solid'),
    'CNVS':('limegreen','dashed'),
    'AUTS':('limegreen','dashed'),
    'AGGS':('limegreen','dotted'),
    
    'CMEL':('darkgreen','solid'),
    
    'HINC':('tan','solid'),
    'HONC':('tan','dashed'),
    'HONH':('tan','dotted'),
    'HONR':('tan','dashdot'),
    'SFR':('tan','dashdot'),
    
    'RIM':('sandybrown','solid'),
    'HMS':('sandybrown','dashed'),
    
    'ACC':('chocolate','solid'),
    'CFRZ':('chocolate','dotted'),
    'RDSF':('chocolate','dashed'),
    
    'WETG':('saddlebrown','solid'),
    'DRYG':('saddlebrown','dashed'),
    'HMG':('saddlebrown','dotted'),
    'CIBU':('saddlebrown','dashdot'),
    
    'GMLT':('bisque','dotted'),
    'IMLT':('bisque','dashed'),
    
    'NECON':('beige','dotted'),
}

def read_MNH_budgets_conf(file):
    tmp1=file['Budgets']['RhodJ']['RhodJS']
    d3=tmp1.shape[2]
    d2=tmp1.shape[1]
    d1=tmp1.shape[0]
    return d1,d2,d3

def read_MNH_budgets_var(var,step,procs,file,fields,units):
    proc_dict = {name:value for name, value in file['Budgets'][var].variables.items()}
    proc_comment = {name:value.comment.split(' ',1)[0] for name, value in proc_dict.items()}
    proc_unit={}
    for name, value in proc_dict.items():
        try:
            proc_unit[name]=value.unit
        except AttributeError:
            proc_unit[name]=''
    for v in proc_dict.keys():
        fields[v]=proc_dict[v][:,:,:]
        units[v]=proc_unit[v]

def truncate_colormap(cmap, minval=0.0, maxval=1.0, n=100):
    new_cmap = colors.LinearSegmentedColormap.from_list(
        'trunc({n},{a:.2f},{b:.2f})'.format(n=cmap.name, a=minval, b=maxval),
        cmap(np.linspace(minval, maxval, n)))
    return new_cmap
new_Greys=truncate_colormap(plt.get_cmap('Greys'), 0.5, 1.)
new_Greens=truncate_colormap(plt.get_cmap('Greens'), 0.5, 1.)
new_Oranges=truncate_colormap(plt.get_cmap('Oranges'), 0.5, 1.)
new_Blues=truncate_colormap(plt.get_cmap('Blues'), 0.5, 1.)
new_Purples=truncate_colormap(plt.get_cmap('Purples'), 0.5, 1.)

def rho_air_sec(T,P):
    M = 28.965338*1e-3
    R = 8.314
    return P*M/(R*T)

def water_path_MNH(MICRO3D_data,Pressure_levels): #from fonction_plot
    # P en Pa <-> kg.m-1.s-2
    # MICRO3D_data en kg.kg-1
    g=9.80665 #constant gravite m.s-2
    nz,nx,ny = np.shape(MICRO3D_data)
    MICRO_WP_data = np.zeros((nx,ny))
    for z in range(0,nz-1):
        MICRO_WP_data[:,:] = MICRO_WP_data[:,:]+np.abs((Pressure_levels[z+1,:,:]-Pressure_levels[z,:,:]))*(MICRO3D_data[z,:,:])
    MICRO_WP_data = MICRO_WP_data/g
    return MICRO_WP_data #en kg/m-2 numpy 2D

simulations=os.listdir('../')
simulations2=os.listdir('../')
for sim in simulations2:
    if (not os.path.isdir("../"+sim)) or (sim[3:7]!='ICE3' and sim[3:7]!='LIMA') :
        simulations.remove(sim)

CHEMIN_SAVE = './'
CHEMIN_NCDF = "../"

Colors_TEMPE = plt.get_cmap("viridis_r")
c_conv = "khaki"
c_strat = "lightseagreen"
c_cirri = "blueviolet"

lon_min, lon_max, lat_min, lat_max = (-53.5, -48.99, 1.99, 6.01)
print(simulations)
for simu in simulations :

    #################################################
    # Page 1 
    #################################################

    fig=plt.figure(figsize=(39,26))
    i=0
    NOM_EXP=simu[3:]
    output_file = PdfPages(NOM_EXP+'.pdf')
    print(NOM_EXP)

    liste_ncdf=[]
    for heure in "08", "10", "20":
        liste_ncdf.append(simu[3:7]+'_.1.SEG01.0'+heure+'.nc')
    liste_ncdf.sort()

    for NCDF_file in liste_ncdf :
        if not "000" in NCDF_file : #excluding first file
            i=i+1
            ncfile = nc4.Dataset(CHEMIN_NCDF+simu+"/"+NCDF_file,'r', format='NETCDF4')
            date = NCDF_file[0:8]
            heure =  NCDF_file[15:17]
            print(heure)
            #Getting variables (converted to g/m3 for mixing ratios)
            THETA = ncfile.variables['THT'][:,:,:][0,:,:,:]
            PRESSION = ncfile.variables["PABST"][:,:,:][0,:,:,:]
            TEMPERATURE = THETA*(100000/PRESSION)**(-2/7)
            RHO = rho_air_sec(TEMPERATURE, PRESSION)
            IWC= ncfile.variables['RIT'][:,:,:][0,:,:,:]*RHO*1000
            SNOW= ncfile.variables['RST'][:,:,:][0,:,:,:]*RHO*1000
            GRAUPEL= ncfile.variables['RGT'][:,:,:][0,:,:,:]*RHO*1000
            LWC = ncfile.variables['RCT'][:,:,:][0,:,:,:]*RHO*1000
            RAIN = ncfile.variables['RRT'][:,:,:][0,:,:,:]*RHO*1000            
            W_WIND = ncfile.variables["WT"][:,:,:][0,:,:,:]
            LAT = ncfile.variables["latitude"][:,:]
            LON = ncfile.variables["longitude"][:,:]
            TXRR = ncfile.variables["INPRT"][:,:][0,:,:]
            ACRR = ncfile.variables["ACPRT"][:,:][0,:,:]
            ncfile.close()
        
            #Getting maximum vertical velocity for convective region
            MAX_VV = np.nanmax(np.abs(W_WIND[:,:,:]),axis=0)        
            #computing total ice and total water contents
            TIWC = IWC+SNOW+GRAUPEL
            TWC = TIWC + RAIN+LWC
            #ice water path
            indice_T_273K = np.where(np.abs(np.nanmean(TEMPERATURE,axis=(1,2))-273)<1)[0][0]
            indices_WP = np.arange(indice_T_273K,np.shape(IWC)[0],1) 
            IWP = water_path_MNH((TIWC[indices_WP,:,:])/RHO[indices_WP,:,:]/1000,PRESSION[indices_WP,:,:])
            #criteria on IWP (kg/m2) maximum vertical velocity (m/s) and rainfall rate (mm/h)
            seuil_IWP = 0.5
            Crit_conv_TF = ((MAX_VV>2.5) & (IWP>seuil_IWP)) | ((TXRR>15)& (IWP>seuil_IWP) )
            Crit_strat_TF = (np.logical_not(Crit_conv_TF))& (TXRR>seuil_IWP)  & (IWP>seuil_IWP) # pour l'affichage j'enlève la negation
            Crit_cirri_TF = (np.logical_not(Crit_conv_TF))& (np.logical_not(Crit_strat_TF)) & (IWP>seuil_IWP)# pour l'affichage j'enlève la negation
            #flagging regions
            Crit_conv = np.where(Crit_conv_TF)
            Crit_strat = np.where(Crit_strat_TF)
            Crit_cirri = np.where(Crit_cirri_TF)
            a,b = np.shape(Crit_conv_TF)
            Crit_conv_plot = np.zeros((a,b))
            Crit_strat_plot = np.zeros((a,b))
            Crit_cirri_plot = np.zeros((a,b))
            Crit_DIAG_plot = np.zeros((a,b))
            for k in range(a):
                for j in range(b):
                    if Crit_conv_TF[k,j]:
                        Crit_conv_plot[k,j] = 1
                    if Crit_strat_TF[k,j]:
                        Crit_strat_plot[k,j] = 1
                    if Crit_cirri_TF[k,j]:
                        Crit_cirri_plot[k,j] = 1

            #plot conv/start/cirri and TWC at -30°C
            level_chosen = np.where(np.abs(np.nanmean(TEMPERATURE,axis=(1,2))-243)<2)[0][0]
            TWC[0,0,level_chosen]=2.75 # for clean plot
            ax = fig.add_subplot(2, 3, i, projection=ccrs.PlateCarree())
            ax.set_extent([lon_min, lon_max, lat_min, lat_max])  
            ax.contourf(LON,LAT,Crit_strat_plot,levels=[0.1,1],colors = c_strat, label = "Stratiforme")
            ax.contourf(LON,LAT,Crit_conv_plot,levels=[0.1,1],colors = c_conv, label = "Convectif")
            ax.contourf(LON,LAT,Crit_cirri_plot,levels =[0.9,1],colors = c_cirri, label = "Convectif")
            ax.contour(LON,LAT,TIWC[level_chosen,:,:],levels =[0.5,1,2],colors = ["yellow","orange","red"],linewidths=3)
            ax.plot(LON[84,75:225],LAT[84,75:225],color = "Grey",linewidth = 1)
            gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
                              linewidth=2, color='gray', alpha=0.5, linestyle='--')
            gl.xlabels_top = False
            gl.ylabels_left = True
            gl.ylabels_right = False
            gl.xlocator = mticker.FixedLocator(np.arange(-56,-46,1))
            gl.ylocator = mticker.FixedLocator(np.arange(1,10,1))
            gl.xformatter = LONGITUDE_FORMATTER
            gl.yformatter = LATITUDE_FORMATTER
            gl.xlabel_style = {'size': 20}
            gl.ylabel_style = {'size': 20}
            ax.set_xlabel("LONGITUDE")
            ax.set_ylabel("LATITUDE")
            ax.set_title("TWC at -30°C, "+heure+"UTC",fontsize=20)
            #Fake plot for legend
            ax.plot(LON[0,0],LAT[0,0],color = c_conv,label = "Convectif",linewidth = 15)
            ax.plot(LON[0,0],LAT[0,0],color = c_strat,label = "Stratiforme",linewidth = 15)
            ax.plot(LON[0,0],LAT[0,0],color = c_cirri,label = "Cirriforme",linewidth = 15)
            ax.plot(LON[0,0],LAT[0,0],color = "yellow",label = "$0.5g.m^{-3}$",linewidth = 3)
            ax.plot(LON[0,0],LAT[0,0],color = "orange",label = "$1g.m^{-3}$",linewidth = 3)
            ax.plot(LON[0,0],LAT[0,0],color = "red",label = "$2g.m^{-3}$",linewidth = 3)
            ax.legend(bbox_to_anchor=(0.63, 0.58, 0.35, 1.), loc=3, ncol=1, mode="expand", borderaxespad=0., fontsize = 20)  

            # plot vertical cross section
            levels=np.logspace(-3,1.,50)
            p=PRESSION[:,84,75]/100
            lon=LON[84,75:225]
            ax = fig.add_subplot(2, 3, 3+i)
            ax.set_yscale('log')
            ax.contourf(lon,p,IWC[:,84,75:225],label="ice",cmap=new_Greys,alpha=0.5,levels=levels,extend="max")
            ax.contour(lon,p,IWC[:,84,75:225],label="ice",colors="Grey",alpha=0.5,levels=[0.001])
            ax.contour(lon,p,IWC[:,84,75:225],label="ice",colors="Grey",alpha=0.5,levels=[0.1],linestyles='dashed')
            ax.contourf(lon,p,SNOW[:,84,75:225],label="snow",cmap=new_Greens,alpha=0.5,levels=levels,extend="max")
            ax.contour(lon,p,SNOW[:,84,75:225],label="snow",colors="Green",alpha=0.5,levels=[0.001])
            ax.contour(lon,p,SNOW[:,84,75:225],label="snow",colors="Green",alpha=0.5,levels=[0.1],linestyles='dashed')
            ax.contourf(lon,p,GRAUPEL[:,84,75:225],label="graupel",cmap=new_Oranges,alpha=0.5,levels=levels,extend="max")
            ax.contour(lon,p,GRAUPEL[:,84,75:225],label="graupel",colors="Orange",alpha=1.,levels=[0.001])
            ax.contour(lon,p,GRAUPEL[:,84,75:225],label="graupel",colors="Orange",alpha=1.,levels=[0.1],linestyles='dashed')
            ax.contourf(lon,p,LWC[:,84,75:225],label="cloud",cmap=new_Blues,alpha=0.5,levels=levels,extend="max")
            ax.contour(lon,p,LWC[:,84,75:225],label="cloud",colors="Blue",alpha=0.5,levels=[0.001])
            ax.contour(lon,p,LWC[:,84,75:225],label="cloud",colors="Blue",alpha=0.5,levels=[0.1],linestyles='dashed')
            ax.contourf(lon,p,RAIN[:,84,75:225],label="rain",cmap=new_Purples,alpha=0.5,levels=levels,extend="max")
            ax.contour(lon,p,RAIN[:,84,75:225],label="rain",colors="Purple",alpha=0.5,levels=[0.001])
            ax.contour(lon,p,RAIN[:,84,75:225],label="rain",colors="Purple",alpha=0.5,levels=[0.1],linestyles='dashed')
            ax.contour(lon,p,TEMPERATURE[:,84,75:225],label="temperature",colors="Black",alpha=0.5,levels=[273.15],linestyles='dotted')
            ax.invert_yaxis()
            ax.set_title("Cross section of hydrometeors mixing ratios at 3.5N, "+heure+"UTC",fontsize=20)
            ax.set_xlabel("Longitude",fontsize=20)
            ax.set_ylabel("Pressure (hPa)",fontsize=20)
            #Fake plot for legend
            ax.plot(LON[84,75],p[1],color = "Blue",label = "cloud",alpha=0.5)
            ax.plot(LON[84,75],p[1],color = "Purple",label = "rain",alpha=0.5)
            ax.plot(LON[84,75],p[1],color = "Grey",label = "ice",alpha=0.5)
            ax.plot(LON[84,75],p[1],color = "Green",label = "snow",alpha=0.5)
            ax.plot(LON[84,75],p[1],color = "Orange",label = "graupel",alpha=0.5)
            ax.plot(LON[84,75],p[1],color = "Black",label = "$0.001g.m^{-3}$",alpha=0.5)
            ax.plot(LON[84,75],p[1],color = "Black",label = "$0.1g.m^{-3}$",alpha=0.5,linestyle='dashed')
            ax.plot(LON[84,75],p[1],color = "Black",label = "T=0°C",alpha=0.5,linestyle='dotted')
            ax.legend(bbox_to_anchor=(0.78, 0.01, 0.21, 1.), loc=3, ncol=1, mode="expand", borderaxespad=0., fontsize = 15)  

    plt.savefig(output_file,format='pdf')
    plt.close

    #################################################
    # Page 2
    #################################################

    fic=nc4.Dataset(CHEMIN_NCDF+simu+"/"+simu[3:7]+'_.1.SEG01.000.nc','r', format='NETCDF4')
    nmask,ntime,nz = read_MNH_budgets_conf(fic)
    masks=np.arange(0,nmask)
    region=["Convective","Stratiform","Cirriform"]
    z=np.arange(1,nz+1)
    budget_length=3600
    budget_number=10
    iea=('INIF','ENDF','AVEF')
    f={}
    u={}
    for variable in 'RC','RR','RI','RS','RG':
        f[variable]={}
        u[variable]={}
        read_MNH_budgets_var(variable,budget_number,'all',fic,f[variable],u[variable])

    fig=plt.figure(figsize=(39,26))
    
    colors={}
    colors['RC']="cyan"
    colors['RR']="blue"
    colors['RI']="gold"
    colors['RS']="orange"
    colors['RG']="brown"
    for mask in masks:
        ax=fig.add_subplot(3,6,mask*6+1)
        for variable in 'RC','RR','RI','RS','RG':
            v="AVEF"
            ax.plot(f[variable][v][mask,budget_number-1,:],z,label=variable,color=colors[variable])
            ax.set_title(region[mask],fontsize=20)
            ax.ticklabel_format(axis='x',style='sci',scilimits=(0,0))
            ax.tick_params(axis='both', which='major', labelsize=20)
            ax.xaxis.offsetText.set_fontsize(20)
            ax.set_yticks([])
            ax.set_xlim(left=0.,right=2.E-3)
            ax.legend(loc='lower right',fontsize='x-large')

    i=0
    factor=[10,1,0.5]
    for variable in 'RC','RR':
        i=i+1
        for mask in masks:
            ax=fig.add_subplot(3,6,mask*6+i+1)
            sum=np.zeros(nz)
            for key in lines.keys():
                v=key
                if v in f[variable].keys() and not key in iea:
                    ax.plot(f[variable][v][mask,budget_number-1,:],z,label=v,color=lines[key][0],linestyle=lines[key][1])
                    sum=sum+f[variable][v][mask,budget_number-1,:]
            ax.plot(sum,z,label="Total",color="black",linestyle="dotted",linewidth=0.5)
            ax.ticklabel_format(axis='x',style='sci',scilimits=(0,0))
            ax.tick_params(axis='both', which='major', labelsize=20)
            ax.xaxis.offsetText.set_fontsize(20)
            ax.set_title(region[mask]+" "+variable,fontsize=20)
            ax.set_yticks([])
            ax.set_xlim(left=-factor[mask]*5.E-7,right=factor[mask]*5.E-7)
            ax.legend(loc="upper right",fontsize='x-large', ncol=3)
    for variable in 'RI','RS','RG':
        i=i+1
        for mask in masks:
            ax=fig.add_subplot(3,6,mask*6+i+1)
            sum=np.zeros(nz)
            for key in lines.keys():
                v=key
                if v in f[variable].keys() and not key in iea:
                    ax.plot(f[variable][v][mask,budget_number-1,:],z,label=v,color=lines[key][0],linestyle=lines[key][1])
                    sum=sum+f[variable][v][mask,budget_number-1,:]
            ax.plot(sum,z,label="Total",color="black",linestyle="dotted",linewidth=0.5)
            ax.ticklabel_format(axis='x',style='sci',scilimits=(0,0))
            ax.tick_params(axis='both', which='major', labelsize=20)
            ax.xaxis.offsetText.set_fontsize(20)
            ax.set_title(region[mask]+" "+variable,fontsize=20)
            ax.set_yticks([])
            ax.set_xlim(left=-factor[mask]*5.E-7,right=factor[mask]*5.E-7)
            ax.legend(loc="lower right",fontsize='x-large', ncol=3)
    
    plt.tight_layout()
    plt.savefig(output_file,format='pdf')
    plt.close()

    output_file.close()

    
