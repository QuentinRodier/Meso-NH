#!/usr/bin/python3
# -*- coding: utf-8 -*-

from plot_MNH_basics_cartopy import *
from netCDF4 import Dataset
from matplotlib.backends.backend_pdf import PdfPages
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cartopy.feature as cpf

mpl.use('Agg')

mpl.rcParams["font.size"] = 3
levels=[1.E-12,1.E-10,1.E-8,1.E-6,2.E-6,5.E-6,1.E-5,2.E-5,5.E-5,1.E-4,2.E-4,5.E-4,1.E-3,2.E-3,5.E-3,1.E-2]
sampling=4
plevels=[]
alevels=[]

for folder in 'LIMA2M',:
#    for simu in 'SPLIT_ADJ', 'SPLIT_ADJ_ICE1M', 'SPLIT_ADJ_ICE1M_JW', 'SPLIT_NOADJ', 'SPLIT_SBG_ADJ', 'SPLIT_SPRO':
#    for simu in 'ICE3', '11111', '22111', '22211' :
#for folder in '550',:
#    for simu in '111110', '221110', '222110', :
#    for simu in '111110', '111111', '222110', '111110_JW_CIBU', '221110_JW_CIBU', '222110_JW_CIBU', '222111_JW_CIBU', '222222_JW_CIBU':
    for simu in 'ICE3', 'ICE3_JW':
        output_file = PdfPages(folder+'-'+simu+'.pdf')

        for ech in '06', '08', '10', '12', '14', '16', '18', '20', '22', '24', '26', '28', '30':
#        for ech in '01', '02', '03', '04', '05', '06', '07', '08' :
            print(folder+' '+simu+' '+ech)
            MNH_file=folder+'/'+simu+'/LIMAA.1.EXP01.0'+ech+'.nc'
            
            #################################
            # Read data from the Meso-NH file
            #################################
            
            fic=Dataset(MNH_file,'r')
            valid_time,lat,lon = read_MNH_output_conf(fic)
            
            f={}
            u={}
            read_MNH_output_var(['RCT', 'RRT', 'RIT', 'RST', 'RGT', 'RHT'],fic,f,u)
#            read_MNH_output_var(['RCT', 'RRT', 'RIT', 'RST', 'RGT'],fic,f,u)
                                
                
            ###########################
            # Horizontal cross sections
            ###########################
                
            fig,ax=plt.subplots(4,6,sharex=True,sharey=True)
            int=0
            for lev in 20,35,50,65:
                for field in 'rc','rr','ri','rs','rg','rh':
#                for field in 'rc','rr','ri','rs','rg','rg':
                    var2D=f[field][lev,:,:]
                    cs=ax[int//6,int-6*(int//6)].contourf(var2D,cmap='jet',levels=levels,extend='max',norm=mpl.colors.LogNorm())
                    cs.cmap.set_under('w')
                    cs.changed()
                    cb=plt.colorbar(cs,format='%.1e',ax=ax[int//6,int-6*(int//6)])
                    cb.set_label(u['rc'], labelpad=-40, y=1.05, rotation=0)
                    ax[int//6,int-6*(int//6)].set_title(field+' at level '+str(lev))
                    ax[int//6,int-6*(int//6)].set_aspect('equal')
                    int=int+1
            plt.savefig(output_file,format='pdf')
            plt.close()


            ################
            # Close MNH file
            ################

            fic.close()

        ###################
        # Close output file
        ###################
        
        output_file.close()
