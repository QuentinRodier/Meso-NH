#!/usr/bin/python3
# -*- coding: utf-8 -*-

from netCDF4 import Dataset,num2date
from matplotlib.backends.backend_pdf import PdfPages
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.colors as colors
import cartopy.crs as ccrs
import cartopy.feature as cpf

mpl.use('Agg')

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
    MesoNH_names={'ZS':'orography', 'ACPRT':'acc_precip', 'ACPRR':'acc_rain', 'ACPRS':'acc_snow', 'ACPRG':'acc_graupel', 'ACPRH':'acc_hail', 'INPRT':'precip_rate', 'INPRR':'rain_rate', 'INPRS':'snow_rate', 'INPRG':'graupel_rate', 'INPRH':'hail_rate', 'PABST':'P', 'THT':'theta', 'RVT':'rv', 'RCT':'rc', 'RRT':'rr', 'RIT':'ri', 'RST':'rs', 'RGT':'rg', 'RHT':'rh', 'CCLOUDT':'Nc', 'CRAINT':'Nr', 'CICET':'Ni', 'UT':'u', 'VT':'v', 'WT':'w'}
    computable=['alt','T','T_K','RH_liq','RH_ice','LWC','IWC','Dc','Dr']
    for variable in variables:
        if not (variable in fields):
            if variable in ['ZS']:
                var=file.variables[variable]
                fields[MesoNH_names[variable]]=var[:,:]
                units[MesoNH_names[variable]]=var.units
            elif variable in ['ACPRT', 'ACPRR', 'ACPRS', 'ACPRG', 'ACPRH', 'INPRT', 'INPRR', 'INPRS', 'INPRG', 'INPRH']:
                try:
                    var=file.variables[variable]
                    fields[MesoNH_names[variable]]=var[0,:,:]
                    units[MesoNH_names[variable]]=var.units
                except:
                    var=file.variables['THT']
                    fields[MesoNH_names[variable]]=var[0,0,:,:]*0.
                    units[MesoNH_names[variable]]='none'
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
                read_MNH_output_var(['ACPRT', 'ACPRR', 'ACPRS', 'ACPRG', 'ACPRH', 'INPRT', 'INPRR', 'INPRS', 'INPRG', 'INPRH'],file,fields,units)
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

def truncate_colormap(cmap, minval=0.0, maxval=1.0, n=100):
    new_cmap = colors.LinearSegmentedColormap.from_list(
        'trunc({n},{a:.2f},{b:.2f})'.format(n=cmap.name, a=minval, b=maxval),
        cmap(np.linspace(minval, maxval, n)))
    return new_cmap
new_Greys=truncate_colormap(plt.get_cmap('Greys'), 0.5, 1.)
new_Greens=truncate_colormap(plt.get_cmap('Greens'), 0.5, 1.)
new_Oranges=truncate_colormap(plt.get_cmap('Oranges'), 0.5, 1.)
new_Reds=truncate_colormap(plt.get_cmap('Reds'), 0.5, 1.)
new_Blues=truncate_colormap(plt.get_cmap('Blues'), 0.5, 1.)
new_Purples=truncate_colormap(plt.get_cmap('Purples'), 0.5, 1.)

mpl.rcParams["font.size"] = 3

xmin=300
xmax=600
ymin=50
ymax=350
exp=["02_run_LIMA111111","02_run_LIMA222222_JW_CIBU","02_run_LIMA222110","02_run_ICE4"]
#exp=["02_run_LIMA111111"]
for folder in '../',:
    for simu in exp:

        output_file = PdfPages('SUPERCELL-'+simu[7:]+'.pdf')

        ########################
        # Plot 1 : mixing ratios
        ########################
        for ech in '28', :
            print(simu+' '+ech)
            MNH_file=folder+simu+'/LIMAA.1.EXP01.0'+ech+'.nc'
            # Read data from the Meso-NH file            
            print(simu)
            fic=Dataset(MNH_file,'r')
            valid_time,lat,lon = read_MNH_output_conf(fic)
            print(valid_time)
            f={}
            u={}
            read_MNH_output_var(['RCT', 'RRT', 'RIT', 'RST', 'RGT', 'RHT', 'alt', 'T'],fic,f,u)
            y=np.unravel_index(np.argmax(f['rc'][50,:,:]),f['rc'][50,:,:].shape)[0]
            # Horizontal cross sections
            levels=[1.E-12,1.E-10,1.E-8,1.E-6,2.E-6,5.E-6,1.E-5,2.E-5,5.E-5,1.E-4,2.E-4,5.E-4,1.E-3,2.E-3,5.E-3,1.E-2]
            fig,ax=plt.subplots(4,6,sharex=True,sharey=True)
            i=0
            for lev in 20,35,50,65:
                for field in 'rc','rr','ri','rs','rg','rh':
                    var2D=f[field][lev,ymin:ymax,xmin:xmax]
                    cs=ax[i//6,i-6*(i//6)].contourf(var2D,cmap='jet',levels=levels,extend='max',norm=mpl.colors.LogNorm())
                    cs.cmap.set_under('w')
                    cs.changed()
                    ax[i//6,i-6*(i//6)].plot([0,xmax-xmin],[y-ymin,y-ymin],color = "Black",alpha=0.5,linestyle='dotted',linewidth=0.5)
                    ax[i//6,i-6*(i//6)].set_title(field+', lvl='+str(lev)+', t='+str(int(ech)*5)+"min",fontsize=5)
                    ax[i//6,i-6*(i//6)].set_aspect('equal')
                    i=i+1
            plt.subplots_adjust(left=0.05, bottom=0.1, right=1.05, top=0.9)
            cb=fig.colorbar(cs,format='%.1e',shrink=0.5,ax=ax.ravel().tolist())
            cb.set_label(u['rc'], labelpad=0, y=1.05, rotation=0)
            # Save fig, close plot
            plt.savefig(output_file,format='pdf')
            plt.close()

            ########################
            # Plot 2 : cross section
            ########################
            fig,ax=plt.subplots(1,1,sharex=True,sharey=True)
            levels=np.logspace(-3,1.,50)
            #x.set_yscale('log')
            alt=f['alt'][:,y,xmin]
            plt.ylim(ymax=20000)
            x=np.arange(0,xmax-xmin,1)
            ax.contourf(x,alt,f['ri'][:,y,xmin:xmax]*1000,cmap=new_Greys,alpha=0.5,levels=levels,extend="max")
            ax.contour(x,alt,f['ri'][:,y,xmin:xmax]*1000,colors="Grey",alpha=0.5,levels=[0.001])
            ax.contour(x,alt,f['ri'][:,y,xmin:xmax]*1000,colors="Grey",alpha=0.5,levels=[0.1],linestyles='dashed')
            ax.contourf(x,alt,f['rs'][:,y,xmin:xmax]*1000,cmap=new_Greens,alpha=0.5,levels=levels,extend="max")
            ax.contour(x,alt,f['rs'][:,y,xmin:xmax]*1000,colors="Green",alpha=0.5,levels=[0.001])
            ax.contour(x,alt,f['rs'][:,y,xmin:xmax]*1000,colors="Green",alpha=0.5,levels=[0.1],linestyles='dashed')
            ax.contourf(x,alt,f['rg'][:,y,xmin:xmax]*1000,cmap=new_Oranges,alpha=0.5,levels=levels,extend="max")
            ax.contour(x,alt,f['rg'][:,y,xmin:xmax]*1000,colors="Orange",alpha=1.,levels=[0.001])
            ax.contour(x,alt,f['rg'][:,y,xmin:xmax]*1000,colors="Orange",alpha=1.,levels=[0.1],linestyles='dashed')
            ax.contourf(x,alt,f['rh'][:,y,xmin:xmax]*1000,cmap=new_Reds,alpha=0.5,levels=levels,extend="max")
            ax.contour(x,alt,f['rh'][:,y,xmin:xmax]*1000,colors="Red",alpha=1.,levels=[0.001])
            ax.contour(x,alt,f['rh'][:,y,xmin:xmax]*1000,colors="Red",alpha=1.,levels=[0.1],linestyles='dashed')
            ax.contourf(x,alt,f['rc'][:,y,xmin:xmax]*1000,cmap=new_Blues,alpha=0.5,levels=levels,extend="max")
            ax.contour(x,alt,f['rc'][:,y,xmin:xmax]*1000,colors="Blue",alpha=0.5,levels=[0.001])
            ax.contour(x,alt,f['rc'][:,y,xmin:xmax]*1000,colors="Blue",alpha=0.5,levels=[0.1],linestyles='dashed')
            ax.contourf(x,alt,f['rr'][:,y,xmin:xmax]*1000,cmap=new_Purples,alpha=0.5,levels=levels,extend="max")
            ax.contour(x,alt,f['rr'][:,y,xmin:xmax]*1000,colors="Purple",alpha=0.5,levels=[0.001])
            ax.contour(x,alt,f['rr'][:,y,xmin:xmax]*1000,colors="Purple",alpha=0.5,levels=[0.1],linestyles='dashed')
            ax.contour(x,alt,f['T'][:,y,xmin:xmax],colors="Black",alpha=0.5,levels=[0],linestyles='dotted')
            #ax.invert_yaxis()
            ax.set_title("Cross section of hydrometeors mixing ratios"+', t='+str(int(ech)*5)+"min",fontsize=10)
            #Fake plot for legend
            ax.plot(1,1,color = "Blue",label = "cloud",alpha=0.5)
            ax.plot(1,1,color = "Purple",label = "rain",alpha=0.5)
            ax.plot(1,1,color = "Grey",label = "ice",alpha=0.5)
            ax.plot(1,1,color = "Green",label = "snow",alpha=0.5)
            ax.plot(1,1,color = "Orange",label = "graupel",alpha=0.5)
            ax.plot(1,1,color = "Red",label = "hail",alpha=0.5)
            ax.plot(1,1,color = "Black",label = "0.001 g.kg$^{-1}$",alpha=0.5)
            ax.plot(1,1,color = "Black",label = "0.1 g.kg$^{-1}$",alpha=0.5,linestyle='dashed')
            ax.plot(1,1,color = "Black",label = "T=0°C",alpha=0.5,linestyle='dotted')
            ax.legend(fontsize = 7, loc="lower right")  
            # Save fig, close plot and MNH file
            plt.savefig(output_file,format='pdf')
            plt.close()
            fic.close()

        ##########################################
        # Plot 3 : rain intensities & accumulation
        ##########################################0
        cmap = mpl.cm.get_cmap('gist_rainbow_r')
        fig=plt.figure()
        ax =fig.add_subplot(1, 2, 1)
        # plot sequence of intensities
        i=0
        #for ech in '06', :
        for ech in '06', '08', '10', '12', '14', '16', '18', '20', '22', '24', '26', '28', '30':
            print(folder+' '+simu+' '+ech)
            MNH_file=folder+simu+'/LIMAA.1.EXP01.0'+ech+'.nc'
            # Read data from the Meso-NH file            
            fic=Dataset(MNH_file,'r')
            valid_time,lat,lon = read_MNH_output_conf(fic)
            f={}
            u={}
            read_MNH_output_var(['INPRT'],fic,f,u)
            # Horizontal cross sections
            var2D=f['precip_rate'][:,:]
            ax.contour(var2D,levels=[15.],colors=[cmap(i/12)])
            ax.plot(1,1,color=cmap(i/12),label=str(int(ech)*5)+"min")
            i=i+1
            fic.close()
        ax.legend(fontsize = 6, loc="upper left",ncol=3)
        ax.set_title("15mm h$^{-1}$ precipitation rate",fontsize=10)
        ax.set_aspect('equal')
        # plot accumulated precipitation
        MNH_file=folder+simu+'/LIMAA.1.EXP01.036.nc'
        fic=Dataset(MNH_file,'r')
        valid_time,lat,lon = read_MNH_output_conf(fic)
        f={}
        u={}
        read_MNH_output_var(['ACPRR','ACPRS','ACPRG','ACPRH'],fic,f,u)
        i=0
        levels=[0.2,0.5,1,2,5,10,15,20,25,30]
        for var in 'acc_rain', 'acc_snow', 'acc_graupel', 'acc_hail' :
            ax=fig.add_subplot(2, 4, 3+(i//2)*2+i)
            ax.contour(f['acc_rain'][:,:],levels=[2],colors=['grey'],linewidths=[0.5])
            cf=ax.contourf(f[var][:,:],levels=levels,extend='max',cmap='jet')
            ax.set_title(var,fontsize=10)
            ax.set_aspect('equal')
            cb=plt.colorbar(cf,orientation='horizontal')
            i=i+1
        # Save fig, close plot and MNH file
        plt.savefig(output_file,format='pdf')
        plt.close()

        ###################
        # Close output file
        ###################
        
        output_file.close()
