#!/usr/bin/python3
# -*- coding: utf8 -*-
import matplotlib.pyplot as plt, cartopy.crs as ccrs
import epygram ; epygram.init_env() #Initialisation d'Epygram indispensable
 
MON_FICHIER_GRIB_A_TRACER="SPWAN.1.25m00.003.nc"
f=epygram.formats.resource(MON_FICHIER_GRIB_A_TRACER,'r')
t2m = f.readfield("THT").getlevel(k=100)

t2m.operation('-', 273.15)
fig,ax =t2m.cartoplot(figsize=(14,12),natural_earth_features="",epygram_departments=True,minmax=[10.31,10.3625],colorsnumber=21,colormap='nipy_spectral', parallels=None,meridians=None,projection=ccrs.Mercator(),plot_method="contourf")
plt.tight_layout()
plt.savefig("ma_super_image.png"); 
plt.close()
