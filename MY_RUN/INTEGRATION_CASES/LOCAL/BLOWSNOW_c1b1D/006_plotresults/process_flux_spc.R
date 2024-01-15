# Read SPC Data for the corresponding event and put them in a convenint format for further comparison with Meso-NH

data_spc=read.table('flux_spc_2011023.csv',sep=';',header=T)
date_spc= paste("2011",sprintf("%02d",data_spc$MM),data_spc$JJ,sprintf("%02d",data_spc$HH),sprintf("%02d",data_spc$MM.1),sep='')
date_spc= strptime(date_spc,'%Y%m%d%H%M')

# Remove SPC data whrn SPS is burried under snow
mask_burried=which(data_spc$HautSPCB<0)
data_spc$FluxB[mask_burried]=NaN

# Apply limitation on particle number flux
mask_r_bas=which(data_spc$HautSPCB<0 | data_spc$nb_particules_B<500)
mask_r_haut=which(data_spc$nb_particules_H<500)
mask_r_fixe=which(data_spc$nb_particules_F<500)

data_spc$FluxB[mask_r_bas]=NaN
data_spc$FluxH[mask_r_haut]=NaN
data_spc$FluxF[mask_r_fixe]=NaN

# Convert into mass fluxes in kg/m2/s 
flux=10000*cbind(data_spc$FluxB,data_spc$FluxH,data_spc$FluxF)

#Contains heigh of SPC
hflux=cbind(data_spc$HautSPCB,data_spc$HautSPCH,data_spc$HautSPCF)

# Process mean radius data
data_spc$MoyB[mask_r_bas]=NaN
data_spc$MoyH[mask_r_haut]=NaN
data_spc$MoyF[mask_r_fixe]=NaN
radius = cbind(data_spc$MoyB,data_spc$MoyH,data_spc$MoyF)/2

#Correct wind speed from USA1 to a standard 5-m height using roughness length 
# reported in Vionnet (PhD, 2012) for blowing snow eventw with wind blowing 
# from the north at Col du Lac Blanc 
h_us=data_spc$HautSPCF+1.2

href=5
z0=0.00013
ucorr_us=data_spc$VitUS*log(href/z0)/log(h_us/z0)

# Classify the SPC data (mass flux and mean radius) by caterogy of 5-m wind speed
conc_cat=list(matrix(0,nrow=6,ncol=1))
for(i in 1:6){
    conc_cat[i]<- list(matrix(0,nrow=6,ncol=1))
}

ind=1

# j correspond to the different wind speed categories
for(j in 10.5:15.5){

mask=which(ucorr_us>j &ucorr_us<j+1)
mask_radius=which(radius[,2]>40)

mask=intersect(mask,mask_radius)
us=mean(ucorr_us[mask])

speed=rep(0,9)
speed[1]=us

res=cbind(flux[mask,],radius[mask,],hflux[mask,])
res=cbind(speed,t(res))

conc_cat[[ind]]=res

ind=ind+1
}

# Save data for further use
save(conc_cat,file='conc_cat_spc_R')


