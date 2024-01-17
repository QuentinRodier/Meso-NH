# Load Model Results
snw_data <-read.table('SNOW_DATA',colClasses=c("character","character"))

data_names= c('UT','SNWNOA','SNWMASS','SNWRGA','SNWSUBL3D')
data_names_1D= c('WSN_T_ISBA','COL_SNWSUBL','THDS','DSN_T_ISBA','SNW_SUBL_INS')
data_names_1D= c('COL_SNWSUBL','THDS','SNW_SUBL_INS')
data_names_canopy= c('ISBA_CAN_Z','CANSNW_NUM','CANSNW_MAS','CANSNW_RGL','ISBA_CAN_U')

numtot = as.numeric(snw_data[2,1])
nlevel_cano=5  #Number of vertical levels in canopy

data_fin_cano=rep(sprintf('%02d',1),length(data_names_canopy))
for(i in 2:nlevel_cano){
    data_fin_cano=c(data_fin_cano,rep(sprintf('%02d',i),length(data_names_canopy)))
}
data_names_canopy=paste(data_names_canopy,data_fin_cano,sep="")


lit_data = list(matrix(0,nrow=numtot+1,ncol=1))

for(i in 1:(length(data_names)+length(data_names_1D)+length(data_names_canopy))){
    lit_data[i]<- list(matrix(0,nrow=numtot+1,ncol=1))
}
names(lit_data) <- c(data_names,data_names_1D,data_names_canopy)

for(i in 1:(length(data_names)+length(data_names_1D)+length(data_names_canopy))){
for(j in 1:numtot){
lit_data[[i]][j+1,1] = as.numeric(snw_data[j+2,1])
}
lit_data[[i]]=as.data.frame(lit_data[[i]])
names(lit_data[[i]])<-c("ZZ")
}


for(idata in 1:length(data_names)) {

mask_data=which(snw_data$V1==data_names[idata])

data_sta <- matrix(0,nrow=numtot+1,ncol=length(mask_data))

for(j in 1:length(mask_data)){
        ind_beg=mask_data[j]+2
        ind_end=ind_beg+numtot-1
        data_sta[1,j] = as.numeric(snw_data[ind_beg-1,2])
        data_sta[2:(numtot+1),j] = as.numeric(snw_data[ind_beg:ind_end,2])
}

data_sta=as.data.frame(data_sta)


lit_data[[idata]] = cbind(lit_data[[idata]],data_sta)
}

for(idata in 1:length(data_names_1D)) {

mask_data=which(snw_data$V1==data_names_1D[idata])

data_sta <- matrix(0,nrow=numtot+1,ncol=length(mask_data))

for(j in 1:length(mask_data)){
        ind=mask_data[j]
        data_sta[1,j] = as.numeric(snw_data[ind+1,1])
        data_sta[2,j] = as.numeric(snw_data[ind+1,2])
}

data_sta=as.data.frame(data_sta)


lit_data[[idata+length(data_names)]] = cbind(lit_data[[idata+length(data_names)]],data_sta)
}

for(idata in 1:length(data_names_canopy)) {

mask_data=which(snw_data$V1==data_names_canopy[idata])

data_sta <- matrix(0,nrow=numtot+1,ncol=length(mask_data))

for(j in 1:length(mask_data)){
        ind=mask_data[j]
        data_sta[1,j] = as.numeric(snw_data[ind+1,1])
        data_sta[2,j] = as.numeric(snw_data[ind+1,2])
}

data_sta=as.data.frame(data_sta)


lit_data[[idata+length(data_names)+length(data_names_1D)]] = cbind(lit_data[[idata+length(data_names)+length(data_names_1D)]],data_sta)
}


lit_profile = list(matrix(0,nrow=numtot+nlevel_cano,ncol=1))

for(i in 1:6){
    lit_profile[i]<- list(matrix(0,nrow=numtot+nlevel_cano,ncol=1))
}
names(lit_profile) <- c('SNWNOA','SNWMASS','SNWRGA','UT','REHU','THT')

names_canopy=c('ISBA_CAN_Z','CANSNW_NUM','CANSNW_MAS','CANSNW_RGL','ISBA_CAN_U')

#for(i in 1:2){
#for(j in 1:nlevel_cano){
#nam=paste(names_canopy[1],j,sep="")
#ind=which(names(lit_data)==nam)
#lit_profile[[i][j+1,1]=lit_data[[ind]][2,2]
#}
lit_profile[[i]][(nlevel_cano+2):(numtot+nlevel_cano),1]=lit_data$SNWNOA$ZZ[2:numtot]
#names(lit_profile[[i]])<-c("ZZ")
#}

for(i in 1:4){

data_sta <- matrix(0,nrow=nlevel_cano+1,ncol=length(lit_data$SNWNOA))

data_sta[1,] = as.numeric(lit_data$SNWNOA[1,])

for(j in 1:nlevel_cano){
nam=paste(names_canopy[1],sprintf('%02d',j),sep="")
ind=which(names(lit_data)==nam)
data_sta[j+1,1]=lit_data[[ind]][2,2]
}

for(j in 1:nlevel_cano){
  for(k in 1:(length(lit_data$SNWNOA)-1)){
    nam=paste(names_canopy[i+1],sprintf('%02d',j),sep="")
    ind=which(names(lit_data)==nam)
    data_sta[j+1,k+1]=lit_data[[ind]][2,k+1]
}
}
ind=which(names(lit_data)==names(lit_profile)[i])
data_sta=as.data.frame(data_sta)
nam_temp=paste('V',seq(1,length(lit_data$SNWNOA)-1,by=1),sep="")
names(data_sta)=c('ZZ',nam_temp)
lit_profile[[i]]=rbind(data_sta,lit_data[[ind]][2:numtot,])
}

lit_data$SNWNOA=lit_profile$SNWNOA
lit_data$SNWMASS=lit_profile$SNWMASS
lit_data$SNWRGA=lit_profile$SNWRGA
lit_data$UT=lit_profile$UT

save(lit_data,file='output_R')
