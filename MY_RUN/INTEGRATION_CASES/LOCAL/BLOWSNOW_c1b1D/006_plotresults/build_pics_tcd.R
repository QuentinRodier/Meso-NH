log_tick_marks <- function(min,max)
{
    nsplit <- abs(round(log10(max)-log10(min)))
    i <- 0
    nurange <- c()
    while(i<=nsplit) {
        nurange <- c(nurange,sapply(1:10,function(x) x*(10^i)))
        i <- i+1;
    }
    nurange*min
}

log_tick_lab <- function(min,max)
{
    nsplit <- abs(round(log10(max)-log10(min)))
    i <- 0
    nurange <- c()
    while(i<=nsplit) {
        nurange <- c(nurange,sapply(1,function(x) x*(10^i)))
        i <- i+1;
    }
    nurange*min
}


axis_log <- function(pos,min,max,mark,ll){

axis(pos,log_tick_marks(min,max),FALSE,TRUE,NA,NA)
axis(pos,log_tick_lab(min,max),mark,FALSE,cex.axis=2.,las=ll)
}



load('res_R_11')
cat_11_05=lit_data
load('res_R_12')
cat_12_05=lit_data
load('res_R_13')
cat_13_05=lit_data
load('res_R_14')
cat_14_05=lit_data
load('res_R_15')
cat_15_05=lit_data
load('res_R_16')
cat_16_05=lit_data


numtot=nrow(cat_11_05$UT)

#Load Observations
load('conc_cat_spc_R')

flux=list(matrix(0,nrow=6,ncol=1))
for(i in 1:length(conc_cat)){
    flux[i]<- list(matrix(0,nrow=3,ncol=1))
}

radius=list(matrix(0,nrow=6,ncol=1))
for(i in 1:length(conc_cat)){
    radius[i]<- list(matrix(0,nrow=3,ncol=1))
}

levels=list(matrix(0,nrow=6,ncol=1))
for(i in 1:length(conc_cat)){
    levels[i]<- list(matrix(0,nrow=3,ncol=1))
}

for(i in 1:length(conc_cat)){
flux[[i]]=conc_cat[[i]][1:3,2:ncol(conc_cat[[i]])]/1000
radius[[i]]=conc_cat[[i]][4:6,2:ncol(conc_cat[[i]])]*1e-6
levels[[i]]=conc_cat[[i]][7:9,2:ncol(conc_cat[[i]])]

}


# Canopy grid 
zmass= cat_11_05$SNWMASS$ZZ[2:numtot]
zflux=rep(0,length(zmass))
zdflux=rep(0,length(zmass))

zflux[1]=0
zflux[2]=0.3

for(i in 3:6){
zflux[i]=2*zmass[i-1]-zflux[i-1]
}
for(i in 7:length(zmass)){
zflux[i]=0.5*(zmass[i]+zmass[i-1])
}


for(i in 1:(length(zmass)-1)){
zdflux[i]=zflux[i+1]-zflux[i]
}

mass_can=rep(1,6)
mass_can[1]=sum(zdflux[1:5]*cat_11_05$SNWMASS$V8[2:6])
mass_can[2]=sum(zdflux[1:5]*cat_12_05$SNWMASS$V8[2:6])
mass_can[3]=sum(zdflux[1:5]*cat_13_05$SNWMASS$V8[2:6])
mass_can[4]=sum(zdflux[1:5]*cat_14_05$SNWMASS$V8[2:6])
mass_can[5]=sum(zdflux[1:5]*cat_15_05$SNWMASS$V8[2:6])
mass_can[6]=sum(zdflux[1:5]*cat_16_05$SNWMASS$V8[2:6])



flux_can=rep(1,6)
flux_can[1]=sum(zdflux[1:(length(zmass)-1)]*cat_11_05$SNWMASS$V8[2:(numtot-1)]*cat_11_05$UT$V8[2:(numtot-1)])
flux_can[2]=sum(zdflux[1:(length(zmass)-1)]*cat_12_05$SNWMASS$V8[2:(numtot-1)]*cat_12_05$UT$V8[2:(numtot-1)])
flux_can[3]=sum(zdflux[1:(length(zmass)-1)]*cat_13_05$SNWMASS$V8[2:(numtot-1)]*cat_13_05$UT$V8[2:(numtot-1)])
flux_can[4]=sum(zdflux[1:(length(zmass)-1)]*cat_14_05$SNWMASS$V8[2:(numtot-1)]*cat_14_05$UT$V8[2:(numtot-1)])
flux_can[5]=sum(zdflux[1:(length(zmass)-1)]*cat_15_05$SNWMASS$V8[2:(numtot-1)]*cat_15_05$UT$V8[2:(numtot-1)])
flux_can[6]=sum(zdflux[1:(length(zmass)-1)]*cat_16_05$SNWMASS$V8[2:(numtot-1)]*cat_16_05$UT$V8[2:(numtot-1)])

xx=99

pdf('fig_tcd.pdf',height=10,width=11)


par(mfrow=c(3,2))

par(mar=c(0,7.5,3,0))
ind=2

plot(xx,xx,xlab='',ylab='',xaxt='n',yaxt='n',log='y',xlim=c(1e-5,1.1e-4),ylim=c(0.1,100),main='Mean radius',cex.main=2.5,cex.axis=2,cex.lab=2.5)
abline(h=c(0.1,1,10,100),v=c(1e-5,3e-5,5e-5,7e-5,9e-5,1.1e-4),lty=2,col='darkgrey')
mask_rad=which(radius[[ind]][1,]>8.5e-5)


#for(i in mask_rad){
for(i in 1:(ncol(conc_cat[[ind]])-1)){
points(radius[[ind]][,i],levels[[ind]][,i],col='grey40',type='b',lwd=2.5,cex=2,pch=3)
}
#cat_12_05$SNWRGA$ZZ[7]=4

lines(cat_12_05$SNWRGA$V8[2:numtot],cat_12_05$SNWRGA$ZZ[2:numtot],col=1,lwd=3,cex=1.5,type='b',pch=15)
#lines(cat_12_1$SNWRGA$V8[2:numtot],cat_12_1$SNWRGA$ZZ[2:numtot],col=4,lwd=3,cex=1.5)

legend('topright',legend=c('LR','HR','LRC','Obs.'),lty=c(1,1,1,1),lwd=c(3,3,3,3),pch=c(18,17,15,3),col=c('green','red','black','grey40'),ncol=2,inset=0.02,bg='white',cex=1.9)
exp_txt=expression(paste(U,'= 12 m',s^{-1}))
#text(3e-5,0.13,exp_txt,cex=2.5,font=4)

legend('bottomleft',legend=c(exp_txt),bg='white',inset=0.02,cex=2) 
mtext('Height (m)',side=2,line=5,cex=1.8)
axis(1,at=seq(1e-5,1.1e-4,by=2e-5),labels=FALSE)
axis_log(2,0.1,100,TRUE,2)

par(mar=c(0,0,3,7.5))

plot(xx,xx,xlab='',ylab='',xaxt='n',yaxt='n',log='xy',xlim=c(1e-7,1),ylim=c(0.1,100),main='Flux',cex.main=2.5)
abline(h=c(0.1,1,10,100),v=c(1e-7,1e-6,1e-5,1e-4,1e-3,0.01,0.1,1),lty=2,col='darkgrey')
for(i in 1:(ncol(conc_cat[[ind]])-1)){
points(flux[[ind]][,i],levels[[ind]][,i],col='grey40',type='b',lwd=2.5,cex=2,pch=3)
}

lines(cat_12_05$SNWMASS$V8[2:numtot]*cat_12_05$UT$V8[2:numtot],cat_12_05$SNWMASS$ZZ[2:numtot],col=1,lwd=3,cex=1.5,type='b',pch=15)
#lines(cat_12_1$SNWMASS$V8[2:numtot]*cat_12_1$UT$V8[2:numtot],cat_12_1$SNWMASS$ZZ[2:numtot],col=4,lwd=3,cex=1.5)

axis_log(1,1e-7,0.1,FALSE,1)
axis_log(2,0.1,100,FALSE,2)
exp_txt=expression(paste(U,'= 12 m',s^{-1}))
#text(8e-7,0.13,exp_txt,cex=2.5)
legend('bottomleft',legend=c(exp_txt),bg='white',inset=0.02,cex=2) 
#q1=expression(paste(Q[HR],'= xx kg',m^{-1},s^{-1}))
#legend('topright',legend=c(q1,q1),bg='white',inset=0.02,cex=1.8)

par(mar=c(0.5,7.5,1.5,0))
ind=4

plot(xx,xx,xlab='',ylab='',xaxt='n',yaxt='n',log='y',xlim=c(1e-5,1.1e-4),ylim=c(0.1,100),cex.axis=2,cex.lab=2.5)
abline(h=c(0.1,1,10,100),v=c(1e-5,3e-5,5e-5,7e-5,9e-5,1.1e-4),lty=2,col='darkgrey')
for(i in 1:(ncol(conc_cat[[ind]])-1)){
points(radius[[ind]][,i],levels[[ind]][,i],col='grey40',type='b',lwd=2.5,cex=2,pch=3)
}


lines(cat_14_05$SNWRGA$V8[2:numtot],cat_14_05$SNWRGA$ZZ[2:numtot],col=1,lwd=3,cex=1.5,type='b',pch=15)
#lines(cat_14_1$SNWRGA$V8[2:numtot],cat_14_1$SNWRGA$ZZ[2:numtot],col=4,lwd=3,cex=1.5)



axis(1,at=seq(1e-5,1.1e-4,by=2e-5),labels=FALSE)
axis_log(2,0.1,100,TRUE,2)

legend('topright',legend=c('LR','HR','LRC','Obs.'),lty=c(1,1,1,1),lwd=c(3,3,3,3),pch=c(18,17,15,3),col=c('green','red','black','grey40'),ncol=2,inset=0.02,bg='white',cex=1.9)
mtext('Height (m)',side=2,line=5,cex=1.8)
exp_txt=expression(paste(U,'= 14 m',s^{-1}))
#text(3e-5,0.13,exp_txt,cex=2.5)

legend('bottomleft',legend=c(exp_txt),bg='white',inset=0.02,cex=2) 

par(mar=c(0.5,0,1.5,7.5))

plot(xx,xx,xlab='',ylab='',xaxt='n',yaxt='n',log='xy',xlim=c(1e-7,1),ylim=c(0.1,100))
abline(h=c(0.1,1,10,100),v=c(1e-7,1e-6,1e-5,1e-4,1e-3,0.01,0.1,1),lty=2,col='darkgrey')
for(i in 1:(ncol(conc_cat[[ind]])-1)){
points(flux[[ind]][,i],levels[[ind]][,i],col='grey40',type='b',lwd=2.5,cex=2,pch=3)
}

lines(cat_14_05$SNWMASS$V8[2:numtot]*cat_14_05$UT$V8[2:numtot],cat_14_05$SNWMASS$ZZ[2:numtot],col=1,lwd=3,cex=1.5,type='b',pch=15)
#lines(cat_14_1$SNWMASS$V8[2:numtot]*cat_14_1$UT$V8[2:numtot],cat_14_1$SNWMASS$ZZ[2:numtot],col=4,lwd=3,cex=1.5)

axis_log(1,1e-7,0.1,FALSE,1)
axis_log(2,0.1,100,FALSE,1)
exp_txt=expression(paste(U,'= 14 m',s^{-1}))
#text(8e-7,0.13,exp_txt,cex=2.5)

legend('bottomleft',legend=c(exp_txt),bg='white',inset=0.02,cex=2) 
par(mar=c(5,7.5,1.5,0))
ind=6

plot(xx,xx,xlab='',ylab='',xaxt='n',yaxt='n',log='y',xlim=c(1e-5,1.1e-4),ylim=c(0.1,100),cex.axis=2,cex.lab=2.5)
abline(h=c(0.1,1,10,100),v=c(1e-5,3e-5,5e-5,7e-5,9e-5,1.1e-4),lty=2,col='darkgrey')
for(i in 1:(ncol(conc_cat[[ind]])-1)){
points(radius[[ind]][,i],levels[[ind]][,i],col='grey40',type='b',lwd=2.5,cex=2,pch=3)
}


lines(cat_16_05$SNWRGA$V8[2:numtot],cat_16_05$SNWRGA$ZZ[2:numtot],col=1,lwd=3,cex=1.5,type='b',pch=15)
#lines(cat_16_1$SNWRGA$V8[2:numtot],cat_16_1$SNWRGA$ZZ[2:numtot],col=4,lwd=3,cex=1.5)


axis(1,at=seq(1e-5,1.1e-4,by=2e-5),labels=c(1e-5,3e-5,5e-5,7e-5,9e-5,1.1e-4),cex.axis=2)
axis_log(2,0.1,100,TRUE,2)

exp_txt=expression(paste(U,'= 16 m',s^{-1}))
#text(3e-5,0.13,exp_txt,cex=2.5)
legend('bottomleft',legend=c(exp_txt),bg='white',inset=0.02,cex=2) 
legend('topright',legend=c('LR','HR','LRC','Obs.'),lty=c(1,1,1,1),lwd=c(3,3,3,3),pch=c(18,17,15,3),col=c('green','red','black','grey40'),ncol=2,inset=0.02,bg='white',cex=1.8)
mtext('Height (m)',side=2,line=5,cex=1.8)
mtext('Radius (m)',side=1,line=3,cex=1.7)
par(mar=c(5,0,1.5,7.5))
x_lab=expression(paste('Flux (kg ',m^{-2},' ',s^{-1},')',sep=''))
plot(xx,xx,xlab='',ylab='',xaxt='n',yaxt='n',log='xy',xlim=c(1e-7,1),ylim=c(0.1,100),cex.axis=2,cex.lab=2.5)
abline(h=c(0.1,1,10,100),v=c(1e-7,1e-6,1e-5,1e-4,1e-3,0.01,0.1,1),lty=2,col='darkgrey')
for(i in 1:(ncol(conc_cat[[ind]])-1)){
points(flux[[ind]][,i],levels[[ind]][,i],col='grey40',type='b',lwd=2.5,cex=2,pch=3)
}

lines(cat_16_05$SNWMASS$V8[2:numtot]*cat_16_05$UT$V8[2:numtot],cat_16_05$SNWMASS$ZZ[2:numtot],col=1,lwd=3,cex=1.5,type='b',pch=15)
#lines(cat_16_1$SNWMASS$V8[2:numtot]*cat_16_1$UT$V8[2:numtot],cat_16_1$SNWMASS$ZZ[2:numtot],col=4,lwd=3,cex=1.5)

axis_log(1,1e-7,0.1,TRUE,1)
axis_log(2,0.1,100,FALSE,1)
exp_txt=expression(paste(U,'= 16 m',s^{-1}))
#text(8e-7,0.13,exp_txt,cex=2.5)
mtext(x_lab,side=1,line=4,cex=1.7)
legend('bottomleft',legend=c(exp_txt),bg='white',inset=0.02,cex=2) 
dev.off()

