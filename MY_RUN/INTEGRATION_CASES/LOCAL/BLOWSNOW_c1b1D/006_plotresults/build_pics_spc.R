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


axis_log <- function(pos,min,max,mark){

axis(pos,log_tick_marks(min,max),FALSE,TRUE,NA,NA)
axis(pos,log_tick_lab(min,max),mark,FALSE,cex.axis=2.)
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
zmass= cat_11_05$SNWMASS$ZZ[2:7]
zflux=rep(0,6)
zdflux=rep(0,6)

zflux[1]=0
zflux[2]=0.3

for(i in 3:6){
zflux[i]=2*zmass[i-1]-zflux[i-1]
}

for(i in 1:5){
zdflux[i]=zflux[i+1]-zflux[i]
}

mass_can=rep(1,6)



mass_can[1]=sum(zdflux[1:5]*cat_11_05$SNWMASS$V8[2:6])
mass_can[2]=sum(zdflux[1:5]*cat_12_05$SNWMASS$V8[2:6])
mass_can[3]=sum(zdflux[1:5]*cat_13_05$SNWMASS$V8[2:6])
mass_can[4]=sum(zdflux[1:5]*cat_14_05$SNWMASS$V8[2:6])
mass_can[5]=sum(zdflux[1:5]*cat_15_05$SNWMASS$V8[2:6])
mass_can[6]=sum(zdflux[1:5]*cat_16_05$SNWMASS$V8[2:6])
xx=99

pdf('multi_K_spc.pdf',height=15,width=10)


par(mfrow=c(6,2))

par(mar=c(0,5,3,0))
ind=1

plot(xx,xx,xlab='',ylab='Hauteur (m)',xaxt='n',yaxt='n',log='y',xlim=c(1e-5,1.1e-4),ylim=c(0.1,100),main='Rayon',cex.main=2,cex.axis=2,cex.lab=2)
abline(h=c(0.1,1,10),v=c(1e-5,3e-5,5e-5,7e-5,9e-5,1.1e-4),lty=2,col='darkgrey')
mask_rad=which(radius[[ind]][1,]>8.5e-5)

for(i in mask_rad){
points(radius[[ind]][,i],levels[[ind]][,i],col='grey40',type='b',lwd=2.5,cex=1.5)
}

lines(cat_11_05$SNWRGA$V8[2:numtot],cat_11_05$SNWRGA$ZZ[2:numtot],col=1,lwd=3,cex=1.5)

exp_txt=expression(paste(U,'= 11 m',s^{-1}))
text(3e-5,0.13,exp_txt,cex=2)

axis(1,at=seq(1e-5,1.1e-4,by=2e-5),labels=FALSE)
axis_log(2,0.1,10,TRUE)

par(mar=c(0,0,3,5))

plot(xx,xx,xlab='',ylab='',xaxt='n',yaxt='n',log='xy',xlim=c(1e-7,1),ylim=c(0.1,100),main='Flux',cex.main=2)
abline(h=c(0.1,1,10),v=c(1e-5,1e-4,1e-3,0.01,0.1,1),lty=2,col='darkgrey')
for(i in 1:(ncol(conc_cat[[ind]])-1)){
points(flux[[ind]][,i],levels[[ind]][,i],col='grey40',type='b',lwd=2.5,cex=1.5)
}

lines(cat_11_05$SNWMASS$V8[2:numtot]*cat_11_05$UT$V8[2:numtot],cat_11_05$SNWMASS$ZZ[2:numtot],col=1,lwd=3,cex=1.5)

axis_log(1,1e-5,0.1,FALSE)
axis_log(2,0.1,10,FALSE)
exp_txt=expression(paste(U,'= 11 m',s^{-1}))
text(8e-5,0.13,exp_txt,cex=2)

mass_can[ind]=sum(zdflux[1:5]*cat_11_05$SNWMASS$V8[2:6])

par(mar=c(0.5,5,1.5,0))
ind=2
plot(xx,xx,xlab='',ylab='Hauteur (m)',xaxt='n',yaxt='n',log='y',xlim=c(1e-5,1.1e-4),ylim=c(0.1,10),cex.axis=2,cex.lab=2)
abline(h=c(0.1,1,10),v=c(1e-5,3e-5,5e-5,7e-5,9e-5,1.1e-4),lty=2,col='darkgrey')
for(i in 1:(ncol(conc_cat[[ind]])-1)){
points(radius[[ind]][,i],levels[[ind]][,i],col='grey40',type='b',lwd=2.5,cex=1.5)
}


lines(cat_12_05$SNWRGA$V8[2:numtot],cat_12_05$SNWRGA$ZZ[2:numtot],col=1,lwd=3,cex=1.5)
axis(1,at=seq(1e-5,1.1e-4,by=2e-5),labels=FALSE)
axis_log(2,0.1,10,TRUE)


exp_txt=expression(paste(U,'= 12 m',s^{-1}))
text(3e-5,0.13,exp_txt,cex=2)


par(mar=c(0.5,0,1.5,5))

plot(xx,xx,xlab='',ylab='',xaxt='n',yaxt='n',log='xy',xlim=c(1e-7,1),ylim=c(0.1,100))
abline(h=c(0.1,1,10),v=c(1e-5,1e-4,1e-3,0.01,0.1,1),lty=2,col='darkgrey')
for(i in 1:(ncol(conc_cat[[ind]])-1)){
points(flux[[ind]][,i],levels[[ind]][,i],col='grey40',type='b',lwd=2.5,cex=1.5)
}

lines(cat_12_05$SNWMASS$V8[2:numtot]*cat_12_05$UT$V8[2:numtot],cat_12_05$SNWMASS$ZZ[2:numtot],col=1,lwd=3,cex=1.5)
axis_log(1,1e-5,0.1,FALSE)
axis_log(2,0.1,10,FALSE)

exp_txt=expression(paste(U,'= 12 m',s^{-1}))
text(8e-5,0.13,exp_txt,cex=2)

par(mar=c(0.5,5,1.5,0))
ind=3

plot(xx,xx,xlab='',ylab='Hauteur (m)',xaxt='n',yaxt='n',log='y',xlim=c(1e-5,1.1e-4),ylim=c(0.1,100),cex.axis=2,cex.lab=2)
abline(h=c(0.1,1,10),v=c(1e-5,3e-5,5e-5,7e-5,9e-5,1.1e-4),lty=2,col='darkgrey')
for(i in 1:(ncol(conc_cat[[ind]])-1)){
points(radius[[ind]][,i],levels[[ind]][,i],col='grey40',type='b',lwd=2.5,cex=1.5)
}


lines(cat_13_05$SNWRGA$V8[2:numtot],cat_13_05$SNWRGA$ZZ[2:numtot],col=1,lwd=3,cex=1.5)

legend('topright',legend=c('Observations'),lty=1,pch=1,col='grey40',cex=1.8,inset=0.01,lwd=2.5)

axis(1,at=seq(1e-5,1.1e-4,by=2e-5),labels=FALSE)
axis_log(2,0.1,10,TRUE)

exp_txt=expression(paste(U,'= 13 m',s^{-1}))
text(3e-5,0.13,exp_txt,cex=2)


par(mar=c(0.5,0,1.5,5))

plot(xx,xx,xlab='',ylab='',xaxt='n',yaxt='n',log='xy',xlim=c(1e-7,1),ylim=c(0.1,100))
abline(h=c(0.1,1,10),v=c(1e-5,1e-4,1e-3,0.01,0.1,1),lty=2,col='darkgrey')
for(i in 1:(ncol(conc_cat[[ind]])-1)){
points(flux[[ind]][,i],levels[[ind]][,i],col='grey40',type='b',lwd=2.5,cex=1.5)
}

lines(cat_13_05$SNWMASS$V8[2:numtot]*cat_13_05$UT$V8[2:numtot],cat_13_05$SNWMASS$ZZ[2:numtot],col=1,lwd=3,cex=1.5)

legend('topright',legend=c('Observations'),lty=1,pch=1,col='grey40',cex=1.8,inset=0.01,lwd=2.5)

axis_log(1,1e-5,0.1,FALSE)
axis_log(2,0.1,10,FALSE)
exp_txt=expression(paste(U,'= 13 m',s^{-1}))
text(8e-5,0.13,exp_txt,cex=2)
par(mar=c(0.5,5,1.5,0))
ind=4

plot(xx,xx,xlab='',ylab='Hauteur (m)',xaxt='n',yaxt='n',log='y',xlim=c(1e-5,1.1e-4),ylim=c(0.1,10),cex.axis=2,cex.lab=2)
abline(h=c(0.1,1,10),v=c(1e-5,3e-5,5e-5,7e-5,9e-5,1.1e-4),lty=2,col='darkgrey')
for(i in 1:(ncol(conc_cat[[ind]])-1)){
points(radius[[ind]][,i],levels[[ind]][,i],col='grey40',type='b',lwd=2.5,cex=1.5)
}


lines(cat_14_05$SNWRGA$V8[2:numtot],cat_14_05$SNWRGA$ZZ[2:numtot],col=1,lwd=3,cex=1.5)

axis(1,at=seq(1e-5,1.1e-4,by=2e-5),labels=FALSE)
axis_log(2,0.1,10,TRUE)

exp_txt=expression(paste(U,'= 14 m',s^{-1}))
text(3e-5,0.13,exp_txt,cex=2)


par(mar=c(0.5,0,1.5,5))

plot(xx,xx,xlab='',ylab='',xaxt='n',yaxt='n',log='xy',xlim=c(1e-7,1),ylim=c(0.1,100))
abline(h=c(0.1,1,10),v=c(1e-5,1e-4,1e-3,0.01,0.1,1),lty=2,col='darkgrey')
for(i in 1:(ncol(conc_cat[[ind]])-1)){
points(flux[[ind]][,i],levels[[ind]][,i],col='grey40',type='b',lwd=2.5,cex=1.5)
}

lines(cat_14_05$SNWMASS$V8[2:numtot]*cat_14_05$UT$V8[2:numtot],cat_14_05$SNWMASS$ZZ[2:numtot],col=1,lwd=3,cex=1.5)

axis_log(1,1e-5,0.1,FALSE)
axis_log(2,0.1,10,FALSE)
exp_txt=expression(paste(U,'= 14 m',s^{-1}))
text(8e-5,0.13,exp_txt,cex=2)
par(mar=c(0.5,5,1.5,0))
ind=5

plot(xx,xx,xlab='',ylab='Hauteur (m)',xaxt='n',yaxt='n',log='y',xlim=c(1e-5,1.1e-4),ylim=c(0.1,10),cex.axis=2,cex.lab=2)
abline(h=c(0.1,1,10),v=c(1e-5,3e-5,5e-5,7e-5,9e-5,1.1e-4),lty=2,col='darkgrey')
for(i in 1:(ncol(conc_cat[[ind]])-1)){
points(radius[[ind]][,i],levels[[ind]][,i],col='grey40',type='b',lwd=2.5,cex=1.5)
}


lines(cat_15_05$SNWRGA$V8[2:numtot],cat_15_05$SNWRGA$ZZ[2:numtot],col=1,lwd=3,cex=1.5)

axis(1,at=seq(1e-5,1.1e-4,by=2e-5),labels=FALSE)
axis_log(2,0.1,10,TRUE)

exp_txt=expression(paste(U,'= 15 m',s^{-1}))
text(3e-5,0.13,exp_txt,cex=2)


par(mar=c(0.5,0,1.5,5))

plot(xx,xx,xlab='',ylab='',xaxt='n',yaxt='n',log='xy',xlim=c(1e-7,1),ylim=c(0.1,100))
abline(h=c(0.1,1,10),v=c(1e-5,1e-4,1e-3,0.01,0.1,1),lty=2,col='darkgrey')
for(i in 1:(ncol(conc_cat[[ind]])-1)){
points(flux[[ind]][,i],levels[[ind]][,i],col='grey40',type='b',lwd=2.5,cex=1.5)
}

lines(cat_15_05$SNWMASS$V8[2:numtot]*cat_15_05$UT$V8[2:numtot],cat_15_05$SNWMASS$ZZ[2:numtot],col=1,lwd=3,cex=1.5)


axis_log(1,1e-5,0.1,FALSE)
axis_log(2,0.1,10,FALSE)

exp_txt=expression(paste(U,'= 15 m',s^{-1}))
text(8e-5,0.13,exp_txt,cex=2)

par(mar=c(4.5,5,1.5,0))
ind=6

plot(xx,xx,xlab='Rayon (m)',ylab='Hauteur (m)',xaxt='n',yaxt='n',log='y',xlim=c(1e-5,1.1e-4),ylim=c(0.1,10),cex.axis=2,cex.lab=2)
abline(h=c(0.1,1,10),v=c(1e-5,3e-5,5e-5,7e-5,9e-5,1.1e-4),lty=2,col='darkgrey')
for(i in 1:(ncol(conc_cat[[ind]])-1)){
points(radius[[ind]][,i],levels[[ind]][,i],col='grey40',type='b',lwd=2.5,cex=1.5)
}


lines(cat_16_05$SNWRGA$V8[2:numtot],cat_16_05$SNWRGA$ZZ[2:numtot],col=1,lwd=3,cex=1.5)


axis(1,at=seq(1e-5,1.1e-4,by=2e-5),labels=c(1e-5,3e-5,5e-5,7e-5,9e-5,1.1e-4),cex.axis=2)
axis_log(2,0.1,10,TRUE)

exp_txt=expression(paste(U,'= 16 m',s^{-1}))
text(3e-5,0.13,exp_txt,cex=2)


par(mar=c(4.5,0,1.5,5))
x_lab=expression(paste('Flux (kg ',m^{-2},' ',s^{-1},')',sep=''))
plot(xx,xx,xlab=x_lab,ylab='',xaxt='n',yaxt='n',log='xy',xlim=c(1e-7,1),ylim=c(0.1,100),cex.axis=2,cex.lab=2)
abline(h=c(0.1,1,10),v=c(1e-5,1e-4,1e-3,0.01,0.1,1),lty=2,col='darkgrey')
for(i in 1:(ncol(conc_cat[[ind]])-1)){
points(flux[[ind]][,i],levels[[ind]][,i],col='grey40',type='b',lwd=2.5,cex=1.5)
}

lines(cat_16_05$SNWMASS$V8[2:numtot]*cat_16_05$UT$V8[2:numtot],cat_16_05$SNWMASS$ZZ[2:numtot],col=1,lwd=3,cex=1.5)

axis_log(1,1e-5,0.1,TRUE)
axis_log(2,0.1,10,FALSE)
exp_txt=expression(paste(U,'= 16 m',s^{-1}))
text(8e-5,0.13,exp_txt,cex=2)
dev.off()

