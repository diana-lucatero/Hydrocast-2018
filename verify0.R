## Compute and plot verification of Q

ptm <- proc.time() # have a look at the time it takes
Dir <- 'D:/Hydrocast/Hydrocast_short/Discharge/'
source(paste0(Dir,'verification/accum.R'))

library(SpecsVerification)
library(hydroGOF)
library(matrixStats)
# Load dates info
file <- paste0(Dir,'info_dates.txt')
dates <- t(matrix(scan(file),nrow = 1,ncol = 1086))
## Load number of rows for 5 events
file <- paste0(Dir,'dates_events_phase.txt')
dat <- t(matrix(scan(file),nrow = 1,ncol = 89))
## More info
no_ens <- 30
lead <- 43
## Stations
name_st <- c('21127','21129','21126')
## Declare vectors of scores
crps0 <- array(NaN,dim = c(length(name_st),lead))
rh0 <- array(NaN,dim = c(length(name_st),lead,(no_ens+1)))
crps1 <- array(NaN,dim = c(length(name_st),lead))
rh1 <- array(NaN,dim = c(length(name_st),lead,(no_ens+1)))
crps2 <- array(NaN,dim = c(length(name_st),lead))
rh2 <- array(NaN,dim = c(length(name_st),lead,(no_ens+1)))
crps3 <- array(NaN,dim = c(length(name_st),lead))
rh3 <- array(NaN,dim = c(length(name_st),lead,(1+1)))
# Moving average
crps0.a <- array(NaN,dim = c(length(name_st),lead))
rh0.a <- array(NaN,dim = c(length(name_st),lead,(no_ens+1)))
crps1.a <- array(NaN,dim = c(length(name_st),lead))
rh1.a <- array(NaN,dim = c(length(name_st),lead,(no_ens+1)))
crps2.a <- array(NaN,dim = c(length(name_st),lead))
rh2.a <- array(NaN,dim = c(length(name_st),lead,(no_ens+1)))
crps3.a <- array(NaN,dim = c(length(name_st),lead))
rh3.a <- array(NaN,dim = c(length(name_st),lead,(1+1)))
for (ist in 1:length(name_st)){#length(name_st)
  ## Load ensembles for DA_Perturbed
  name_file <- paste0(Dir,'forecast/DA_Perturb/ens_station_',name_st[ist],'.txt')
  ens <- t(matrix(scan(name_file),nrow = 50,ncol = no_ens*(1086)))
  ens0 <- ens[,3:50]
  ens0.a <- accum(ens0,2,dim(ens0)[1])
  ## Load ensembles for DA_Observed
  name_file <- paste0(Dir,'forecast/DA_obs/ens_station_',name_st[ist],'.txt')
  ens <- t(matrix(scan(name_file),nrow = 50,ncol = no_ens*(1086)))
  ens1 <- ens[,3:50] 
  ens1.a <- accum(ens1,2,dim(ens1)[1])
  ## Load ensembles for No_Perturbed
  name_file <- paste0(Dir,'forecast/NoPerturb/ens_station_',name_st[ist],'.txt')
  ens <- t(matrix(scan(name_file),nrow = 50,ncol = no_ens*(1086)))
  ens2 <- ens[,3:50] 
  ens2.a <- accum(ens2,2,dim(ens2)[1])
  ## Load No DA
  name_file <- paste0(Dir,'forecast/NoDA/ens_station_',name_st[ist],'.txt')
  ens <- t(matrix(scan(name_file),nrow = 50,ncol = 1086))
  ens3 <- ens[,3:50]
  ens3.a <- accum(ens3,2,dim(ens3)[1])
  ## Load observations
  name_file <- paste0(Dir,'observed/obs_station_',name_st[ist],'.txt')
  obs <- t(matrix(scan(name_file),nrow = 49,ncol = 1086))
  obs0 <- obs[,2:49]
  obs0.a <- accum(obs0,1,dim(obs0)[1])
  ## Start verification
  for (ilead in 1:43){#48
    ## Verification versus observed
    ## DA preturbed
    forecast <- matrix(ens0[,ilead],nrow = 1086,ncol = no_ens,byrow = TRUE)
    crps0[ist,ilead] <- mean(EnsCrps(forecast[dat,],obs0[dat,ilead]),na.rm = TRUE)
    rh0[ist,ilead,] <- Rankhist(forecast[dat,],obs0[dat,ilead])
    ## DA observed
    forecast <- matrix(ens1[,ilead],nrow = 1086,ncol = no_ens,byrow = TRUE)
    crps1[ist,ilead] <- mean(EnsCrps(forecast[dat,],obs0[dat,ilead]),na.rm = TRUE)
    rh1[ist,ilead,] <- Rankhist(forecast[dat,],obs0[dat,ilead])
    ## DA No perturbed
    forecast <- matrix(ens2[,ilead],nrow = 1086,ncol = no_ens,byrow = TRUE)
    crps2[ist,ilead] <- mean(EnsCrps(forecast[dat,],obs0[dat,ilead]),na.rm = TRUE)
    rh2[ist,ilead,] <- Rankhist(forecast[dat,],obs0[dat,ilead])
    ## No DA
    forecast <- matrix(ens3[,ilead],nrow = 1086,ncol = 1,byrow = TRUE)
    crps3[ist,ilead] <- mean(EnsCrps(matrix(forecast[dat,],ncol = 1,nrow = 89),obs0.a[dat,ilead]),na.rm = TRUE)
    rh3[ist,ilead,] <- Rankhist(matrix(forecast[dat,],ncol = 1,nrow = 89),obs0.a[dat,ilead])
    ## Verification versus MOVING AVERAGE
    ## DA preturbed
    forecast <- matrix(ens0.a[,ilead],nrow = 1086,ncol = no_ens,byrow = TRUE)
    crps0.a[ist,ilead] <- mean(EnsCrps(forecast[dat,],obs0.a[dat,ilead]),na.rm = TRUE)
    rh0.a[ist,ilead,] <- Rankhist(forecast[dat,],obs0.a[dat,ilead])
    ## DA observed
    forecast <- matrix(ens1.a[,ilead],nrow = 1086,ncol = no_ens,byrow = TRUE)
    crps1.a[ist,ilead] <- mean(EnsCrps(forecast[dat,],obs0.a[dat,ilead]),na.rm = TRUE)
    rh1.a[ist,ilead,] <- Rankhist(forecast[dat,],obs0.a[dat,ilead])
    ## DA No perturbed
    forecast <- matrix(ens2.a[,ilead],nrow = 1086,ncol = no_ens,byrow = TRUE)
    crps2.a[ist,ilead] <- mean(EnsCrps(forecast[dat,],obs0.a[dat,ilead]),na.rm = TRUE)
    rh2.a[ist,ilead,] <- Rankhist(forecast[dat,],obs0.a[dat,ilead])
    ## No DA
    forecast <- matrix(ens3.a[,ilead],nrow = 1086,ncol = 1,byrow = TRUE)
    crps3.a[ist,ilead] <- mean(EnsCrps(matrix(forecast[dat,],ncol = 1,nrow = 89),obs0.a[dat,ilead]),na.rm = TRUE)
    rh3.a[ist,ilead,] <- Rankhist(matrix(forecast[dat,],ncol = 1,nrow = 89),obs0.a[dat,ilead])
  }# End lead time
  #rm(ens0);rm(ens);rm(obs);rm(obs0)
} # End stations


## Start plotting
## Scores
m <- matrix(c(1:length(name_st)),nrow = length(name_st),ncol = 1,byrow = TRUE)
name_plt = paste0(Dir,'verification3/crps0.png')
png(name_plt,width=750,height=900)
layout(mat = m)
par(mar=c(5,8,4,8))
col0 <- c('black','firebrick3','goldenrod1','forestgreen')
ylim0 <- c(0,0.1)
for (ist in 1:length(name_st)){
  ## Title
  main0 <- paste0('Discharge Station ',name_st[ist])
  ## CRPS
  #ylim0 <- c(min(crps0[ist,]),max(crps0[ist,]))
  plot(crps0[ist,],main = main0,type='o',pch=17,col= col0[1],ylim = ylim0,axes=FALSE, xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
  lines(crps1[ist,],type='o',pch=17,col= col0[2],ylim = ylim0, xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
  lines(crps2[ist,],type='o',pch=17,col= col0[3],ylim = ylim0, xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
  lines(crps3[ist,],type='o',pch=17,col= col0[4],ylim = ylim0, xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
  grid(nx=NULL,ny=NULL,col = 'lightgray',lty = 'dotted',equilogs = TRUE)
  axis(1,at=seq(0,48,4),labels =seq(0,48,4),cex.axis=1.5,las= HORIZONTAL<-1)
  seq0 <- round(seq(ylim0[1],ylim0[2],(ylim0[2]-ylim0[1])/5),digits = 4)
  axis(2,at=seq0,labels = seq0,cex.axis=1.5,las= HORIZONTAL<-1)
  mtext('crps [m3/s]',2,6,cex=1.5)
  mtext('lead time (hours)',1,3,cex=1.3)
  ## Legend
  c0 <- c('DA_PerturbedRainfall','DA_ObservedRainfall','DA_NoPerturbedRainfall','NoDA')
  legend('topleft',legend = c0,col = col0,pch = c(17,17,17,17),cex=1.5)
}
## Rank Histograms DA Perturbed
par(mar=c(1,1,1,1))
for (ist in 1:length(name_st)){
  m <- matrix(c(1:48),nrow = 6,ncol = 8,byrow = TRUE)
  name_plt = paste0(Dir,'verification3/rh_',name_st[ist],'_DAPerturbed0.png')
  png(name_plt,width=750,height=900)
  layout(mat = m)
  for (ilead in 1:lead){
    main0 <- paste0('LT ',ilead,' hrs')
    barplot(rh0[ist,ilead,],main = main0,ylim=c(0,max(rh0[ist,,])),ylab = 'freq',axes = FALSE,yaxt='n')
    #axis(1,at=seq(1,31,1),labels = seq(1,31,1))
    axis(2,at=seq(0,max(rh0[ist,,]),50),labels = seq(0,max(rh0[ist,,]),50),las= HORIZONTAL<-1)
    #abline(h=dim(forecast[dat,])[1]/31,col='red',lty='dotted',lwd=2)
  }
}

## Rank Histograms DA observed
par(mar=c(1,1,1,1))
for (ist in 1:length(name_st)){
  m <- matrix(c(1:48),nrow = 6,ncol = 8,byrow = TRUE)
  name_plt = paste0(Dir,'verification3/rh_',name_st[ist],'_DAObserved0.png')
  png(name_plt,width=750,height=900)
  layout(mat = m)
  for (ilead in 1:lead){
    main0 <- paste0('LT ',ilead,' hrs')
    barplot(rh1[ist,ilead,],main = main0,ylim=c(0,max(rh1[ist,,])),ylab = 'freq',axes = FALSE,yaxt='n')
    #axis(1,at=seq(1,31,1),labels = seq(1,31,1))
    axis(2,at=seq(0,max(rh1[ist,,]),50),labels = seq(0,max(rh1[ist,,]),50),las= HORIZONTAL<-1)
    #abline(h=dim(forecast[dat,])[1]/31,col='red',lty='dotted',lwd=2)
  }
}

## Rank Histograms DA No perturbed
par(mar=c(1,1,1,1))
for (ist in 1:length(name_st)){
  m <- matrix(c(1:48),nrow = 6,ncol = 8,byrow = TRUE)
  name_plt = paste0(Dir,'verification3/rh_',name_st[ist],'_DANoPerturbed0.png')
  png(name_plt,width=750,height=900)
  layout(mat = m)
  for (ilead in 1:lead){
    main0 <- paste0('LT ',ilead,' hrs')
    barplot(rh2[ist,ilead,],main = main0,ylim=c(0,max(rh2[ist,,])),ylab = 'freq',axes = FALSE,yaxt='n')
    #axis(1,at=seq(1,31,1),labels = seq(1,31,1))
    axis(2,at=seq(0,max(rh2[ist,,]),50),labels = seq(0,max(rh2[ist,,]),50),las= HORIZONTAL<-1)
    #abline(h=dim(forecast[dat,])[1]/31,col='red',lty='dotted',lwd=2)
  }
}


## MOVING AVERAGES
## Scores
m <- matrix(c(1:length(name_st)),nrow = length(name_st),ncol = 1,byrow = TRUE)
name_plt = paste0(Dir,'verification3/crps_MA0.png')
png(name_plt,width=750,height=900)
layout(mat = m)
par(mar=c(5,8,4,8))
col0 <- c('black','firebrick3','goldenrod1','forestgreen')
ylim0 <- c(0,0.1)
for (ist in 1:length(name_st)){
  ## Title
  main0 <- paste0('Discharge Station ',name_st[ist])
  ## CRPS
  #ylim0 <- c(min(crps0[ist,]),max(crps0[ist,]))
  plot(crps0.a[ist,],main = main0,type='o',pch=17,col= col0[1],ylim = ylim0,axes=FALSE, xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
  lines(crps1.a[ist,],type='o',pch=17,col= col0[2],ylim = ylim0, xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
  lines(crps2.a[ist,],type='o',pch=17,col= col0[3],ylim = ylim0, xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
  lines(crps3.a[ist,],type='o',pch=17,col= col0[4],ylim = ylim0, xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
  grid(nx=NULL,ny=NULL,col = 'lightgray',lty = 'dotted',equilogs = TRUE)
  axis(1,at=seq(0,48,4),labels =seq(0,48,4),cex.axis=1.5,las= HORIZONTAL<-1)
  seq0 <- round(seq(ylim0[1],ylim0[2],(ylim0[2]-ylim0[1])/5),digits = 4)
  axis(2,at=seq0,labels = seq0,cex.axis=1.5,las= HORIZONTAL<-1)
  mtext('crps [m3/s]',2,6,cex=1.5)
  mtext('lead time (hours)',1,3,cex=1.3)
  ## Legend
  c0 <- c('DA_PerturbedRainfall','DA_ObservedRainfall','DA_NoPerturbedRainfall','NoDA')
  legend('topleft',legend = c0,col = col0,pch = c(17,17,17,17),cex=1.5)
}
## Rank Histograms DA Perturbed
par(mar=c(1,1,1,1))
for (ist in 1:length(name_st)){
  m <- matrix(c(1:48),nrow = 6,ncol = 8,byrow = TRUE)
  name_plt = paste0(Dir,'verification3/rh_',name_st[ist],'_DAPerturbed_MA0.png')
  png(name_plt,width=750,height=900)
  layout(mat = m)
  for (ilead in 1:lead){
    main0 <- paste0('LT ',ilead,' hrs')
    barplot(rh0.a[ist,ilead,],main = main0,ylim=c(0,max(rh0.a[ist,,])),ylab = 'freq',axes = FALSE,yaxt='n')
    #axis(1,at=seq(1,31,1),labels = seq(1,31,1))
    axis(2,at=seq(0,max(rh0.a[ist,,]),50),labels = seq(0,max(rh0.a[ist,,]),50),las= HORIZONTAL<-1)
    #abline(h=dim(forecast[dat,])[1]/31,col='red',lty='dotted',lwd=2)
  }
}

## Rank Histograms DA observed
par(mar=c(1,1,1,1))
for (ist in 1:length(name_st)){
  m <- matrix(c(1:48),nrow = 6,ncol = 8,byrow = TRUE)
  name_plt = paste0(Dir,'verification3/rh_',name_st[ist],'_DAObserved_MA0.png')
  png(name_plt,width=750,height=900)
  layout(mat = m)
  for (ilead in 1:lead){
    main0 <- paste0('LT ',ilead,' hrs')
    barplot(rh1.a[ist,ilead,],main = main0,ylim=c(0,max(rh1.a[ist,,])),ylab = 'freq',axes = FALSE,yaxt='n')
    #axis(1,at=seq(1,31,1),labels = seq(1,31,1))
    axis(2,at=seq(0,max(rh1.a[ist,,]),50),labels = seq(0,max(rh1.a[ist,,]),50),las= HORIZONTAL<-1)
    #abline(h=dim(forecast[dat,])[1]/31,col='red',lty='dotted',lwd=2)
  }
}

## Rank Histograms DA No perturbed
par(mar=c(1,1,1,1))
for (ist in 1:length(name_st)){
  m <- matrix(c(1:48),nrow = 6,ncol = 8,byrow = TRUE)
  name_plt = paste0(Dir,'verification3/rh_',name_st[ist],'_DANoPerturbed_MA0.png')
  png(name_plt,width=750,height=900)
  layout(mat = m)
  for (ilead in 1:lead){
    main0 <- paste0('LT ',ilead,' hrs')
    barplot(rh2.a[ist,ilead,],main = main0,ylim=c(0,max(rh2.a[ist,,])),ylab = 'freq',axes = FALSE,yaxt='n')
    #axis(1,at=seq(1,31,1),labels = seq(1,31,1))
    axis(2,at=seq(0,max(rh2.a[ist,,]),50),labels = seq(0,max(rh2.a[ist,,]),50),las= HORIZONTAL<-1)
    #abline(h=dim(forecast[dat,])[1]/31,col='red',lty='dotted',lwd=2)
  }
}

graphics.off()

proc.time() - ptm 