## ------------------------------------------------ ##
## -------- Extracts data for Xin ----------------- ##
## ------------------------------------------------ ##

Dir <- 'C:/Users/DianaL/Desktop/Hydrocast/'
library(matrixStats)
# Load dates info
file <- paste0(Dir,'info_dates.txt')
dates <- t(matrix(scan(file),nrow = 1,ncol = 1086))
## Extra information
no_ens <- 30
name_st <- c('3278')
ist <- 1

## Load ensembles for DA Perturbed Precipitation
name_file <- paste0(Dir,'head/forecast/sim3/ens_station_',name_st[ist],'.txt')
ens <- t(matrix(scan(name_file),nrow = 51,ncol = no_ens*(1086)))
sim3 <- ens[,3:51]
## Load ensembles for DA Observed Precipitation
name_file <- paste0(Dir,'head/forecast/sim2/ens_station_',name_st[ist],'.txt')
ens <- t(matrix(scan(name_file),nrow = 51,ncol = no_ens*(1086)))
sim2 <- ens[,3:51]
## Load ensembles for No_Perturbed
name_file <- paste0(Dir,'head/forecast/sim4/ens_station_',name_st[ist],'.txt')
ens <- t(matrix(scan(name_file),nrow = 51,ncol = no_ens*(1086)))
sim4 <- ens[,3:51] 
## Load No DA
name_file <- paste0(Dir,'head/forecast/sim5/ens_station_',name_st[ist],'.txt')
ens <- t(matrix(scan(name_file),nrow = 51,ncol = 1086))
sim5 <- ens[,3:51]
## Load observations
name_file <- paste0(Dir,'head/observed/obs_station_',name_st[ist],'.txt')
obs <- t(matrix(scan(name_file),nrow = 50,ncol = 1085))
obs0 <- obs[,2:50] ## Remove dates not modelled
## Load simulated
name_file <- paste0(Dir,'head/forecast/sim1/sim_station_',name_st[ist],'.txt')
sim <- t(matrix(scan(name_file),nrow = 50,ncol = 1085))
sim1 <- sim[,2:50] ## Remove dates not modelled

## Load biases
name_file <- paste0(Dir,'head/biases/bias_station_',name_st[ist],'.txt')
bias <- t(matrix(scan(name_file),nrow = 49,ncol = 30*(1084)))
## Remove biases
sim2 <- sim2[1:(30*1084),] + bias
sim3 <- sim3[1:(30*1084),] + bias
sim4 <- sim4[1:(30*1084),] + bias

## Choose date 222 for 2015122606; 636  for 2016040718
ifc <- 632
m = seq(1,dim(sim3)[1],30)
## Read ens + obs for given date
## Compute quantiles
q0 <- c(0,25,50,75,100)
obsp = obs0[ifc,]
simA = sim1[ifc,]
ensB = colQuantiles(sim2[m[ifc]:(m[ifc]+29),],q=q0)
ensC = sim5[ifc,]
ensD = colQuantiles(sim4[m[ifc]:(m[ifc]+29),],q=q0)
ensE = colQuantiles(sim3[m[ifc]:(m[ifc]+29),],q=q0)

deter <- cbind(obsp,simA,ensC)
colnames(deter) <- c('obs','simA','simC')
colnames(ensB)<-c('0%','25%','50%','75%','100%')
colnames(ensD)<-c('0%','25%','50%','75%','100%')
colnames(ensE)<-c('0%','25%','50%','75%','100%')

## Write to ascii files
file0 <- paste0(Dir,'xin_data/head/obs_simA_simC_',dates[ifc],'.txt')
write.table(deter,file = file0,col.names = TRUE,row.names = FALSE)

file0 <- paste0(Dir,'xin_data/head/simB_',dates[ifc],'.txt')
write.table(ensB,file = file0,col.names = TRUE,row.names = FALSE)

file0 <- paste0(Dir,'xin_data/head/simD_',dates[ifc],'.txt')
write.table(ensD,file = file0,col.names = TRUE,row.names = FALSE)

file0 <- paste0(Dir,'xin_data/head/simE_',dates[ifc],'.txt')
write.table(ensE,file = file0,col.names = TRUE,row.names = FALSE)
