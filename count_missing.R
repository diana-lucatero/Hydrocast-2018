## ------------------------------------- ##
## Counts missing data ----------------- ##
## ------------------------------------- ##

Dir <- 'C:/Users/DianaL/Desktop/Hydrocast/'
library(swfscMisc)
# Load dates info
file <- paste0(Dir,'info_dates.txt')
dates <- t(matrix(scan(file),nrow = 1,ncol = 1086))
## Extra information
no_ens <- 30
name_st <- c('3278')
ist <- 1

## Load observations
name_file <- paste0(Dir,'head/observed/obs_station_',name_st[ist],'.txt')
obs <- t(matrix(scan(name_file),nrow = 50,ncol = 1085))
obs0 <- obs[c(1:1084),2:50] ## Remove dates not modelled
## Load simulated
name_file <- paste0(Dir,'head/forecast/sim1/sim_station_',name_st[ist],'.txt')
sim <- t(matrix(scan(name_file),nrow = 50,ncol = 1085))
sim1 <- sim[,2:50] ## Remove dates not modelled


na.obs <- array(NaN,49)
na.sim <- array(NaN,49)
for (ilead in c(1:dim(obs0)[2])){
  na.obs[ilead] <- na.count(obs0[,ilead])
  na.sim[ilead] <- na.count(sim1[,ilead])
}


#  ----------- SURFACE WATER --------------------
name_st <- c('21126','21127','21129')
ist <- 2
## Load observations
name_file <- paste0(Dir,'discharge/observed/obs_station_',name_st[ist],'.txt')
obs <- t(matrix(scan(name_file),nrow = 49,ncol = 1086))
obs0 <- obs[,2:49] ## Remove dates not modelled
## Load simulated
name_file <- paste0(Dir,'discharge/forecast/sim1/sim_station_',name_st[ist],'.txt')
sim <- t(matrix(scan(name_file),nrow = 49,ncol = 1085))
sim1 <- sim[,2:49] ## Remove dates not modelled

na.obs0 <- array(NaN,48)
na.sim0 <- array(NaN,48)
for (ilead in c(1:dim(obs0)[2])){
  na.obs0[ilead] <- na.count(obs0[,ilead])
  na.sim0[ilead] <- na.count(sim1[,ilead])
}