## ------------------------------------------------ ##
## -------- Extracts data for Xin ----------------- ##
## ------------------------------------------------ ##

Dir <- 'C:/Users/DianaL/Desktop/Hydrocast/'
library(matrixStats)
library(SpecsVerification)
library(hydroGOF)
# Load dates info
file <- paste0(Dir,'info_dates.txt')
dates <- t(matrix(scan(file),nrow = 1,ncol = 1086))
## Load number of rows for 5 events
file <- paste0(Dir,'xin_data/dates_events_phase.txt')
dat <- t(matrix(scan(file),nrow = 1,ncol = 89))
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
obs0 <- obs[c(1:1084),2:50] ## Remove dates not modelled
## Load simulated
name_file <- paste0(Dir,'head/forecast/sim1/sim_station_',name_st[ist],'.txt')
sim <- t(matrix(scan(name_file),nrow = 50,ncol = 1085))
sim1 <- sim[,2:50] ## Remove dates not modelled

## Load biases
name_file <- paste0(Dir,'head/biases/bias_station_',name_st[ist],'.txt')
bias <- t(matrix(scan(name_file),nrow = 49,ncol = 30*(1084)))
# ## Remove biases
sim2 <- sim2[1:(30*1084),] + bias
sim3 <- sim3[1:(30*1084),] + bias
sim4 <- sim4[1:(30*1084),] + bias

## ---- Verification ------ ##
verA <- array(NaN,dim = c(49,4))
verB <- array(NaN,dim = c(49,4))
rank.B <- array(NaN,dim = c(length(dat),49))
verC <- array(NaN,dim = c(49,4))
verD <- array(NaN,dim = c(49,4))
rank.D <- array(NaN,dim = c(length(dat),49))
verE <- array(NaN,dim = c(49,4))
rank.E <- array(NaN,dim = c(length(dat),49))
colnames0 <- c('bias','rmse','crps','alpha')
colnames(verB)<-colnames0
colnames(verC)<-colnames0
colnames(verD)<-colnames0
colnames(verE)<-colnames0
colnames(verA)<-colnames0
probs0 <- seq(0,1,0.01)
for (ilead in 1:49){#48
  ## Verification versus observed
  ## DA preturbed -- sim E
  forecast <- matrix(sim3[,ilead],nrow = 1084,ncol = no_ens,byrow = TRUE)
  forecast_mean <- rowMedians(forecast)
  ## Bias [forecast-observation]
  verE[ilead,1] <- mean(forecast_mean[dat],na.rm = T) - mean(obs0[dat,ilead],na.rm = T)
  verE[ilead,2] <- rmse(forecast_mean[dat],obs0[dat,ilead],na.rm=T)
  verE[ilead,3] <- mean(EnsCrps(forecast[dat,],obs0[dat,ilead]),na.rm = TRUE)
  ranks <- apply(cbind(obs0[dat,ilead],forecast[dat,]), 1, rank, ties.method="last")[1, ]
  rank_m <- (ranks-1)/no_ens
  f <- ecdf(rank_m)
  verE[ilead,4] <- 1-2*mean(abs(f(probs0)-probs0))
  rank.E[,ilead] <- ranks 
  ## DA observed -- sim B
  forecast <- matrix(sim2[,ilead],nrow = 1084,ncol = no_ens,byrow = TRUE)
  forecast_mean <- rowMedians(forecast)
  ## Bias [forecast-observation]
  verB[ilead,1] <- mean(forecast_mean[dat],na.rm = T) - mean(obs0[dat,ilead],na.rm = T)
  verB[ilead,2] <- rmse(forecast_mean[dat],obs0[dat,ilead],na.rm=T)
  verB[ilead,3] <- mean(EnsCrps(forecast[dat,],obs0[dat,ilead]),na.rm = TRUE)
  ranks <- apply(cbind(obs0[dat,ilead],forecast[dat,]), 1, rank, ties.method="last")[1, ]
  rank_m <- (ranks-1)/no_ens
  f <- ecdf(rank_m)
  verB[ilead,4] <- 1-2*mean(abs(f(probs0)-probs0))
  rank.B[,ilead] <- ranks 
  ## DA No perturbed -- sim D
  forecast <- matrix(sim4[,ilead],nrow = 1084,ncol = no_ens,byrow = TRUE)
  forecast_mean <- rowMedians(forecast)
  ## Bias [forecast-observation]
  verD[ilead,1] <- mean(forecast_mean[dat],na.rm = T) - mean(obs0[dat,ilead],na.rm = T)
  verD[ilead,2] <- rmse(forecast_mean[dat],obs0[dat,ilead],na.rm=T)
  verD[ilead,3] <- mean(EnsCrps(forecast[dat,],obs0[dat,ilead]),na.rm = TRUE)
  ranks <- apply(cbind(obs0[dat,ilead],forecast[dat,]), 1, rank, ties.method="last")[1, ]
  rank_m <- (ranks-1)/no_ens
  f <- ecdf(rank_m)
  verD[ilead,4] <- 1-2*mean(abs(f(probs0)-probs0))
  rank.D[,ilead] <- ranks 
  ## No DA -- sim C
  forecast <- matrix(sim5[c(1:1084),ilead],nrow = 1084,ncol = 1,byrow = TRUE)
  forecast_mean <- rowMedians(forecast)
  ## Bias [forecast-observation]
  verC[ilead,1] <- mean(forecast_mean[dat],na.rm = T) - mean(obs0[dat,ilead],na.rm = T)
  verC[ilead,2] <- rmse(forecast_mean[dat],obs0[dat,ilead],na.rm=T)
  verC[ilead,3] <- mean(EnsCrps(as.matrix(forecast[dat,]),as.matrix(obs0[dat,ilead])),na.rm = TRUE)
  ## No DA No forecast -- sim A
  forecast <- matrix(sim1[c(1:1084),ilead],nrow = 1084,ncol = 1,byrow = TRUE)
  forecast_mean <- rowMedians(forecast)
  ## Bias [forecast-observation]
  verA[ilead,1] <- mean(forecast_mean[dat],na.rm = T) - mean(obs0[dat,ilead],na.rm = T)
  verA[ilead,2] <- rmse(forecast_mean[dat],obs0[dat,ilead],na.rm=T)
  verA[ilead,3] <- mean(EnsCrps(as.matrix(forecast[dat,]),as.matrix(obs0[dat,ilead])),na.rm = TRUE)
}# End lead time

file0 <- paste0(Dir,'xin_data/head/events/performance_B.txt')
write.table(verB,file = file0,col.names = TRUE,row.names = FALSE)

file0 <- paste0(Dir,'xin_data/head/events/performance_C.txt')
write.table(verC,file = file0,col.names = TRUE,row.names = FALSE)

file0 <- paste0(Dir,'xin_data/head/events/performance_D.txt')
write.table(verD,file = file0,col.names = TRUE,row.names = FALSE)

file0 <- paste0(Dir,'xin_data/head/events/performance_E.txt')
write.table(verE,file = file0,col.names = TRUE,row.names = FALSE)

file0 <- paste0(Dir,'xin_data/head/events/performance_A.txt')
write.table(verA,file = file0,col.names = TRUE,row.names = FALSE)

file0 <- paste0(Dir,'xin_data/head/events/performance_B_rank.txt')
write.table(rank.B,file = file0,col.names = FALSE,row.names = FALSE)

file0 <- paste0(Dir,'xin_data/head/events/performance_D_rank.txt')
write.table(rank.D,file = file0,col.names = FALSE,row.names = FALSE)

file0 <- paste0(Dir,'xin_data/head/events/performance_E_rank.txt')
write.table(rank.E,file = file0,col.names = FALSE,row.names = FALSE)
