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
## Extra information
no_ens <- 30
name_st <- c('21126','21127','21129')
ist <- 2

## Load ensembles for DA Perturbed Precipitation
name_file <- paste0(Dir,'discharge/forecast/sim3/ens_station_',name_st[ist],'.txt')
ens <- t(matrix(scan(name_file),nrow = 50,ncol = no_ens*(1086)))
sim3 <- ens[,3:50]
## Load ensembles for DA Observed Precipitation
name_file <- paste0(Dir,'discharge/forecast/sim2/ens_station_',name_st[ist],'.txt')
ens <- t(matrix(scan(name_file),nrow = 50,ncol = no_ens*(1086)))
sim2 <- ens[,3:50]
## Load ensembles for No_Perturbed
name_file <- paste0(Dir,'discharge/forecast/sim4/ens_station_',name_st[ist],'.txt')
ens <- t(matrix(scan(name_file),nrow = 50,ncol = no_ens*(1086)))
sim4 <- ens[,3:50] 
## Load No DA
name_file <- paste0(Dir,'discharge/forecast/sim5/ens_station_',name_st[ist],'.txt')
ens <- t(matrix(scan(name_file),nrow = 50,ncol = 1086))
sim5 <- ens[,3:50]
## Load observations
name_file <- paste0(Dir,'discharge/observed/obs_station_',name_st[ist],'.txt')
obs <- t(matrix(scan(name_file),nrow = 49,ncol = 1086))
obs0 <- obs[,2:49] ## Remove dates not modelled
## Load simulated
name_file <- paste0(Dir,'discharge/forecast/sim1/sim_station_',name_st[ist],'.txt')
sim <- t(matrix(scan(name_file),nrow = 49,ncol = 1085))
sim1 <- sim[,2:49] ## Remove dates not modelled


## ---- Verification ------ ##
verA <- array(NaN,dim = c(48,4))
verB <- array(NaN,dim = c(48,4))
rank.B <- array(NaN,dim = c(1086,48))
verC <- array(NaN,dim = c(48,4))
verD <- array(NaN,dim = c(48,4))
rank.D <- array(NaN,dim = c(1086,48))
verE <- array(NaN,dim = c(48,4))
rank.E <- array(NaN,dim = c(1086,48))
colnames0 <- c('bias','rmse','crps','alpha')
colnames(verB)<-colnames0
colnames(verC)<-colnames0
colnames(verD)<-colnames0
colnames(verE)<-colnames0
colnames(verA)<-colnames0
probs0 <- seq(0,1,0.01)
for (ilead in 1:48){#48
  ## Verification versus observed
  ## DA preturbed -- sim E
  forecast <- matrix(sim3[,ilead],nrow = 1086,ncol = no_ens,byrow = TRUE)
  forecast_mean <- rowMedians(forecast)
  ## Bias [forecast-observation]
  verE[ilead,1] <- mean(forecast_mean,na.rm = T) - mean(obs0[,ilead],na.rm = T)
  verE[ilead,2] <- rmse(forecast_mean,obs0[,ilead],na.rm=T)
  verE[ilead,3] <- mean(EnsCrps(forecast,obs0[,ilead]),na.rm = TRUE)
  ranks <- apply(cbind(obs0[,ilead],forecast), 1, rank, ties.method="last")[1, ]
  rank_m <- (ranks-1)/no_ens
  f <- ecdf(rank_m)
  verE[ilead,4] <- 1-2*mean(abs(f(probs0)-probs0))
  rank.E[,ilead] <- ranks 
  ## DA observed -- sim B
  forecast <- matrix(sim2[,ilead],nrow = 1086,ncol = no_ens,byrow = TRUE)
  forecast_mean <- rowMedians(forecast)
  ## Bias [forecast-observation]
  verB[ilead,1] <- mean(forecast_mean,na.rm = T) - mean(obs0[,ilead],na.rm = T)
  verB[ilead,2] <- rmse(forecast_mean,obs0[,ilead],na.rm=T)
  verB[ilead,3] <- mean(EnsCrps(forecast,obs0[,ilead]),na.rm = TRUE)
  ranks <- apply(cbind(obs0[,ilead],forecast), 1, rank, ties.method="last")[1, ]
  rank_m <- (ranks-1)/no_ens
  f <- ecdf(rank_m)
  verB[ilead,4] <- 1-2*mean(abs(f(probs0)-probs0))
  rank.B[,ilead] <- ranks 
  ## DA No perturbed -- sim D
  forecast <- matrix(sim4[,ilead],nrow = 1086,ncol = no_ens,byrow = TRUE)
  forecast_mean <- rowMedians(forecast)
  ## Bias [forecast-observation]
  verD[ilead,1] <- mean(forecast_mean,na.rm = T) - mean(obs0[,ilead],na.rm = T)
  verD[ilead,2] <- rmse(forecast_mean,obs0[,ilead],na.rm=T)
  verD[ilead,3] <- mean(EnsCrps(forecast,obs0[,ilead]),na.rm = TRUE)
  ranks <- apply(cbind(obs0[,ilead],forecast), 1, rank, ties.method="last")[1, ]
  rank_m <- (ranks-1)/no_ens
  f <- ecdf(rank_m)
  verD[ilead,4] <- 1-2*mean(abs(f(probs0)-probs0))
  rank.D[,ilead] <- ranks 
  ## No DA -- sim C
  forecast <- matrix(sim5[,ilead],nrow = 1086,ncol = 1,byrow = TRUE)
  forecast_mean <- rowMedians(forecast)
  ## Bias [forecast-observation]
  verC[ilead,1] <- mean(forecast_mean,na.rm = T) - mean(obs0[,ilead],na.rm = T)
  verC[ilead,2] <- rmse(forecast_mean,obs0[,ilead],na.rm=T)
  verC[ilead,3] <- mean(EnsCrps(forecast,obs0[,ilead]),na.rm = TRUE)
  ## No DA - No perturbed -- sim A
  forecast <- matrix(sim1[,ilead],nrow = 1085,ncol = 1,byrow = TRUE)
  forecast_mean <- rowMedians(forecast)
  ## Bias [forecast-observation]
  verA[ilead,1] <- mean(forecast_mean,na.rm = T) - mean(obs0[c(1:1085),ilead],na.rm = T)
  verA[ilead,2] <- rmse(forecast_mean,obs0[c(1:1085),ilead],na.rm=T)
  verA[ilead,3] <- mean(EnsCrps(forecast,obs0[c(1:1085),ilead]),na.rm = TRUE)
}# End lead time

file0 <- paste0(Dir,'xin_data/discharge/performance_B.txt')
write.table(verB,file = file0,col.names = TRUE,row.names = FALSE)

file0 <- paste0(Dir,'xin_data/discharge/performance_C.txt')
write.table(verC,file = file0,col.names = TRUE,row.names = FALSE)

file0 <- paste0(Dir,'xin_data/discharge/performance_D.txt')
write.table(verD,file = file0,col.names = TRUE,row.names = FALSE)

file0 <- paste0(Dir,'xin_data/discharge/performance_E.txt')
write.table(verE,file = file0,col.names = TRUE,row.names = FALSE)

file0 <- paste0(Dir,'xin_data/discharge/performance_A.txt')
write.table(verA,file = file0,col.names = TRUE,row.names = FALSE)

file0 <- paste0(Dir,'xin_data/discharge/performance_B_rank.txt')
write.table(rank.B,file = file0,col.names = FALSE,row.names = FALSE)

file0 <- paste0(Dir,'xin_data/discharge/performance_D_rank.txt')
write.table(rank.D,file = file0,col.names = FALSE,row.names = FALSE)

file0 <- paste0(Dir,'xin_data/discharge/performance_E_rank.txt')
write.table(rank.E,file = file0,col.names = FALSE,row.names = FALSE)
