## -------------------------------------- ##
## ----- Makes plots of Paper 4 --------- ##
## ----- Diana Lucatero 28/02/2018 ------ ##
## -------------------------------------- ##

Dir <- 'C:/Users/DianaL/Desktop/Hydrocast/xin_data/'

## Load verification metrics for surface water
name_file <- paste0(Dir,'discharge/events/performance_B_rank.txt')
dataB.rank <- read.table(name_file)
name_file <- paste0(Dir,'discharge/events/performance_D_rank.txt')
dataD.rank <- read.table(name_file)
name_file <- paste0(Dir,'discharge/events/performance_E_rank.txt')
dataE.rank <- read.table(name_file)

## Load verification metrics for subsurface water
name_file <- paste0(Dir,'head/events/performance_B_rank.txt')
dataB.h.rank <- read.table(name_file)
name_file <- paste0(Dir,'head/events/performance_D_rank.txt')
dataD.h.rank <- read.table(name_file)
name_file <- paste0(Dir,'head/events/performance_E_rank.txt')
dataE.h.rank <- read.table(name_file)

## ---------- PLOT ------------------- ##
ilead <- 24
m <- matrix(c(1:6),nrow = 2,ncol = 3,byrow = TRUE)
name_plt = paste0(Dir,'rh_lead',ilead,'_0.png')
png(name_plt,width=750,height=750)
layout(m)
par(mar=c(5,6,4,2))
cex0 <- 2
cex1 <- 3
cex2 <- 1.5

ylim0 <- c(0,50)
seq0 <- seq(0,50,10)

main0 <- paste0('LT ',ilead,' hrs')

hist(dataB.rank[,ilead],breaks = seq(1,31),main = 'simB',ylim = ylim0,
     ylab = 'freq',axes = FALSE,yaxt='n',xlab = 'Rank',cex.lab=cex0,cex.main=cex1)
abline(h=dim(dataB.rank)[1]/31,col='red',lty='dotted',lwd=2)
axis(1,at=seq(1,31,1),labels = seq(1,31,1),cex.axis=cex2)
axis(2,at=seq0,labels = seq0,las= HORIZONTAL<-1,cex.axis=cex2)

hist(dataD.rank[,ilead],breaks = seq(1,31),main = 'simD',ylim = ylim0,
     ylab = 'freq',axes = FALSE,yaxt='n',xlab = 'Rank',cex.lab=cex0,cex.main=cex1)
abline(h=dim(dataD.rank)[1]/31,col='red',lty='dotted',lwd=2)
axis(1,at=seq(1,31,1),labels = seq(1,31,1),cex.axis=cex2)
axis(2,at=seq0,labels = seq0,las= HORIZONTAL<-1,cex.axis=cex2)

hist(dataE.rank[,ilead],breaks = seq(1,31),main = 'simE',ylim = ylim0,
     ylab = 'freq',axes = FALSE,yaxt='n',xlab = 'Rank',cex.lab=cex0,cex.main=cex1)
abline(h=dim(dataE.rank)[1]/31,col='red',lty='dotted',lwd=2)
axis(1,at=seq(1,31,1),labels = seq(1,31,1),cex.axis=cex2)
axis(2,at=seq0,labels = seq0,las= HORIZONTAL<-1,cex.axis=cex2)

hist(dataB.h.rank[,ilead],breaks = seq(1,31),main = 'simB',ylim = ylim0,
     ylab = 'freq',axes = FALSE,yaxt='n',xlab = 'Rank',cex.lab=cex0,cex.main=cex1)
abline(h=dim(dataB.h.rank)[1]/31,col='red',lty='dotted',lwd=2)
axis(1,at=seq(1,31,1),labels = seq(1,31,1),cex.axis=cex2)
axis(2,at=seq0,labels = seq0,las= HORIZONTAL<-1,cex.axis=cex2)

hist(dataD.h.rank[,ilead],breaks = seq(1,31),main = 'simD',ylim = ylim0,
     ylab = 'freq',axes = FALSE,yaxt='n',xlab = 'Rank',cex.lab=cex0,cex.main=cex1)
abline(h=dim(dataD.h.rank)[1]/31,col='red',lty='dotted',lwd=2)
axis(1,at=seq(1,31,1),labels = seq(1,31,1),cex.axis=cex2)
axis(2,at=seq0,labels = seq0,las= HORIZONTAL<-1,cex.axis=cex2)

hist(dataE.h.rank[,ilead],breaks = seq(1,31),main = 'simE',ylim = ylim0,
     ylab = 'freq',axes = FALSE,yaxt='n',xlab = 'Rank',cex.lab=cex0,cex.main=cex1)
abline(h=dim(dataE.h.rank)[1]/31,col='red',lty='dotted',lwd=2)
axis(1,at=seq(1,31,1),labels = seq(1,31,1),cex.axis=cex2)
axis(2,at=seq0,labels = seq0,las= HORIZONTAL<-1,cex.axis=cex2)


graphics.off()