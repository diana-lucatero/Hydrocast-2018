## -------------------------------------- ##
## ----- Makes plots of Paper 4 --------- ##
## ----- Diana Lucatero 28/02/2018 ------ ##
## -------------------------------------- ##

Dir <- 'C:/Users/DianaL/Desktop/Hydrocast/xin_data/'

## Load verification metrics for surface water
name_file <- paste0(Dir,'discharge/performance_A.txt')
dataA <- read.table(name_file,header=TRUE)
name_file <- paste0(Dir,'discharge/performance_B.txt')
dataB <- read.table(name_file,header=TRUE)
name_file <- paste0(Dir,'discharge/performance_B_rank.txt')
dataB.rank <- read.table(name_file,header=TRUE)
name_file <- paste0(Dir,'discharge/performance_C.txt')
dataC <- read.table(name_file,header=TRUE)
name_file <- paste0(Dir,'discharge/performance_D.txt')
dataD <- read.table(name_file,header=TRUE)
name_file <- paste0(Dir,'discharge/performance_D_rank.txt')
dataD.rank <- read.table(name_file,header=TRUE)
name_file <- paste0(Dir,'discharge/performance_E.txt')
dataE <- read.table(name_file,header=TRUE)
name_file <- paste0(Dir,'discharge/performance_E_rank.txt')
dataE.rank <- read.table(name_file,header=TRUE)

## Load verification metrics for subsurface water
name_file <- paste0(Dir,'head/performance_A.txt')
dataA.h <- read.table(name_file,header=TRUE)
name_file <- paste0(Dir,'head/performance_B.txt')
dataB.h <- read.table(name_file,header=TRUE)
name_file <- paste0(Dir,'head/performance_B_rank.txt')
dataB.h.rank <- read.table(name_file,header=TRUE)
name_file <- paste0(Dir,'head/performance_C.txt')
dataC.h <- read.table(name_file,header=TRUE)
name_file <- paste0(Dir,'head/performance_D.txt')
dataD.h <- read.table(name_file,header=TRUE)
name_file <- paste0(Dir,'head/performance_D_rank.txt')
dataD.h.rank <- read.table(name_file,header=TRUE)
name_file <- paste0(Dir,'head/performance_E.txt')
dataE.h <- read.table(name_file,header=TRUE)
name_file <- paste0(Dir,'head/performance_E_rank.txt')
dataE.h.rank <- read.table(name_file,header=TRUE)

## Start plotting
## Scores
m <- matrix(c(seq(1:6),7,7,7),nrow = 3,ncol = 3,byrow = TRUE)
#m <- matrix(1,nrow = 1,ncol = 1,byrow = TRUE)
name_plt = paste0(Dir,'crps.png')
png(name_plt,width=900,height=900)
layout(mat = m,heights = c(1,1,0.2))
par(mar=c(5.1,8,4.1,2.1))
col0 <- c('black','firebrick3','goldenrod1','forestgreen','hotpink4')
ylim0 <- c(0,0.12)
ylim1 <- c(-0.1,0.1)
pch0 <- 19

## ------------------------- Surface water --------------------------------- ##

## Title
main0 <- paste0('Discharge Station 21127')
## BIAS
plot(dataA$bias,main = main0,type='o',pch=pch0,col= col0[1],ylim = ylim1,
     axes=FALSE, xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
lines(dataB$bias,type='o',pch=pch0,col= col0[2],ylim = ylim1, 
      xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
lines(dataC$bias,type='o',pch=pch0,col= col0[3],ylim = ylim1, 
      xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
lines(dataD$bias,type='o',pch=pch0,col= col0[4],ylim = ylim1, 
      xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
lines(dataE$bias,type='o',pch=pch0,col= col0[5],ylim = ylim1, 
      xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
grid(nx=NULL,ny=NULL,col = 'lightgray',lty = 'dotted',equilogs = TRUE)
axis(1,at=seq(0,48,4),labels =seq(0,48,4),cex.axis=1.5,las= HORIZONTAL<-1)
seq0 <- round(seq(ylim1[1],ylim1[2],(ylim1[2]-ylim1[1])/5),digits = 4)
axis(2,at=seq0,labels = seq0,cex.axis=1.5,las= HORIZONTAL<-1)
mtext('bias [m3/s]',2,6,cex=1.5)
#mtext('lead time (hours)',1,3,cex=1.3)

## Title
main0 <- paste0('Discharge Station 21127')
## RMSE
plot(dataA$rmse,main = main0,type='o',pch=pch0,col= col0[1],ylim = ylim0,
     axes=FALSE, xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
lines(dataB$rmse,type='o',pch=pch0,col= col0[2],ylim = ylim0, 
      xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
lines(dataC$rmse,type='o',pch=pch0,col= col0[3],ylim = ylim0, 
      xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
lines(dataD$rmse,type='o',pch=pch0,col= col0[4],ylim = ylim0, 
      xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
lines(dataE$rmse,type='o',pch=pch0,col= col0[5],ylim = ylim0, 
      xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
grid(nx=NULL,ny=NULL,col = 'lightgray',lty = 'dotted',equilogs = TRUE)
axis(1,at=seq(0,48,4),labels =seq(0,48,4),cex.axis=1.5,las= HORIZONTAL<-1)
seq0 <- round(seq(ylim0[1],ylim0[2],(ylim0[2]-ylim0[1])/5),digits = 4)
axis(2,at=seq0,labels = seq0,cex.axis=1.5,las= HORIZONTAL<-1)
mtext('rmse [m3/s]',2,6,cex=1.5)
#mtext('lead time (hours)',1,3,cex=1.3)

## Title
main0 <- paste0('Discharge Station 21127')
## RMSE
plot(dataA$crps,main = main0,type='o',pch=pch0,col= col0[1],ylim = ylim0,
     axes=FALSE, xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
lines(dataB$crps,type='o',pch=pch0,col= col0[2],ylim = ylim0, 
      xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
lines(dataC$crps,type='o',pch=pch0,col= col0[3],ylim = ylim0, 
      xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
lines(dataD$crps,type='o',pch=pch0,col= col0[4],ylim = ylim0, 
      xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
lines(dataE$crps,type='o',pch=pch0,col= col0[5],ylim = ylim0, 
      xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
grid(nx=NULL,ny=NULL,col = 'lightgray',lty = 'dotted',equilogs = TRUE)
axis(1,at=seq(0,48,4),labels =seq(0,48,4),cex.axis=1.5,las= HORIZONTAL<-1)
seq0 <- round(seq(ylim0[1],ylim0[2],(ylim0[2]-ylim0[1])/5),digits = 4)
axis(2,at=seq0,labels = seq0,cex.axis=1.5,las= HORIZONTAL<-1)
mtext('crps [m3/s]',2,6,cex=1.5)
#mtext('lead time (hours)',1,3,cex=1.3)

### ------------------------ Subsurface water ------------------------------------------ ##

## Title
main0 <- paste0('Borehole nr. 3278')
## CRPS
ylim0 <- c(0,0.4)
plot(dataA.h$bias,main = main0,type='o',pch=pch0,col=col0[1],ylim = ylim0,
     axes=FALSE, xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
lines(dataB.h$bias,type='o',pch=pch0,col= col0[2],ylim = ylim0,
      xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
lines(dataC.h$bias,type='o',pch=pch0,col= col0[3],ylim = ylim0,
      xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
lines(dataD.h$bias,type='o',pch=pch0,col= col0[4],ylim = ylim0,
      xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
lines(dataE.h$bias,type='o',pch=pch0,col= col0[4],ylim = ylim0,
      xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
grid(nx=NULL,ny=NULL,col = 'lightgray',lty = 'dotted',equilogs = TRUE)
axis(1,at=seq(0,48,4),labels =seq(0,48,4),cex.axis=1.5,las= HORIZONTAL<-1)
seq0 <- round(seq(ylim0[1],ylim0[2],(ylim0[2]-ylim0[1])/5),digits = 4)
axis(2,at=seq0,labels = seq0,cex.axis=1.5,las= HORIZONTAL<-1)
mtext('bias [m]',2,6,cex=1.5)
mtext('lead time (hours)',1,3,cex=1.3)

## Title
main0 <- paste0('Borehole nr. 3278')
## CRPS
#ylim0 <- c(min(dataA.h$rmse),max(dataA.h$rmse))
plot(dataA.h$rmse,main = main0,type='o',pch=pch0,col=col0[1],ylim = ylim0,
     axes=FALSE, xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
lines(dataB.h$rmse,type='o',pch=pch0,col= col0[2],ylim = ylim0,
      xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
lines(dataC.h$rmse,type='o',pch=pch0,col= col0[3],ylim = ylim0,
      xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
lines(dataD.h$rmse,type='o',pch=pch0,col= col0[4],ylim = ylim0,
      xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
lines(dataE.h$rmse,type='o',pch=pch0,col= col0[4],ylim = ylim0,
      xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
grid(nx=NULL,ny=NULL,col = 'lightgray',lty = 'dotted',equilogs = TRUE)
axis(1,at=seq(0,48,4),labels =seq(0,48,4),cex.axis=1.5,las= HORIZONTAL<-1)
seq0 <- round(seq(ylim0[1],ylim0[2],(ylim0[2]-ylim0[1])/5),digits = 4)
axis(2,at=seq0,labels = seq0,cex.axis=1.5,las= HORIZONTAL<-1)
mtext('rmse [m]',2,6,cex=1.5)
mtext('lead time (hours)',1,3,cex=1.3)

## Title
main0 <- paste0('Borehole nr. 3278')
## CRPS
#ylim0 <- c(min(dataA.h$crps),max(dataA.h$crps))
plot(dataA.h$crps,main = main0,type='o',pch=pch0,col=col0[1],ylim = ylim0,
     axes=FALSE, xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
lines(dataB.h$crps,type='o',pch=pch0,col= col0[2],ylim = ylim0,
      xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
lines(dataC.h$crps,type='o',pch=pch0,col= col0[3],ylim = ylim0,
      xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
lines(dataD.h$crps,type='o',pch=pch0,col= col0[4],ylim = ylim0,
      xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
lines(dataE.h$crps,type='o',pch=pch0,col= col0[4],ylim = ylim0,
      xlab = '',ylab = '',xaxt='n',yaxt='n',cex.main=2,xaxs="i", yaxs="i")
grid(nx=NULL,ny=NULL,col = 'lightgray',lty = 'dotted',equilogs = TRUE)
axis(1,at=seq(0,48,4),labels =seq(0,48,4),cex.axis=1.5,las= HORIZONTAL<-1)
seq0 <- round(seq(ylim0[1],ylim0[2],(ylim0[2]-ylim0[1])/5),digits = 4)
axis(2,at=seq0,labels = seq0,cex.axis=1.5,las= HORIZONTAL<-1)
mtext('crps [m]',2,6,cex=1.5)
mtext('lead time (hours)',1,3,cex=1.3)

## -------------------- Legend ---------------------------------------- ##

## Legend

par(mar=c(0,0,0,0))
# Legend
plot(1, type="n", xlab="", ylab="",xaxt='n',yaxt='n', axes = F)
legend0 <- c('simA','simB','simC','simD','simE')
legend('center',legend = legend0,col = col0,pch = 19,cex = 3,ncol = 5)

graphics.off()
