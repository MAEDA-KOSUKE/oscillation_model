time <- 1000 #計算回数
min <- 0 #横軸の最小値
max <- 1.0 #横軸の最大値

#K <- 1.0
#A <- 1.5

Gx <- function(x){
  return(K/2*( A*(cos(2*pi*x)-cos(2*pi))+cos(pi*x)-cos(pi) ))
}

Fmin <- Gx(min)
Fmax <- Gx(max)

data1 <- matrix(nrow=time+1, ncol=2)

for(i in 0:time){
  data1[i+1,1] <- (max-min)*i/time+min
  data1[i+1,2] <- Gx((max-min)*i/time+min)
  
  if(data1[i+1,2]<Fmin) Fmin <- data1[i+1,2]
  if(data1[i+1,2]>Fmax) Fmax <- data1[i+1,2]
}

Fmin <- -1 #縦軸の最小値
Fmax <- 1 #縦軸の最大値


matplot(data1[,1],data1[,2],xlim=c(min,max), ylim=c(Fmin,Fmax), type="l",lwd=2.5, xlab="distance x",ylab="結合強度", col=1, lty=1, las=1)

par(new=T)
lines(x=c(-2,2),y=c(0,0),lty=3,col=1)

#メッシュ状における結合強度
G_plot <- matrix(0,nrow=(1/scale),ncol=2)
G_plot[,1] <- c(1:(1/scale))*scale
if(size>=1/scale-1){
  for(i in 0:(1/scale-1)) G_plot[(i+1),2] <- G[1,(3*i*(i+1)+2)] 
} else {
  for(i in 0:(size-1)) G_plot[(i+1),2] <- G[1,(3*i*(i+1)+2)] 
}
par(new=T)
matplot(G_plot[,1],G_plot[,2], xlim=c(min,max), ylim=c(Fmin,Fmax), type="o",lwd=2.5, xlab="distance x",ylab="結合強度",pch=1, col="orange", lty=1, las=1)
#matplot(G08[,1],G08[,2], xlim=c(min,max), ylim=c(Fmin,Fmax), type="o",lwd=2.5, xlab="distance x",ylab="結合強度",pch=1, col="blue", lty=1, las=1)

#G10 <- G_plot