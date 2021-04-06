#データ読み込み & 行列変換
phase <- read.csv("Phase.csv")
phase <- matrix(as.matrix(phase), nrow(phase), ncol(phase))
phase_speed <- read.csv("Speed.csv")
phase_speed <- matrix(as.matrix(phase_speed), nrow(phase_speed), ncol(phase_speed))

#基本情報
SIZE <- 10

#ノードの通し番号の選択
number <- c(1,2)
#for(i in 1:(SIZE-1)) number <- c(number, (3*(2^i-1)+2)) #樹状
for(i in 1:(SIZE-1)) number <- c(number, 3*(i)*(i+1)+2 ) #メッシュ状

#グラフィックパラメータ
cols <- rainbow(12)

#位相の時間変化のプロット
start <- 0
last <- 2000
xlims <- c(start,last)*0.25
ylims <- c(0,500)

plot(0, 0, type="n", xlim=xlims, ylim=ylims, ylab="Phase (rad)", xlab="Time", las=1)
for(i in 1:length(number)){
  par(new=T)
  plot(c(start:last)*0.25, phase[(c(start:last)+1), number[i]], lwd=2, type="l", col=cols[i], xlim=xlims, ylim=ylims, xlab="", ylab="", las=1)
}




#振動数の時間変化のプロット
last <- 2000
xlims <- c(0,last)*0.25
ylims <- c(0.75,1.22)

plot(0, 0, type="n", xlim=xlims, ylim=ylims, ylab="Phase speed (rad / time unit)", xlab="Time", las=1)
for(i in 1:length(number)){
  par(new=T)
  plot(c(1:last)*0.25, phase_speed[(c(1:last)), number[i]], lwd=2, type="l", col=cols[i], xlim=xlims, ylim=ylims, xlab="", ylab="", las=1)
}



#位相の時間変化（0~2pi）
start <- 3650
last <- 4000
xlims <- c(start,last)*0.25
ylims <- c(0,(2*pi))

plot(0, 0, type="n", xlim=xlims, ylim=ylims, ylab="Phase (rad)", xlab="Time", las=1)
for(i in 1:length(number)){
  par(new=T)
  plot(c(start:last)*0.25, phase[(c(start:last)+1), number[i]]%%(2*pi), cex=0.5, type="p", pch=16, col=cols[i], xlim=xlims, ylim=ylims, xlab="", ylab="", las=1)
}



#各位置ごとの位相のプロット(時間固定・メッシュ状専用)
t <- 1000
xlims <- c(-1,1)
ylims <- c(0,2*pi)

plot(0, 0, type="n", xlim=xlims, ylim=ylims, ylab="Phase speed (rad / time unit)", xlab="X axis coordinates", las=1)
par(new=T)
plot(0, (phase[(t+1), 1]%%(2*pi)), lwd=2, type="p", xlim=xlims, ylim=ylims, xlab="", ylab="", las=1)
for(i in 1:SIZE){
  par(new=T)
  n <- 3*(i-1)*(i)+2
  plot(node_position[n,1], phase[(t+1), n]%%(2*pi), type="p", xlim=xlims, ylim=ylims, xlab="", ylab="", las=1)
  par(new=T)
  n <- n+3*i
  plot(node_position[n,1], phase[(t+1), n]%%(2*pi), type="p", xlim=xlims, ylim=ylims, xlab="", ylab="", las=1)
}