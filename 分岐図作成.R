#分岐図作成
data <- read.csv("Phase_Speed_Data.csv", header=TRUE)

pchs <- c(16,2,3)
cols <- c("red","blue","black")
cexs <- c(1,1,1)
#xlims <- c(0,2900)
xlims <- c(1,30) #横軸サイズ変換用
ylims <- c(0.65,1.00)

#plot(0, 0, type="n", xlim=xlims, ylim=ylims, ylab="Phase speed (rad / time unit)", xlab="Number of oscillators", las=1)
plot(0, 0, type="n", xlim=xlims, ylim=ylims, ylab="Phase speed (rad / time unit)", xlab="Network size", las=1)

for(i in 1:nrow(data)){
  SIZE <- data$size[i]
  #node_num <- 3*SIZE*(SIZE+1)+1 #メッシュ状
  #node_num <- 3*(2^SIZE-1)+1 #樹状
  node_num <- SIZE #横軸サイズ変換用
  #cexs <- data$density[i]*0.07
  cexs <- 1.0
  if(cexs<0.0001) cexs<- 0
  par(new=T)
  if(data$number[i]==1) plot(node_num, data$speed[i], col=cols[1], pch=pchs[1], cex=cexs, xlim=xlims, ylim=ylims, xlab="", ylab="", las=1)
  if(data$number[i]==2) plot(node_num, data$speed[i], col=cols[2], pch=pchs[2], cex=cexs*0.8, xlim=xlims, ylim=ylims, xlab="", ylab="", las=1)
  if(data$number[i]==3) plot(node_num, data$speed[i], col=cols[3], pch=pchs[3], cex=cexs*0.8, xlim=xlims, ylim=ylims, xlab="", ylab="", las=1)
}

