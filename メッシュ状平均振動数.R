###パッケージのセット(MASS)############################
targetPackages <- c('MASS') 
newPackages <- targetPackages[!(targetPackages %in% installed.packages()[,"Package"])]
if(length(newPackages)) install.packages(newPackages, repos = "http://cran.us.r-project.org")
for(package in targetPackages) library(package, character.only = T)
########################################################

###角振動数の平均値#########################
speed_mean <- apply(phase_speed[2000:time,], 2, mean)
speed_mean_max <- max(speed_mean)
speed_mean_min <- min(speed_mean)

###ピーク振動数検出#########################
dense <- density(speed_mean, bw = "nrd") #カーネル密度推定法
y_max <- max(dense$y)*1.2 #縦軸の最大値
x_max <- (speed_mean_max-(speed_mean_max%%0.005))+0.005
x_min <- (speed_mean_min-(speed_mean_min%%0.005))
peak <- matrix(0,nrow=1,ncol=2)
peak_count <- 0

for(i in 2:(length(dense$x)-1)){
  if(dense$y[i]>dense$y[i-1] && dense$y[i]>dense$y[i+1]){
    peak_count <- peak_count+1
    if(peak_count==1){ peak[1,] <-  c(dense$x[i],dense$y[i]) }
    else {  peak <- rbind(peak,c(dense$x[i],dense$y[i])) }
  }
}

peak <- peak[order(peak[,2],decreasing=T),]
###ピーク振動数検出#########################


###ファイル出力（位相・振動数）#########################
data <- rbind(peak,c(max(phase_speed),min(phase_speed)),c(speed_mean_min,speed_mean_max))
file_name <- sprintf("./size%d/Mean_Speed.csv",size)
write.csv(data, file = file_name, row.names = F)
########################################################

###ヒストグラム#############################
#保存するフォルダと図の名前を指定
file_name <- sprintf("./size%d/Speed_hist.png",size)
png(file_name, width=700, height=700)

truehist(speed_mean,
         #hist(speed_mean,
         #prob = FALSE,
         #breaks=seq(x_min,x_max,0.005), #横軸(最小値,最大値,刻み幅)
         #ylim=c(0,y_max), #縦軸メモリ範囲
         ylim=c(0,35),
         xlab="Mean Phase Speed (rad/time unit)", #横軸の名前
         ylab="Density", #縦軸の名前
         main="",             #表題（今回は空白）
         las=1,               #縦軸メモリを90°回転
         #xaxs="i", 
         yaxs="i",  #横軸と縦軸が最小値で交わる
         col="white")
#色の選択
#box() #外枠の追加

#密度関数描画
lines(dense, col="orange",lwd=2)

#ピークプロット
#if(peak_count>0){
#  for(i in 1:nrow(peak)){
#    par(new=T)
#    plot(peak[i,1], peak[i,2], col="blue", bty = "n", pch=1, cex=1.3, xlab="", ylab="", main="", yaxs="i", las=1, 
#         xlim=c(x_min,x_max), #横軸範囲
#         ylim=c(0,y_max) #縦軸範囲
#    )
#  }
#}

#図の書き込み終了
dev.off()
###ヒストグラム#############################


###ネットワーク描画（角振動数の平均値の可視化）###########################

###グラデーションの設定#################################
cols = colorRamp(c("#29E8A6","#2986E8","#7029E8","#E829BD","#E83929"))
########################################################

#保存するフォルダと図の名前を指定
file_name <- sprintf("./size%d/Speed_Mean.png",size)
png(file_name, width=700, height=700)

#描画リセット
plot(0, 0, xlim=c(-horizontal , horizontal), ylim=c(-vertical, vertical), type="n", xlab="", ylab="", xaxt="n", yaxt="n")

#背景（白）
plot(0, 0, cex=110, xlim=c(-horizontal , horizontal), ylim=c(-vertical, vertical), col="white", pch=15, xlab="", ylab="" , xaxt="n", yaxt="n")

#リンクの描画
for(j in 1:all_link){
  par(new=T)
  segments(link_position[j,1], link_position[j,2], link_position[j,3], link_position[j,4], xlim=c(-horizontal, horizontal), ylim=c(-vertical, vertical), xaxt="n", yaxt="n", xlab="", ylab="", lwd=1, col="grey40", lty=2)
}


#ノードの描画
for(j in 1:all_node){
  par(new=T)
  plot(node_position[j,1], node_position[j,2], xlim=c(-horizontal, horizontal), ylim=c(-vertical, vertical), xlab="", ylab="", cex=20/size+(5/6), lwd=2, pch=16, xaxt="n", yaxt="n",
       col=rgb(cols( (speed_mean[j]-speed_mean_min) /(speed_mean_max-speed_mean_min) )/255))
}

#図の書き込み終了
dev.off()
################################################