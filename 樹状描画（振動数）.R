###ネットワーク描画（角振動数の可視化）###########################

###グラデーションの設定#################################
cols = colorRamp(c("#29E8A6","#2986E8","#7029E8","#E829BD","#E83929"))
########################################################

speed_min <- min(phase_speed)
speed_max <- max(phase_speed)

ommit <- 250 #何枚おきに描画するか
last <- time/ommit
for(i_2 in 0:last){
  i <- i_2*ommit+1
  
#for(i in 1:time){
  
  #保存するフォルダと図の名前を指定
  file_name <- sprintf("./size%d/Speed/Speed%d.png",size,i-1)
  png(file_name, width=700, height=700)
  
  #描画リセット
  plot(0, 0, xlim=c(-horizontal , horizontal), ylim=c(-vertical, vertical), type="n", xlab="", ylab="", xaxt="n", yaxt="n")
  
  #背景（白）
  plot(0, 0, cex=110, col="white", pch=15, xlab="", ylab="" , xaxt="n", yaxt="n")
  
  #リンクの描画（グレー線）
  for(j in 1:link_num){
    par(new=T)
    segments(link_position[j,1], link_position[j,2], link_position[j,3], link_position[j,4], xlim=c(-horizontal, horizontal), ylim=c(-vertical, vertical), xlab="", ylab="", lwd=1, col="grey40", lty=2)
  }
  
  
  #ノードの描画
  for(j in 1:node_num){
    par(new=T)
    plot(node_position[j,1], node_position[j,2], xlim=c(-horizontal, horizontal), ylim=c(-vertical, vertical), xlab="", ylab="", cex=2+3/size_count[j], lwd=2, pch=16, xaxt="n", yaxt="n",
         col=rgb(cols( (phase_speed[i,j]-speed_min) /(speed_max-speed_min) )/255))
  }
  
  #テキスト
  text(x=-0.10,y=-1.0,labels="TIMESTEP  :", cex=1, col="black")
  text(x=0.11,y=-1.0,labels=i-1, cex=1, col="black")
  
  
  #図の書き込み終了
  dev.off()
}
###ネットワーク描画（角振動数の可視化）###########################