###ネットワーク描画#####################################
horizontal <- 1 #横軸範囲
vertical <- 1 #縦軸範囲
node_num <- 3*(2^size-1)+1 #ノード総数
link_num <- node_num-1 #リンク総数


###各ノードが何周目に位置しているか計算
size_count <- numeric(node_num)
size_count[1] <- 1
mark <- 1
for(i in 2:node_num){
  size_count[i] <- mark
  if(i==(3*(2^mark-1)+1)) mark <- mark+1
}
#######################################

length <- numeric(node_num) #描画するリンク1本の長さ
length <- 0.28/size_count



angle <- numeric(node_num) #親ノードに対する角度
node_position <- matrix(0, nrow=node_num, ncol=2) #ノードの座標(x,y)
link_position <- matrix(0, nrow=link_num, ncol=4) #リンクの座標(x1,y1,x2,y2)


###設定#################
angle[2:4] <- c(pi/6, 5*pi/6, 3*pi/2)
for(i in 2:4) node_position[i,] <- c(cos(angle[i])*length[i], sin(angle[i])*length[i])
########################


###ノードの座標を算出###
if(size>=2){
  for(i in 2:(3*(2^(size-1)-1)+1)){
    angle[(i*2+1):(i*2+2)] <- c((angle[i]-pi/(size_count[i]+3)), (angle[i]+pi/(size_count[i]+3)))
    node_position[(i*2+1),] <- c(node_position[i,1]+cos(angle[i*2+1])*length[i], node_position[i,2]+sin(angle[i*2+1])*length[i])
    node_position[(i*2+2),] <- c(node_position[i,1]+cos(angle[i*2+2])*length[i], node_position[i,2]+sin(angle[i*2+2])*length[i])
  }
}
########################


###リンクの座標を算出###
for(i in 1:3) link_position[i,] <- c(0,0,node_position[(i+1),1],node_position[(i+1),2])

if(size>=2){
  for(i in 2:(3*(2^(size-1)-1)+1)){
    link_position[(i*2+0),] <- c(node_position[i,1], node_position[i,2], node_position[(i*2+1),1], node_position[(i*2+1),2])
    link_position[(i*2+1),] <- c(node_position[i,1], node_position[i,2], node_position[(i*2+2),1], node_position[(i*2+2),2])
  }
}
########################