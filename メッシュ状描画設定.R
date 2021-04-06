#ネットワーク描画

###sizeによって描画範囲を指定################################
if(size==1){
  horizontal <- 1.15 #横軸範囲
  vertical <- 1.15 #縦軸範囲
}
if(size==2){
  horizontal <- 1.15 #横軸範囲
  vertical <- 1.15 #縦軸範囲
}
if(size>=3 && size<=19){
  horizontal <- 1.00 #横軸範囲
  vertical <- 1.00 #縦軸範囲
}
if(size>=20){
  horizontal <- 0.95 #横軸範囲
  vertical <- 0.95 #縦軸範囲
}
###ネットワークの情報################################

all_link <- 3*size*(3*size+1) #リンクの総数
all_node <- 3*size*(size+1)+1 #ノードの総数
link_num <- 3*size*(3*size+1) #リンクの総数
node_num <- 3*size*(size+1)+1 #ノードの総数
node_position <- matrix(0, nrow=all_node, ncol=2)  #各ノードの座標を表す行列(x,y)
link_position <- matrix(0, nrow=all_link, ncol=4)  #各リンクの座標を表す行列(x1,y1,x2,y2)
size_count <- 1 #何周目を計算しているかを表す
node_count <- 1 #何辺目を計算しているかを表す（各頂点を過ぎたら+1）
node_start <- 2 #始点ノードの通し番
###ネットワークの情報################################


###補完接続行列の作成################################

compA <- matrix(0, nrow=all_link, ncol=all_node) #補完接続行列

size_count <- 1 #何周目について扱っているかを表す

for(i in 1:all_link){
  
  if(i<=6){  #1周目1～6番目のリンク
    compA[i,1] <- 1
    compA[i,i+1] <- -1
  }
  
  if(i>=7 && i<=11){  #1周目7～11番目のリンク
    compA[i,i-5] <- 1
    compA[i,i-4] <- -1
  }    
  
  if(i==12){  #1周目12番目のリンク
    compA[12,7] <- 1
    compA[12,2] <- -1
  }
  
  if(i>=13){  #2周目以降のリンク
    if(i>3*(size_count-1)*(3*(size_count-1)+1) && i<=3*(size_count-1)*(3*(size_count-1)+1) + 12*size_count-6){ #外向き
      compA[i,link_start] <- 1
      compA[i,link_goal] <- -1
      
      if(i==link_first+2*(size_count-2)*(node_count_start-1)+3*(node_count_start-1)+1){ #外向きリンクの始点ノードの指定（頂点）
        link_start <- link_start+1
        node_count_start <- node_count_start+1
      }else if((i-(link_first+2*(size_count-2)*(node_count_start-2)+3*(node_count_start-2)+1))%%2 == 0  && i!=link_first+2*(size_count-2)*(node_count_start-1)+3*(node_count_start-1)){ #外向きリンクの始点ノードの指定（頂点以外）
        link_start <- link_start+1
      }
      
      if(i==link_first+2*(size_count-1)*(node_count_goal-1)+(node_count_goal-1)){ #外向きリンクの終点ノードの指定（頂点）
        link_goal <- link_goal+1
        node_count_goal <- node_count_goal+1
      }else if((i-(link_first+2*(size_count-1)*(node_count_goal-2)+(node_count_goal-2)))%%2 == 0){ #外向きリンクの始点ノードの指定（頂点以外）
        link_goal <- link_goal+1
      }
      
      if(i==3*(size_count-1)*(3*(size_count-1)+1) + 12*size_count-6){  #各周の最後の外向きリンクの書き換え
        compA[i,] <- 0
        compA[i,3*(size_count-2)*(size_count-1)+2] <- 1
        compA[i,3*size_count*(size_count+1)+1] <- -1
      }
    }
    
    
    if(i>3*(size_count-1)*(3*(size_count-1)+1) + 12*size_count-6 && i<3*size_count*(3*size_count+1)){ #周回向き
      compA[i,3*(size_count-1)*size_count+2 + i-(3*(size_count-1)*(3*(size_count-1)+1) + 12*size_count-6 +1)] <- 1
      compA[i,3*(size_count-1)*size_count+2 + i-(3*(size_count-1)*(3*(size_count-1)+1) + 12*size_count-6)-1+1] <- -1
      
    }
    
    if(i==3*size_count*(3*size_count+1)){  #各周の最後の外向きリンクの書き換え
      compA[i,] <- 0
      compA[i,3*size_count*(size_count+1)+1] <- 1
      compA[i,3*(size_count-1)*size_count+2] <- -1
    }
  }
  
  
  
  if(i==3*size_count*(3*size_count+1)){
    node_count_start <- 1  #外向きリンクについて考える際の頂点ノードの出現数
    node_count_goal <- 1
    link_start <- 3*(size_count-1)*size_count+2  #外向きリンクの始点ノード
    link_goal <- 3*size_count*(size_count+1)+2  #外向きリンクの終点ノード
    size_count <- size_count+1  #サイズカウントの更新
    link_first <- 3*(size_count-1)*(3*(size_count-1)+1)+1  #各周における最初のリンク番号
  }
}
###補完接続行列の作成################################


###リンク接続行列の作成###

link_connection <- matrix(0, nrow=all_link, ncol=2)  #リンク接続行列[始点,終点]

#補完接続行列から各リンクの接続ノードをリンク接続行列に格納する
for(i in 1:all_link){
  for(j in 1:all_node){
    
    if(compA[i,j]==1){  #始点の探索
      link_connection[i,1] <- j
    }
    
    if(compA[i,j]==-1){  #終点の探索
      link_connection[i,2] <- j
    }
    
  }
}
###リンク接続行列の作成###


###各ノードの座標を計算###############################
node_position <- matrix(0, nrow=node_num, ncol=2)  #各ノードの座標を表す行列(x,y)
length <- 1/size #各リンクの長さ
size_count <- 1 #三角格子において計算を行う周回数
node_count <- 1 #各周の六角形において計算を行う辺（各頂点を過ぎたら+1）
node_start <- 2 #始点ノードの通し番

for(i in 2:node_num){
  if(i==node_start){ #各周における始点の座標指定
    node_position[i,1] <- size_count*length
    node_position[i,2] <- 0
    
  }else if(node_count==1){ #辺1におけるノードの座標指定
    node_position[i,1] <- node_position[i-1,1]-length/2
    node_position[i,2] <- node_position[i-1,2]+length*sqrt(3)/2
    
  }else if(node_count==2){ #辺2におけるノードの座標指定
    node_position[i,1] <- node_position[i-1,1]-length
    node_position[i,2] <- node_position[i-1,2]
    
  }else if(node_count==3){ #辺3におけるノードの座標指定
    node_position[i,1] <- node_position[i-1,1]-length/2
    node_position[i,2] <- node_position[i-1,2]-length*sqrt(3)/2
    
  }else if(node_count==4){ #辺4におけるノードの座標指定
    node_position[i,1] <- node_position[i-1,1]+length/2
    node_position[i,2] <- node_position[i-1,2]-length*sqrt(3)/2
    
  }else if(node_count==5){ #辺5におけるノードの座標指定
    node_position[i,1] <- node_position[i-1,1]+length
    node_position[i,2] <- node_position[i-1,2]
    
  }else if(node_count==6){ #辺6におけるノードの座標指定
    node_position[i,1] <- node_position[i-1,1]+length/2
    node_position[i,2] <- node_position[i-1,2]+length*sqrt(3)/2
  }
  
  
  if(i==node_start+size_count*1){ #ノードカウントの更新
    node_count <- node_count+1
  }else if(i==node_start+size_count*2){ 
    node_count <- node_count+1
  }else if(i==node_start+size_count*3){ 
    node_count <- node_count+1  
  }else if(i==node_start+size_count*4){ 
    node_count <- node_count+1
  }else if(i==node_start+size_count*5){ 
    node_count <- node_count+1
  }
  
  if(i==3*size_count*(size_count+1)+1){ #各周における計算終了・次周の計算
    size_count <- size_count+1 #サイズカウントの更新
    node_count <- 1 #ノードカウントを1に戻す
    node_start <- 3*(size_count-1)*size_count+2 #始点座標の更新
  }
} #座標計算
###各ノードの座標を計算###############################


###各リンクの座標を計算##################
for(i in 1:all_link){
  link_position[i,1] <- node_position[link_connection[i,1],1]
  link_position[i,2] <- node_position[link_connection[i,1],2]
  link_position[i,3] <- node_position[link_connection[i,2],1]
  link_position[i,4] <- node_position[link_connection[i,2],2]
}
###各リンクの座標を計算##################