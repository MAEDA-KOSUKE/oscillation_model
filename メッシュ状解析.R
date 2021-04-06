###パッケージのセット(tcltk)############################
targetPackages <- c('tcltk') 
newPackages <- targetPackages[!(targetPackages %in% installed.packages()[,"Package"])]
if(length(newPackages)) install.packages(newPackages, repos = "http://cran.us.r-project.org")
for(package in targetPackages) library(package, character.only = T)
########################################################

###ネットワーク情報###
link_num <- 3*size*(3*size+1) #リンクの総数
node_num <- 3*size*(size+1)+1 #ノードの総数
###ネットワーク情報###


###ノード同士の接続を記述 nodes[右,右上,左上,左,左下,右下]###
nodes <- matrix(nrow=node_num, ncol=6)
nodes[1,] <- c(2,3,4,5,6,7)
size_count <- 1 #三角格子において計算を行う周回数
side_count <- 1 #各周の六角形において計算を行う辺
node_count <- 1 #各辺における計算を行ったノード数
node_pre <-  1 #前周の始点ノード
node_next <- 8 #次周の始点ノード

for(i in 2:node_num){ #各ノードについて隣接ノードを考える
  
  side <- (side_count+c(0,1,2,3,4,5))%%6
  for(j in 1:6) if(side[j]==0) side[j]<-6
  
  nodes[i,side[1]] <- node_next+(size_count+1)*(side_count-1)+node_count-1
  nodes[i,side[2]] <- node_next+(size_count+1)*(side_count-1)+node_count
  nodes[i,side[3]] <- i+1
  nodes[i,side[4]] <- node_pre+(size_count-1)*(side_count-1)+node_count-1
  if(node_count==1){
    nodes[i,side[5]] <- i-1
    nodes[i,side[6]] <- node_next+(size_count+1)*(side_count-2)+size_count
  } else {
    nodes[i,side[5]] <- node_pre+(size_count-1)*(side_count-1)+node_count-2
    nodes[i,side[6]] <- i-1
  }
  
  if(side_count==1 && node_count==1){ #辺１のノード１について(周のつなぎ目)
    nodes[i,5] <- node_next-1
    nodes[i,6] <- 3*(size_count+1)*(size_count+2)+1
  }
  
  if(side_count==6 && node_count==size_count){ #辺６の最終ノードについて(周のつなぎ目)
    nodes[i,1] <- 3*(size_count+1)*(size_count+2)+1
    nodes[i,2] <- 3*(size_count-1)*(size_count)+2
    nodes[i,3] <- node_pre
  }
  
  if(size_count==size){ #最外周の考慮
    nodes[i,side[1]] <- 0
    nodes[i,side[2]] <- 0
    if(node_count==1) nodes[i,side[6]]<-0
  } #最外周の考慮
  
  
  if(node_count==size_count){ #各辺の計算終了
    side_count <- side_count+1 #更新
    node_count <- 1 #初期化
  } else {
    node_count <- node_count+1 #更新
  }
  
  if(i==(3*size_count*(size_count+1)+1)){ #各周の計算終了
    size_count <- size_count+1 #更新
    side_count <- 1 #初期化
    node_pre <-  3*(size_count-2)*(size_count-1)+2 #更新
    node_next <- 3*(size_count)*(size_count+1)+2 #更新
  }
  
} #各ノードについて隣接ノードを考える
###ノード同士の接続を記述 nodes[右,右上,左上,左,左下,右下]####


###pattern行列の作成####################################
pattern <- matrix(0, ncol=node_num, nrow=node_num) #２つのノードの位置関係(1or2)

for(i in 1:node_num){ #各ノードについて
  for(j in 1:6){ #各方向について
    child <- nodes[i,j] #子ノード
    
    while(child!=0){ #子ノードが端点ではない場合
      pattern[i,child] <- j #パターン行列に格納
      child <- nodes[child,j] #子ノードの更新
    }
    
  } #各方向について
} #各ノードについて
###pattern行列の作成####################################


###ノードの座標計算#####################################
node_position <- matrix(0, nrow=node_num, ncol=2)  #各ノードの座標を表す行列(x,y)
length <- 1 #各リンクの長さ
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
###ノードの座標計算#####################################


###ノード間ルートの距離算出#####################################
distance <- matrix(list(), nrow=node_num, ncol=node_num) #距離を格納
trace_num <- trunc(1/scale) #距離が1以下である最大のリンクの本数

for(i in 1:(node_num-1)){ #ノードiについて
  for(j in (i+1):node_num){ #ノードjについて
    
    NG_even <- 0 #1で偶数ルート探索終了
    NG_odd <- 0  #1で奇数ルート探索終了
    
    #直線距離の計算
    st <- ((node_position[i,1]-node_position[j,1])^2+(node_position[i,2]-node_position[j,2])^2)^(1/2) #直線距離
    st <- round(st) #小数点以下を丸める(不必要かもしれない)
    
    if(pattern[i,j]!=0 && (st*scale)<1){ #パターンA(直線上にある) #stはリンク数に対応する
      
      #第１ルート
      distance[[i,j]] <- c(distance[[i,j]],st) #最短距離の格納
      distance[[j,i]] <- c(distance[[j,i]],st) #最短距離の格納
      
      #第２・第３ルート
      if((st+1)*scale<1){
        direction <- (c(pattern[i,j],(pattern[i,j]-1),(pattern[i,j]+1),(pattern[i,j]-2),(pattern[i,j]+2),(pattern[i,j]-3)))%%6
        for(k in 1:6) if(direction[k]==0) direction[k] <- 6
        
        #第２ルート探索
        for(k in 1:(st+1)){
          if(k==1) child <- nodes[i,direction[2]]
          if(k>=2 && k<=st) child <- nodes[child,direction[1]]
          if(k==(st+1)){
            distance[[i,j]] <- c(distance[[i,j]],(st+1)) #距離の格納
            distance[[j,i]] <- c(distance[[j,i]],(st+1)) #距離の格納
          }
          if(child==0){ #ノードが存在しない場合
            NG_even <- 1
            break
          }
        }#第２ルート探索
        
        #第３ルート探索
        for(k in 1:(st+1)){
          if(k==1) child <- nodes[i,direction[3]]
          if(k>=2 && k<=st) child <- nodes[child,direction[1]]
          if(k==(st+1)){
            distance[[i,j]] <- c(distance[[i,j]],(st+1)) #距離の格納
            distance[[j,i]] <- c(distance[[j,i]],(st+1)) #距離の格納
          }
          if(child==0){ #ノードが存在しない場合
            NG_odd <- 1
            break
          }
        }#第３ルート探索
        
        
      } #第２・第３ルート
      else { #距離がオーバーしている場合探索終了
        NG_even <- 1
        NG_odd <- 1
      }
      
      #第４・第５ルート
      if((st+4)*scale<1){
        #第４ルート探索
        if(NG_even==0){
          for(k in 1:(st+4)){
            if(k==1) child <- nodes[i,direction[4]]
            if(k==2) child <- nodes[child,direction[2]]
            if(k>=3 && k<=(st+2)) child <- nodes[child,direction[1]]
            if(k==(st+3)) child <- nodes[child,direction[3]]
            if(k==(st+1)){
              distance[[i,j]] <- c(distance[[i,j]],(st+4)) #距離の格納
              distance[[j,i]] <- c(distance[[j,i]],(st+4)) #距離の格納
            }
            if(child==0){ #ノードが存在しない場合
              NG_even <- 1
              break
            }
          }
        }#第４ルート探索
        
        #第５ルート探索
        if(NG_odd==0){
          for(k in 1:(st+4)){
            if(k==1) child <- nodes[i,direction[5]]
            if(k==2) child <- nodes[child,direction[3]]
            if(k>=3 && k<=(st+2)) child <- nodes[child,direction[1]]
            if(k==(st+3)) child <- nodes[child,direction[2]]
            if(k==(st+1)){
              distance[[i,j]] <- c(distance[[i,j]],(st+4)) #距離の格納
              distance[[j,i]] <- c(distance[[j,i]],(st+4)) #距離の格納
            }
            if(child==0){ #ノードが存在しない場合
              NG_even <- 1
              break
            }
          }
        }#第５ルート探索
        
      } #第４・５ルート
      else { #距離がオーバーしている場合探索終了
        NG_even <- 1
        NG_odd <- 1
      }
      
      #第６・第７ルート以降
      if((st+9)*scale<1){
        #偶数ルート
        count <- 1 #試行回数
        trace <- st+9 #距離
        while(NG_even==0){
          for(k in 1:trace){
            if(k<=count){
              if(k==1){ child <- nodes[i,direction[6]] } 
              else { child <- nodes[child,direction[6]] }
            }
            if(k==(count+1)) child <- nodes[child,direction[4]]
            if(k>=(count+2) && k<=(count*2+2)) child <- nodes[child,direction[2]]
            if(k>=(count*2+3) && k<=(count*3+2+st)) child <- nodes[child,direction[1]]
            if(k>=(count*3+3+st) && k<=(count*4+3+st)) child <- nodes[child,direction[3]]
            if(k==(count*4+4+st)) child <- nodes[child,direction[5]]
            if(k>=(count*4+5+st)) child <- nodes[child,direction[6]]
            if(k==trace){
              distance[[i,j]] <- c(distance[[i,j]],trace) #距離の格納
              distance[[j,i]] <- c(distance[[j,i]],trace) #距離の格納
            }
            if(child==0){ #ノードが存在しない場合
              NG_even <- 1
              break
            }
          }
          
          count <- count+1
          trace <- st+5*(count+1)-1
          if((trace*scale>=1)) NG_even <- 1
        }
        
        
        #奇数ルート
        count <- 1 #試行回数
        trace <- st+9 #距離
        while(NG_odd==0){
          for(k in 1:trace){
            if(k<=count){
              if(k==1){ child <- nodes[i,direction[6]] } 
              else { child <- nodes[child,direction[6]] }
            }
            if(k==(count+1)) child <- nodes[child,direction[5]]
            if(k>=(count+2) && k<=(count*2+2)) child <- nodes[child,direction[3]]
            if(k>=(count*2+3) && k<=(count*3+2+st)) child <- nodes[child,direction[1]]
            if(k>=(count*3+3+st) && k<=(count*4+3+st)) child <- nodes[child,direction[2]]
            if(k==(count*4+4+st)) child <- nodes[child,direction[4]]
            if(k>=(count*4+5+st)) child <- nodes[child,direction[6]]
            if(k==trace){
              distance[[i,j]] <- c(distance[[i,j]],trace) #距離の格納
              distance[[j,i]] <- c(distance[[j,i]],trace) #距離の格納
            }
            if(child==0){ #ノードが存在しない場合
              NG_odd <- 1
              break
            }
          }
          
          count <- count+1
          trace <- st+5*(count+1)-1
          if((trace*scale>=1)) NG_odd <- 1
        }
        
      }
    } #パターンA
    
    #直線距離の計算
    st <- ((node_position[i,1]-node_position[j,1])^2+(node_position[i,2]-node_position[j,2])^2)^(1/2) #直線距離
    
    
    if(pattern[i,j]==0 && (st*scale)<1){ #パターンB(直線上にない)
      
      #ノード間の角度angle(-pi<angle<pi)の計算
      if(node_position[i,1]==node_position[j,1] && node_position[i,2]<node_position[j,2]) angle <- pi/2
      if(node_position[i,1]==node_position[j,1] && node_position[i,2]>node_position[j,2]) angle <- -pi/2
      if(node_position[i,1]!=node_position[j,1]){
        angle <- atan((node_position[j,2]-node_position[i,2])/(node_position[j,1]-node_position[i,1]))
        if(node_position[i,1]>node_position[j,1] && node_position[i,2]<node_position[j,2]) angle <- angle+pi #第２象限
        if(node_position[i,1]>node_position[j,1] && node_position[i,2]>node_position[j,2]) angle <- angle-pi #第３象限
      }
      
      
      #経路方向づけ
      #最短経路中のリンク数算出
      #正弦定理(1辺と２角)から他の２辺leの長さを求める
      if(angle>0 && angle<pi/3){
        direction <- c(1,2,6,3,5,2)
        le <- c(round(st*sin((pi/3)-angle)/(sqrt(3)/2)), round(st*sin(angle)/(sqrt(3)/2)))
      } else if(angle>pi/3 && angle<pi*2/3){
        direction <- c(2,3,1,4,6,5)
        le <- c(round(st*sin((pi*2/3)-angle)/(sqrt(3)/2)), round(st*sin(angle-pi/3)/(sqrt(3)/2)))
      } else if(angle>pi*2/3 && angle<pi){
        direction <- c(3,4,2,5,1,6)
        le <- c(round(st*sin(pi-angle)/(sqrt(3)/2)), round(st*sin(angle-(pi*2/3))/(sqrt(3)/2)))
      } else if(angle>(-pi/3) && angle<0){
        direction <- c(6,1,5,2,4,3)
        le <- c(round(st*sin(abs(angle))/(sqrt(3)/2)), round(st*sin((pi/3)-abs(angle))/(sqrt(3)/2)))
      } else if(angle>(-pi*2/3) && angle<(-pi/3)){
        direction <- c(5,6,4,1,3,2)
        le <- c(round(st*sin(abs(angle)-pi/3)/(sqrt(3)/2)), round(st*sin((pi*2/3)-abs(angle))/(sqrt(3)/2)))
      } else if(angle>(-pi) && angle<(-pi*2/3)){
        direction <- c(4,5,3,6,2,1)
        le <- c(round(st*sin(abs(angle)-(pi*2/3))/(sqrt(3)/2)), round(st*sin(pi-abs(angle))/(sqrt(3)/2)))
      }
      
      #第１・第２ルート
      if(sum(le)*scale<1){ #最短距離が1未満の場合
        distance[[i,j]] <- c(distance[[i,j]],sum(le),sum(le))
        distance[[j,i]] <- c(distance[[j,i]],sum(le),sum(le))
        
      } else { #距離が1より大きい場合
        NG_odd  <- 1
        NG_even <- 1
      }
      
      #第３ルート
      if((sum(le)+2)*scale<1){ #距離が1未満の場合
        for(k in 1:(sum(le)+2)){ #ルート探索
          if(k==1){
            child <- nodes[i,direction[3]]
          } else if(k<=(1+le[1])){
            child <- nodes[child,direction[1]]
          } else if(k<=(1+sum(le))){
            child <- nodes[child,direction[2]]
          } else if(k==(sum(le)+2)){
            child <- nodes[child,direction[4]]
          }
          
          if(child==0){
            NG_odd <- 1
            break
          }
        }
        
        if(NG_odd==0){  #第3ルートの格納
          distance[[i,j]] <- c(distance[[i,j]],(sum(le)+2))
          distance[[j,i]] <- c(distance[[j,i]],(sum(le)+2))
        }
        
      } else { #距離が1より大きい場合
        NG_odd  <- 1
      }
      
      #第４ルート
      if((sum(le)+2)*scale<1){ #距離が1未満の場合
        for(k in 1:(sum(le)+2)){ #ルート探索
          if(k==1){
            child <- nodes[i,direction[4]]
          } else if(k<=(1+le[2])){
            child <- nodes[child,direction[2]]
          } else if(k<=(1+sum(le))){
            child <- nodes[child,direction[1]]
          } else if(k==(sum(le)+2)){
            child <- nodes[child,direction[3]]
          }
          
          if(child==0){
            NG_even <- 1
            break
          }
        }
        
        if(NG_even==0){ #第４ルートの格納
          distance[[i,j]] <- c(distance[[i,j]],(sum(le)+2))
          distance[[j,i]] <- c(distance[[j,i]],(sum(le)+2))
        }
        
      } else { #距離が1より大きい場合
        NG_even <- 1
      }
      
      #第５ルート以降の奇数ルート
      if((sum(le)+6)*scale<1){
        count <- 1 #試行回数
        trace <- sum(le)+6 #距離
        
        while(NG_odd==0){
          for(k in 1:trace){
            if(k<=count){
              if(k==1){ child <- nodes[i,direction[5]] } 
              else { child <- nodes[child,direction[5]] }
            } else if(k==(count+1)){ child <- nodes[child,direction[3]]
            } else if(k<=(le[1]+count*2+1)){ child <- nodes[child,direction[1]]
            } else if(k<=(sum(le)+count*3+1)){ child <- nodes[child,direction[2]]
            } else if(k==(sum(le)+count*3+2)){ child <- nodes[child,direction[4]]
            } else if(k<=(sum(le)+count*4+2)){ child <- nodes[child,direction[6]]
            }
            if(k==trace){
              distance[[i,j]] <- c(distance[[i,j]],trace) #距離の格納
              distance[[j,i]] <- c(distance[[j,i]],trace) #距離の格納
            }
            if(child==0){ #ノードが存在しない場合
              NG_odd <- 1
              break
            }
          }
          
          count <- count+1
          trace <- sum(le)+4*(count+1)-2
          if((trace*scale>=1)) NG_odd <- 1
        }
      }
      
      #第６ルート以降の偶数ルート
      if((sum(le)+6)*scale<1){
        count <- 1 #試行回数
        trace <- sum(le)+6 #距離
        
        while(NG_even==0){
          for(k in 1:trace){
            if(k<=count){
              if(k==1){ child <- nodes[i,direction[6]] } 
              else { child <- nodes[child,direction[6]] }
            } else if(k==(count+1)){ child <- nodes[child,direction[4]]
            } else if(k<=(le[2]+count*2+1)){ child <- nodes[child,direction[2]]
            } else if(k<=(sum(le)+count*3+1)){ child <- nodes[child,direction[1]]
            } else if(k==(sum(le)+count*3+2)){ child <- nodes[child,direction[3]]
            } else if(k<=(sum(le)+count*4+2)){ child <- nodes[child,direction[5]]
            }
            if(k==trace){
              distance[[i,j]] <- c(distance[[i,j]],trace) #距離の格納
              distance[[j,i]] <- c(distance[[j,i]],trace) #距離の格納
            }
            if(child==0){ #ノードが存在しない場合
              NG_even <- 1
              break
            }
          }
          
          count <- count+1
          trace <- sum(le)+4*(count+1)-2
          if((trace*scale>=1)) NG_even <- 1
        }
      }
      
    } #パターンB
    
  } #ノードjについて
} #ノードiについて
###ノード間距離算出#####################################


###振動子モデルの適用###################################
#固有振動数
omega <- numeric(node_num) 
omega <- omega+OMEGA

#初期位相
phase <- matrix(0, nrow=(time+1), ncol=node_num)
set.seed(29)
phase[1,] <- runif(node_num, min=0, max=(2*pi))

#角振動数保存
phase_speed <- matrix(0, nrow=time, ncol=node_num)

#距離を振動モデル用に縮尺する
for(i in 1:node_num){
  for(j in 1:node_num){
    distance[[i,j]] <- distance[[i,j]]*scale
  }
}

#結合強度関数
G_function <- function(x){
  path_count <- 0 #式中のMにあたる
  G_sum <- 0
  
  if(length(x)>0){
    for(i in 1:length(x)){
      if(x[i]>0 && x[i]<1){
        G_sum <- G_sum + K/2*( A*(cos(2*pi*x[i])-cos(2*pi))+cos(pi*x[i])-cos(pi) )
        path_count <- path_count+1
      }
    }
    return(G_sum/path_count)
  } else { return(0)}
}

#結合強度
G <- matrix(nrow=node_num, ncol=node_num)
N <- numeric(node_num)
for(i in 1: node_num){
  for(j in 1:node_num){
    G[i,j] <- G_function(distance[[i,j]])
    if(G[i,j]!=0) N[i] <- N[i]+1
  }
}

#角速度計算関数
f_function <- function(t, num){
  sigma <- 0
  for(i in 1:node_num){
    if(i != num){
      sigma <- sigma + G[num,i]*sin(phase[t,num]-phase[t,i]+alpha)
    }
  }
  return(omega[j]-sigma/N[i])
}


###プログレスバーの設定###
pb <- txtProgressBar(min=1, max=time, style=3)

#角速度・位相計算
for(i in 1:time){
  for(j in 1:node_num){
    phase_speed[i,j] <- f_function(i,j)
    phase[(i+1),j] <- phase[i,j]+phase_speed[i,j]*dt
    #θ[(i+1),j] <- θ[(i+1),j]%%(2*pi)
  }
  
  ###プログレスバーの表示###
  setTxtProgressBar(pb, i)
}
########################################################


###ファイル出力（位相・振動数）#########################
file_name <- sprintf("./size%d/Phase.csv",size)
write.csv(phase, file = file_name, row.names = F)
file_name <- sprintf("./size%d/Speed.csv",size)
write.csv(phase_speed, file = file_name, row.names = F)
########################################################