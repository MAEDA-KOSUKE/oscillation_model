###パッケージのセット(tcltk)############################
targetPackages <- c('tcltk') 
newPackages <- targetPackages[!(targetPackages %in% installed.packages()[,"Package"])]
if(length(newPackages)) install.packages(newPackages, repos = "http://cran.us.r-project.org")
for(package in targetPackages) library(package, character.only = T)
########################################################



###ネットワークの行列表現###############################
node_num <- 3*(2^size-1)+1 #ノード総数
connect <- matrix(nrow=node_num, ncol=3) #接続するノードの通し番号を格納
connect[1,] <- c(2,3,4)
connect[2:4,1] <- c(1,1,1)

if(size>=2){
  for(i in 2:node_num){
    connect[i,2:3] <- c((i*2+1),(i*2+2))
  }
  
  for(i in 2:(3*(2^(size-1)-1)+1)){
    connect[(i*2+1):(i*2+2),1] <- c(i,i)
  }
}
########################################################

###ノード間距離算出#####################################
distance <- matrix(list(), nrow=node_num, ncol=node_num) #距離を格納

#ノードiとノードj間の距離(リンク数)を算出する
for(i in 1:node_num){
  for(j in i:node_num){
    
    count <- 0
    mark1 <- i
    mark2 <- j
    
    while(mark1!=mark2){
      if(mark1>mark2){
        mark1 <- connect[mark1,1]
        count <- count+1
      }
      if(mark1<mark2){
        mark2 <- connect[mark2,1]
        count <- count+1
      }
    }
    
    distance[[i,j]] <- count
    distance[[j,i]] <- count
    
  }
}
########################################################

#source("振動子モデル適用.R")
###振動子モデルの適用###################################
#固有振動数
omega <- numeric(node_num) 
omega <- omega+OMEGA

#初期位相(θでは文字化けするためphaseとする)
phase <- matrix(0, nrow=(time+1), ncol=node_num)
set.seed(523)
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
    if(path_count==0){ return(0) }
    else { return(G_sum/path_count) }
    
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
  return(omega[j]-sigma/(node_num-1))
}


###プログレスバーの設定###
pb <- txtProgressBar(min=1, max=time, style=3)

#角速度・位相計算
for(i in 1:time){
  for(j in 1:node_num){
    phase_speed[i,j] <- f_function(i,j)
    phase[(i+1),j] <- phase[i,j]+phase_speed[i,j]*dt
    #phase[(i+1),j] <- phase[(i+1),j]%%(2*pi)
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