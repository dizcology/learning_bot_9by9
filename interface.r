
hplay = function(r,c,g=conf[82]){
  if (conf[82]==0 && g==0){
    print ("must specify which game to play: hplay(r,c,g)")
    print(status())
    return(FALSE)
  }
  play(r+(c-1)*3+(g-1)*9)
}


ox = function(v){
  w=rep(NA,length(v))
  for (i in 1:length(v)){
    if (v[i]==1){
      w[i]="O"
    } else if (v[i]==-1){
      w[i]="X"
    } else {
      w[i]=" "
    }
    
  }

  return(w)
}

board = function(){
    
  #m=matrix(ox(conf),3,3)
  n=matrix(ox(conf[1:81]),9,9)
  a=rep(list(NA),9)
  for (i in 1:9){
    a[[i]]=matrix(n[,i],3,3)
  }
  m=cbind(rbind(a[[1]],"-",a[[2]],"-",a[[3]]),"|",rbind(a[[4]],"-",a[[5]],"-",a[[6]]),"|",rbind(a[[7]],"-",a[[8]],"-",a[[9]]))
  
  rownames(m)=c(1:3,"",1:3,"",1:3)
  colnames(m)=c(1:3,"",1:3,"",1:3)
  return(as.table(m))

}

status = function(){
  print(paste("current turn:",ox(turn)))
  r=ifelse((conf[82]%%3==0 && conf[82]!=0),3,conf[82]%%3)
  c=ceiling(conf[82]/3)
  print(paste("current game:",paste0(r,","),c))
  return(board())
}

getgame = function(i){
  n=matrix(conf[1:81],9,9)
  return(n[,i])

}
