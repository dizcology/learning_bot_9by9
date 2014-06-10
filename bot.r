

train = function(n=50,lmat=learn_matrix, players=c("s","s"),cut=200){
  reset()
  count=0 #to avoid unending loop!
  win_count=rep(0,3)
  names(win_count)=c(paste0(players,1:2),"tie")
  
  for(i in 1:n){
    winner=0
    while (winner==0){
      game=generate(show=FALSE, players=players)
      
      count=count+1
      if (count>cut){
        print (paste0(cut," games played"))
        print (paste0(i," games without tie"))
        reset()
        return(lmat)
      }
        
      winner=game$winner
      if (winner==0){
        win_count[3]=win_count[3]+1
      }
    }
    win_count[(3-winner)/2]=win_count[(3-winner)/2]+1
    lmat=learn(game=game, lmat=lmat)
  }
  
  print(win_count)
  reset()
  return(lmat)
}

learn = function(game=list(records=rcd,winner=judge(conf1)$winner), lmat=learn_matrix, wt=1){

  temp=NULL  
  records=game$records
  winner=game$winner
  
  for (k in 1:dim(records)[1]){
      
    match=0  
    for (t in 1:dim(lmat)[1]){
      if (all(records[k,]$conf==lmat[t,1:82])){
        m=hstrat[records[k,]$move]
        lmat[t,m] <- lmat[t,m]+wt*((-1)^(k+1))*winner
        match=1
        break
      } 
      
    }
    
    if (match==0){
      
      temp <- rbind(temp,c(records[k,]$conf,rep(0,81)))
      colnames(temp)=c(1:82,hstrat)
      m=hstrat[records[k,]$move]
      s=dim(temp)[1]
      temp[s,m] <- temp[s,m]+wt*((-1)^(k+1))*winner      
    }
    
  }
  
  reset()

  return(rbind(lmat,temp))
}

botmove = function(show=TRUE){  #TODO
  m=NA
  
  g=conf[82]
  
  if (g!=0){
    w=intersect(which(conf[1:81]==0),1:9+(g-1)*9)
  } else {
    w=which(conf[1:81]==0)
  }
  
  
  for (t in 1:dim(learn_matrix)[1]){
    if (all(conf==learn_matrix[t,1:82])){

      v=learn_matrix[t,83:163]
      mx=max(v[w])
      ww=intersect(which(v==mx),w)
      
      m=ssample(ww)
      
      #if (length(which(v0==max(v)))==0) {
      #  break
      #} else if (length(which(v0==max(v)))==1){
      #  m=which(v0==max(v))
      #} else{
      #  m=sample(which(v==max(v)),1)
      #}
      break
    } 
  }
  
  if(is.na(m)){
    
    m=ssample(w)
    
  }
  
  if (show==TRUE){
    print(search.lm())
  }
  return(m)
}

search.lm = function(c=conf){ 
  r=NA
  for (t in 1:dim(learn_matrix)[1]){
    if (all(conf==learn_matrix[t,1:82])){
      b=getgames(learn_matrix[t,83:163])
      a=rep(list(NA),9)
      for (i in 1:9){
        a[[i]]=matrix(b[[i]],3,3)
      }
      r=cbind(rbind(a[[1]],"-",a[[2]],"-",a[[3]]),"|",rbind(a[[4]],"-",a[[5]],"-",a[[6]]),"|",rbind(a[[7]],"-",a[[8]],"-",a[[9]]))
      
      #r=matrix(unlist(learn_matrix[t,2:82]),9,9)
      
      rownames(r)=c(1:3,"",1:3,"",1:3)
      colnames(r)=c(1:3,"",1:3,"",1:3)
      break
    } 
  }
  
  res=ifelse(is.na(r),NA,as.table(r))
  return(res)
}

write.lm = function(lmat=learn_matrix,file="learn_matrix.dat"){
  write.table(lmat,file=file,row.names=F,col.names=F)
}


read.lm = function(file="learn_matrix.dat"){  
  lmat=as.matrix(read.table(file=file))
  colnames(lmat)=c(1:82,hstrat)

  rownames(lmat)=NULL
  return(lmat)
}



stat.lm = function(lmat=learn_matrix){
  print (paste0("Configurations observed: ",dim(lmat)[1]))
  mm=lmat[1:dim(lmat)[1],83:163]
  idx=sum(abs(mm))
  
  print (paste0("Learn index: ",idx))
  
}
