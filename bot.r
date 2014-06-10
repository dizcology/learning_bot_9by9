

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
    
    t=observed(c(records[k,]$conf,k-1), lmat)
    
    if (t!=0){
      m=hstrat[records[k,]$move]
      lmat[t,m] <- lmat[t,m]+wt*((-1)^(k+1))*winner

    } else {
    
      temp <- rbind(temp,c(records[k,]$conf,k-1,rep(0,81)))
      colnames(temp)=c(1:83,hstrat)
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
  
  t=observed(c(conf,taken(conf)), learn_matrix)
    
    if (t!=0){
      v=learn_matrix[t,84:164]
      mx=max(v[w])
      ww=intersect(which(v==mx),w)
      
      m=ssample(ww)

    } else {
    
      m=ssample(w)

    }
  
  
  if (show==TRUE){
    print(search.lm())
  }
  return(m)
}

search.lm = function(cnf=conf){ 
  r=NA
  
  t=observed(c(cnf,taken(cnf)),learn_matrix)
  
  if (t!=0){
    b=getgames(learn_matrix[t,84:164])
    a=rep(list(NA),9)
    for (i in 1:9){
      a[[i]]=matrix(b[[i]],3,3)
    }
    r=cbind(rbind(a[[1]],"-",a[[2]],"-",a[[3]]),"|",rbind(a[[4]],"-",a[[5]],"-",a[[6]]),"|",rbind(a[[7]],"-",a[[8]],"-",a[[9]]))
       
    rownames(r)=c(1:3,"",1:3,"",1:3)
    colnames(r)=c(1:3,"",1:3,"",1:3)

  }
  
  
  res=ifelse(is.na(r),NA,as.table(r))
  return(res)
}

write.lm = function(lmat=learn_matrix,file="learn_matrix.dat"){
  write.table(lmat,file=file,row.names=F,col.names=F)
}


read.lm = function(file="learn_matrix.dat"){  
  lmat=as.matrix(read.table(file=file))
  colnames(lmat)=c(1:83,hstrat)

  rownames(lmat)=NULL
  return(lmat)
}



stat.lm = function(lmat=learn_matrix){
  print (paste0("Configurations observed: ",dim(lmat)[1]))
  mm=lmat[1:dim(lmat)[1],84:164]
  idx=sum(abs(mm))
  
  print (paste0("Learn index: ",idx))
  return()
}


observed = function(cnf, lmat=learn_matrix){
  tt=0
  ww=0
  k=cnf[83]
  w=which(lmat[,83]==k)
  
  for (ww in w){
    if (all(cnf[1:82]==lmat[ww,1:82])){
      tt=ww
      break
    }
  }
  
  return(tt)

}



