

train = function(n=50,mat=learn_matrix, players=c("s","s"),cut=200){
  reset()
  count=0 #to avoid unending loop!
  lmat=mat
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
    lmat=learn(game=game, mat=lmat)
  }
  
  print(win_count)
  reset()
  return(lmat)
}

learn = function(game=list(records=rcd,winner=judge(conf1)$winner), mat=learn_matrix, wt=1){
  lmat=mat
  temp=NULL
  
  records=game$records
  winner=game$winner
  
  for (k in 1:dim(records)[1]){
      
    match=0  
    for (t in 1:dim(lmat)[1]){
      if (all(records[k,]$conf==lmat[t,]$conf)){
        #m=as.character(records[k,]$move)
        m=hstrat[records[k,]$move]
        lmat[[t,m]] <- lmat[[t,m]]+wt*((-1)^(k+1))*winner
        match=1
        break
      } 
      
    }
    
    if (match==0){
      temp <- rbind(temp,matrix(c(list(conf=records[k,]$conf),rep(list(0),81)),1,82))
      colnames(temp)=c("conf",hstrat)
      #m=as.character(records[k,]$move)
      m=hstrat[records[k,]$move]
      s=dim(temp)[1]
      temp[[s,m]] <- temp[[s,m]]+wt*((-1)^(k+1))*winner      
    }
    
  }
  
  reset()

  return(rbind(lmat,temp))
}

botmove = function(show=TRUE){  #TODO
  m=NA
  
  for (t in 1:dim(learn_matrix)[1]){
    if (all(conf==learn_matrix[t,]$conf)){
      v=unlist(learn_matrix[t,2:10])[which(conf==0)]
      mm=as.numeric(names(v))
      if (length(which(v==max(v)))==0) {
        break
      } else if (length(which(v==max(v)))==1){
        m=mm[which(v==max(v))]
      } else{
        m=mm[sample(which(v==max(v)),1)]
      }
      break
    } 
  }
  
  if(is.na(m)){
    if (length(which(conf==0))==1){
      m=which(conf==0)
    } else{

      m=sample(which(conf==0),1)
    }
  }
  if (show==TRUE){
    print(search.lm())
  }
  return(m)
}

search.lm = function(c=conf){
  r=NA
  for (t in 1:dim(learn_matrix)[1]){
    if (all(conf==learn_matrix[t,]$conf)){
      r=matrix(unlist(learn_matrix[t,2:10]),3,3)
      rownames(r)=1:3
      colnames(r)=1:3
      break
    } 
  }
  
  return(r)
}

write.lm = function(lmat=learn_matrix,file="learn_matrix.dat"){
  write.table(t(apply(lmat,1,unlist)),file=file,row.names=F,col.names=F)
}

read.lm = function(file="learn_matrix.dat"){
  lmat=NULL
  tmp=read.table(file=file)
  colnames(tmp)=c(1:82,1:81)
  for (i in 1:dim(tmp)[1]){
    rw=c(list(conf=as.numeric(tmp[i,1:82])),as.list(tmp[i,83:163]))
    lmat=rbind(lmat,rw)
  }
  
  colnames(lmat)=c("conf",hstrat)
  rownames(lmat)=NULL
  return(lmat)
}

stat.lm = function(lmat=learn_matrix){
  print (paste0("Configurations observed: ",dim(lmat)[1]))
  mm=unlist(as.matrix(lmat[1:dim(lmat)[1],2:10]))
  idx=sum(abs(mm))
  
  print (paste0("Learn index: ",idx))
  #rng=range(lmat[2:dim(lmat)[1],2:10])
  #print (paste0("Range: ",rng[1],"~",rng[2]))
  
}
