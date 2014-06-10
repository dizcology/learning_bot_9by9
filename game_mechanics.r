
play = function(m,show=TRUE){
  if(!(m%in%strat) || conf[m]!=0 || (conf[82]!=0 && ceiling(m/9)!=conf[82])){
    print("illegal move")
    if (show==TRUE){
      print(status())
    }
    return(FALSE)
  }
  
  if (judge(conf1)$finished==TRUE){
    print("game already finished")
    if (show==TRUE){
      print(status())
    }
    return(FALSE)
  }
  
  rcd <<- rbind(rcd,list(conf=conf,move=m))
  conf[m] <<- conf[m]+turn
  
  conf[82] <<- ifelse(m%%9==0,9,m%%9)
  conf[82] <<- ifelse(sum(abs(getgames(conf)[[conf[82]]]))==9,0,conf[82])

  a=getgames(conf)
  for (i in 1:9){
    conf1[i] <<- judge(a[[i]])$winner
  }
  
  turn <<- (-1)*turn
  if (show==TRUE){
    print(status())
  }
  return(TRUE)
}



judge = function(cnf){ 
  m=matrix(cnf,3,3)
  v=apply(m,1,sum)
  w=apply(m,2,sum)
  s=m[1,1]+m[2,2]+m[3,3]
  t=m[1,3]+m[2,2]+m[3,1]
  chk=c(v,w,s,t)
  
  winner=0
  finished=NA
  
  if (max(abs(chk))==3){
    
    finished=TRUE
    if (max(chk)==3){
      winner=1
    } else {
      winner=-1
    }
  } else {
    finished=FALSE
  }
  
  return(list(winner=winner,finished=finished))
}

reset = function(){
  conf <<- rep(0,82)
  conf1 <<- rep(0,9)
  turn <<- 1
  rcd <<- NULL

}


generate = function(show=TRUE, show_each=FALSE, players=c("s","s")){ #TODO
  reset()
  winner=0
  fnshed=FALSE
  
  v=list()
  for (i in 1:length(players)){
    if (players[i]=="m"){
      v[[i]]=botmove 
    } else {
      v[[i]]=sbotmove
    }
  }
  
  while ((fnshed==FALSE) && (sum(abs(conf[1:81]))<81)){
    j=(3-turn)/2
    
    m=v[[j]](show=FALSE)
    
    play(m, show=show_each)
    jdg=judge(conf1)
    fnshed=jdg$finished
    winner=jdg$winner
    
    if (sum(abs(conf[1:81]))==81){
      fnshed=TRUE
    }
  }
  
  if (show==TRUE){
    print(status())
  }
  return(list(records=rcd,winner=winner))
}


random.conf = function(){
  
  cnf=sample(c(-1,0,1),81,replace=T)
  g=sample(1:9,1)
  return(c(cnf,g))
}

getgames = function(cnf){
  n=matrix(cnf[1:81],9,9)
  a=rep(list(NA),9)
  for (i in 1:9){
    a[[i]]=matrix(n[,i],3,3)
  }

  return(a)
}

ssample = function(v){
  m=NA
  if (length(v)==1){
    m=v
  } else {
    m=sample(v,1)
  }
  return(m)

}

taken = function(v){
  return(sum(abs(v)))

}
