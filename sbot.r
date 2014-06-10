

find_check = function(cnf){
  check=NULL
  m=matrix(cnf,3,3)
  v=apply(m,1,sum)
  w=apply(m,2,sum)
  s=m[1,1]+m[2,2]+m[3,3]
  t=m[1,3]+m[2,2]+m[3,1]
  chk=c(v,w,s,t)
  
  if (max(abs(chk))==2){
    p=which(abs(chk)==2)
    for (pp in p){
      if (pp<=3){
        v=c(1,4,7)+(pp-1)
        check=c(check,v[which(cnf[v]==0)])
        next
      }
      if ((pp<=6) && (pp>=4)){
        v=c(1,2,3)+3*(pp-4)
        check=c(check,v[which(cnf[v]==0)])
        next
      }
      if (pp==7){
        v=c(1,5,9)
        check=c(check,v[which(cnf[v]==0)])
        next
      }
      if (pp==8){
        v=c(3,5,7)
        check=c(check,v[which(cnf[v]==0)])
        next
      }
    }
  }
  
  return(check)
}

find_chance = function(cnf){
  check=NULL
  check1=NULL
  m=matrix(cnf,3,3)
  v=apply(m,1,sum)
  w=apply(m,2,sum)
  s=m[1,1]+m[2,2]+m[3,3]
  t=m[1,3]+m[2,2]+m[3,1]
  chk=c(v,w,s,t)
  
  ma=matrix(abs(cnf),3,3)
  va=apply(ma,1,sum)
  wa=apply(ma,2,sum)
  sa=ma[1,1]+ma[2,2]+ma[3,3]
  ta=ma[1,3]+ma[2,2]+ma[3,1]
  chka=c(va,wa,sa,ta)
  
  if (length(which(chka==1))>0){
    p=which((chka==1) && (chk==turn))
    for (pp in p){
      if (pp<=3){
        v=c(1,4,7)+(pp-1)
        check=c(check,v[which(cnf[v]==0)])
        next
      }
      if ((pp<=6) && (pp>=4)){
        v=c(1,2,3)+3*(pp-4)
        check=c(check,v[which(cnf[v]==0)])
        next
      }
      if (pp==7){
        v=c(1,5,9)
        check=c(check,v[which(cnf[v]==0)])
        next
      }
      if (pp==8){
        v=c(3,5,7)
        check=c(check,v[which(cnf[v]==0)])
        next
      }
    }
  }
  
  if ((length(which(chka==1))>0)){
    p=which(chka==1)
    for (pp in p){
      if (pp<=3){
        v=c(1,4,7)+(pp-1)
        check=c(check,v[which(cnf[v]==0)])
        next
      }
      if ((pp<=6) && (pp>=4)){
        v=c(1,2,3)+3*(pp-4)
        check=c(check,v[which(cnf[v]==0)])
        next
      }
      if (pp==7){
        v=c(1,5,9)
        check=c(check,v[which(cnf[v]==0)])
        next
      }
      if (pp==8){
        v=c(3,5,7)
        check=c(check,v[which(cnf[v]==0)])
        next
      }
    }
  }
  
  
  return(check)
}

sfindmove = function(cnf){
  m=NA
  ck=NULL
  ch=NULL
  if (length(which(cnf==0))==1){
    m=which(cnf==0)
  } else if (length(find_check(cnf))>0) {
    ck=find_check(cnf)
    m=ssample(ck)
    
  } else if (length(find_chance(cnf))>0) {
    ch=find_chance(cnf)
    m=ssample(ch)
    
  } else{
    m=sample(which(cnf==0),1)
  }
  
  return(m)

}


sbotmove = function(show=TRUE){
  v=rep(NA,9)
  
  if (conf[82]==0 && sum(abs(conf1))!=9){
  
    g=sfindmove(conf1)  
    
    if (sum(abs(getgames(conf)[[g]]))==9){
      
      games=getgames(conf)
      
      for (i in 1:9){
        v[i]=sum(abs(games[[i]]))
      }
      
      vv=which(v<9)
      
      g=ssample(vv)
        
    }
    
  } else if (conf[82]==0 && sum(abs(conf1))==9) {
    games=getgames(conf)
      
    for (i in 1:9){
      v[i]=sum(abs(games[[i]]))
    }
    
    vv=which(v<9)
    g=ssample(vv)

  } else {
    g=conf[82]
  }
  
  game=getgames(conf)[[g]]
  
  mv=sfindmove(game)
  
  if (show==TRUE){
    #print(paste(ck,ch))
  }
  
  m=mv+(g-1)*9
  return(m)
}

 