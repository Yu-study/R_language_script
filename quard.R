#program 二次函数求根公式
#函数形式:a2*x^2+a1*x+a0=0
quad<-function(a0,a1,a2){
  if(a2==0&&a1==0&&a0==0)
  {roots<-NA}
  else if (a2==0&&a1==0){
    roots<-NULL
  }else if (a2==0){
    roots<- -a0/a1
  }else{
    #calculate the discriminant
    discrim<- a1^2-4*a2*a0
    #calculate the roots depending on the value of the discriminant
    if(discrim>0){
     roots<- (-a1+c(1,-1)*sqrt(a1^2-4*a2*a0)/(2*a2)) 
    }else if (discrim==0){
      roots<- -a1/(2*a2)
    }else{
      roots<-NULL
    }
  }
  return(roots)
}