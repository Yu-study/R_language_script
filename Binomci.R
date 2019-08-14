#This function is used to get a confidence interval of Bernouli Proportion
#s is a value(not a vector) which is the T- or T
#n is number of observations. If length(n) > 1, the length is taken to be the number required.
#theta1 and theta2 are the two values and we need use bisection algorgetithm to 
# get real theta
#value is the goal of alpha, but rember it is a two_tail and we need to make aplha=alpha1+aplh2
# finally thanks for the "Introduction to Mathematical Statistics" version 7 Page 645

binomci<-function(s,n,theta1,theta2,value,maxstp=100,eps=0.00001){
  y1=pbinom(s,n,theta1)
  y2=pbinom(s,n,theta2)
  ic1=0
  ic2=0
  if(y1>=value){ic1=1}
  if(y2<=value){ic2=1}
  if((ic1*ic2)>0){
    istep=0
    while(istep<maxstp){
      istep=istep+1
      theta3=(theta1+theta2)/2
      y3=pbinom(s,n,theta3)
      if(y3>value){
        theta1=theta3
        y1=y3
      }else{
         theta2=theta3
         y2=y3
      }
      if(abs(theta1-theta2)<eps){istep=maxstp}
    }
    list(solution=theta3,valatsol=y3)
  }else{
    list(error="Bad Starts")
  }
}