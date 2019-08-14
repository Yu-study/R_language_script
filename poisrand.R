#This program is used to generate sample 
# whose distribution is possion distribution
#code comes from introduction to Mathematical Statistic
#principle:introduction to Mathematical Statistic Page 164
poisrand=function(n,lamada){
#
#  n is the number of simulations
#  lamada is the mean of the Possion distribution
#
  poisrand=rep(0,n)
  for(i in 1:n){
    xt=0
    t=0
    while(t<1){
      x=xt
      y=-(1/lamada)*log(1-runif(1))
      t=t+y
      xt=xt+1
    }
    poisrand[i]=x
  }
  poisrand
}