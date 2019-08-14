#program 素数密度函数
#estimate the density of primes(using a very indfficient alforithm)

#clear the workspace
rm(list=ls())

prime<-function(n){
  #return TRUE if n is prime
  #assume n is a positive interger
  if(n==1){
    is.prime<-FALSE
  }else if(n==2){
    is.prime<-TRUE
  }else{
   is.prime<-TRUE
   m<-2
   m.max<-sqrt(n)#only want to calculate the once
   while( is.prime && m<=m.max){
    if(n%%m==0) is.prime<-FALSE
    m<-m+1
   }
  }
  return(is.prime)
}

#input
#we consider prime<=n
primedensity<-function(n){
  cat("运算时间可能较长，请耐心等待\n")
#calculate the number of primes<=m for m in 2:n
#num.prime[i]==number of primes<=i+1
m.vec<-2:n
primes<-sapply(m.vec,prime)
num.prime<-cumsum(primes)

#output
#plot the autual prime against the theoretial limit
par(mfrow=c(1,2))
plot(m.vec,num.prime/m.vec,type="l",
     main = "prime density",xlab = "n",ylab = "")
lines(m.vec,1/log(m.vec),col="red")

plot(m.vec,num.prime/m.vec*log(m.vec),type="l",
     main="prime density * log(n)",xlab="n",ylab = "")
par(mfrow=c(1,1))
}