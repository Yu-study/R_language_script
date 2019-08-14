# This program is based on Accept-Reject Generate Algorithm 
# This program is used to generate the N(0,1) distribution
# f(x) is a standard normal distribution
# g(x) is a cauchy distribution
Norrandar<-function(n){
  Norran<-rep(0,n)
  M<-sqrt(2*pi)*exp(-0.5)
  for(i in 1:n){
    t<-0
    U<-1
    while(t<=U){
    U<-runif(1)
    Y<-rcauchy(1)
    t<-dnorm(Y)/M*dcauchy(Y) 
    }
    Norran[i]<-Y
  }
  Norran
}