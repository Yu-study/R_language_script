empalphacn<-function(nsims){
#
# Obtains the empirical level of the test discussed
# in Introduction to Mathematical Statistics Example 4.8.6
# 
# nisms is the number of simulations
#
  sigmac<-25
  eps<-0.25
  alpha<-.05
  n<-20
  tc<-qt(1-alpha,n-1)
  ic<-0
  for(i in 1:nsims){
    samp<-rcn(n,eps,sigmac)
    ttest<-(sqrt(n)*mean(samp))/var(samp)^.5
    if(ttest>tc){ic<-ic+1}
  }
  empalp<-ic/nsims
  err<-1.96*sqrt((empalp*(1-empalp))/nsims)
  list(No.Simulat=nsims,empiricalalpha=empalp,error=err,CIof0.95=c(empalp-1.96*err,empalp+1.96^err))
}

rcn<-function(n,eps,sigmac){
#
# return a random sample of size n drawn from
# a contaminated normal distribution with percent
# contamination eps and variance ration sigmac
#
  ind<-rbinom(n,1,eps)
  x<-rnorm(n)
  rcn<-x*(1-ind)+sigmac*x*ind
  rcn
}

rscn<-function(n,eps,sd,mu){
#
# returns a random sample of size n drawn from
# a skewed contaminated normal districution with percent
# contaminated eps, variance ratio sd, and mean mu
# 
  x1<-rnorm(n)
  x2<-rnorm(n,mu,sd)
  b1<-rbinom(n,1,eps)
  rscn<-x1*(1-b1)+b1*x2
  rscn
}


