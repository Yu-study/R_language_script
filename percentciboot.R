#this program is used to obtain a percentile confidence for the mean
#to change this to a parameter other than the mean, simply substitute the
#approprite function at both calls to the mean
percentciboot<-function(x,b,alpha){
  #x is a vector containing the orginal sample
  #b is the desired number of bootstraps
  #alpha: (1- alpha) is the confidence coefficient
  #
  #theta is the point estimate.
  #lower is the lower end of the percentile confidence interval
  #upper is the upper end of the percentile confidence intervalthe
  #thetastar is the vector of bootstraped theta^*s
  #
  theta<-mean(x)
  thetastar<-rep(0,b)
  n<-length(x)
  for(i in 1:n){xstar<-sample(x,n,replace = TRUE)
  thetastar[i]<-mean(xstar)  }
  thetastar<-sort(thetastar)
  pick<-round((alpha/2)*(b+1))
  lower<-thetastar[pick]
  upper<-thetastar[b-pick+1]
  #list(theta=theta,lower=lower,upper=upper,thetastar=thetastar)
  list(theta=theta,lower=lower,upper=upper)
}
