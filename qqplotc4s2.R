qqplotc4s2<-function(){
  data.1<-matrix(scan("c4s2.dat"),ncol = 1,byrow = TRUE)
  vec<-data.1[,1]
  n<-length(vec)
  ps<-(1:n)/(n+1)
  normalqs<-qnorm(ps)
  y<-sort(vec)
  #postscript("try.ps",horizontial=TRUE)
  par(mfrow=c(2,2))
  boxplot(y,ylab="x")
  title(main = "Panel A")
  plot(normalqs,y,xlab = "Normal quantiles",ylab = "Sample quantiles")
  title(main = "Panel B",xlab = "Normal quantiles",ylab = "Sample quantiles")
  plot(qlaplace(ps),y,xlab = "Laplace quantiles",ylab = "Sample quantiles")
  title(main = "Planel C")
  plot(qexp(ps),y,xlab = "Exponential quantiles",ylab = "Sample quantiles")
  title(main = "Panel D")
}
  qlaplace<-function(ps){
  low<-ps[ps<0.5]
  high<-ps[ps>=0.5]
  lowq<-log(2*low)
  highq<-log(2*(1-high))
  qlaplace<-c(lowq,highq)
}