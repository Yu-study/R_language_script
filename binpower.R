binpower<-function(){
  n<-20
  k1<-11
  k2<-12
  p0<-0.7
  x<-seq(0.4,1,0.1)
  pow1<-pbinom(k1,n,x)
  pow2<-pbinom(k2,n,x)
  #par(mfrow=c(2,2))
  postscript(file = "figbino.ps")
  plot(x,pow2,xlab = "p",ylab = "power",ylim = c(0,1),xlim = c(0.35,1),
       type="l",lty=2)
  lines(x,pow1,lty=1)
  text(0.72,0.4,"Level 0.23")
  text(0.54,0.4,"Level 0.11")
}
  