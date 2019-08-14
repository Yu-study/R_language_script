#Because b=(t(X)%*%X)^(-1))%*%(t(x)%*%y), we have y^=X(t(X)%*%X)^(-1))%*%(t(x)%*%y)
#We define that H=X(t(X)%*%X)^(-1))%*%(t(x) is a hat matrix and Var(y^)=h_i_i*sigma^2
Reg_Diag<-function(fm){
  #cat("This function is used to check the outliers","\n")
  #cat("this function will output four statistics")
  #cat("hat_matrix will print h_i_i, generally we like h_i_i>=2(p+1)/n")
  #cat("DIFFITS will print D_i=sqrt(h_i_i/(1-h_i_i))*(epsilon^/(sigma*1-h_i_i)), generally we like D_i>=2(p+1)/n")
  #cat("Cooks will print D_i=(1/(p+1)*(h_i_i/(1-h_i_i))*r_i^2, generally we like D_i to be smaller")
  #cat("Cooks will print D_i=(1/(p+1)*(h_i_i/(1-h_i_i))*r_i^2, generally we like D_i to be smaller")
  n<-nrow(fm$model);df<-fm$df.residual
  p<-n-df-1;s<-rep("",n);
  res<-residuals(fm);s1<-s;s1[abs(res)==max(abs(res))]<-"*"
  sta<-rstandard(fm);s2<-s;s2[abs(sta)>2]<-"*"
  stu<-rstudent(fm);s3<-s;s3[abs(sta)>2]<-"*"
  h<-hatvalues(fm);s4<-s;s4[h>=2*(p+1)/n]<-"*"
  d<-dffits(fm);s5<-s;s5[abs(d)>2*sqrt((p+1)/n)]<-"*"
  c<-cooks.distance(fm);s6<-s;s6[c==max(c)]<-"*"
  co<-covratio(fm);abs_co<-abs(co-1)
  s7<-s;s7[abs_co==max(abs_co)]<-"*"
  data.frame(residual=res,s1,standard=sta,s2,
             student=stu,s3,hat_matrix=h,s4,
             DFFITS=d,s5,cooks_distance=c,s6,
             COVRATIO=co,s7)
}