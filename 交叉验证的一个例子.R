ww<-read.table("C://Users/Administrator/Desktop/Dermatology.txt",header = FALSE,sep=",")
ww[,35]=factor(ww[,35])
n=nrow(ww);
T=length(table(ww[,35]));
Z=10;
# above n is the number of sample,T is the class of indepenednt variables,z is zheshu
d=1:n;dd=list();e=names(table(ww$V35))
#每个dd[[i]]是i类下标集
for(i in 1:T) dd[[i]]=d[ww$V35==e[i]]  
#下面每个kk[i]是i类中每折的数目：
kk=NULL;for(i in 1:T) kk=c(kk,round(length(dd[[i]])/Z))
set.seed(111);yy=list(NULL,NULL,NULL,NULL,NULL,NULL)
for(i in 1:T)
  {xx=list();uu=dd[[i]];
   for(j in 1:9){ xx[[j]]=sample(uu,kk[i])
   uu=setdiff(uu,xx[[j]])};xx[[10]]=uu
  for(k in 1:10)yy[[j]][[k]]=xx[[k]]}
mm=list(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL)
for(i in 1:Z)for(j in 1:T)mm[[i]]=c(mm[[i]],yy[[j]][[i]])
#mm[[i]]是第i折的测试集下标集合

library(MASS)
E1=rep(0,Z);for(i in 1:Z){m=mm[[i]];
  n1<-length(m);a=lda(V35~.,ww[-m,])
  E1[i]=sum(ww[m,35]!=predict(a,ww[m,])$class)/n1};
mean(E1)

