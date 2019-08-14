runstest<-function(y,cut=0){
  if(cut!=0)x<-(y>cut)*1 else x<-y; 
  N=length(x);  k<-1
  for(i in 1:(N-1)) if(x[i]!=x[i+1]) k=k+1;
  r=k; m=sum(1-x);n=N-m
  P1=function(m,n,k)
  {2*choose(m-1,k-1)/choose(m+n,n)*choose(n-1,k-1)}
  P2=function(m,n,k)
  {choose(m-1,k-1)*choose(n-1,k)/choose(m+n,n)+
    choose(m-1,k)*choose(n-1,k-1)/choose(m+n,n)}
  r2=floor(r/2);if(r2==r/2){pv=0;for(i in 1:r2) pv=pv+P1(m,n,i)
  for(i in 1:(r2-1)) pv=pv+P2(m,n,i)}else
  {pv=0;for(i in 1:r2) pv=pv+P1(m,n,i);
  for(i in 1:r2) pv=pv+P2(m,n,i)}  
  if(r2==r/2)pv1=1-pv+P1(m,n,r2) else pv1=1-pv+P2(m,n,r2)
  tpv=min(pv,pv1)*2
  list(Exact.pvalue=min(pv,pv1),Exact.2sided.pvalue=tpv)
}
