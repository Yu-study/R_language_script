#program 排列问题 从N个元素中选出r个有多少种选法
 
n_factortial<-function(n){
  # Calculate n facrortial
  n_fact<-prod(1:n)
  return(n_fact)
}

n_choose_r <- function(n,r){
  # Calculate n choose r
  n_ch_r<-n-factorial(n)/n_factorial(r)/N-factorial(n-r)
  return(n_ch_r)
}