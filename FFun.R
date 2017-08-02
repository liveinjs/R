#  用来将n 个周期的成交量合并为一个周期。
#  参数v,成交量数据向量集合
#  参数n,n个周期合为一个周期

cvt<-function(v,n){
  l<-length(v)
  m<-floor(l/n)
  o<-l%%n
  
  p<-seq(1:m)
  q<-sapply(p,function(x){return(sum(v[((n-1)*x):(x*n)]))})
  return(c(q,sum(tail(v,o))))
}
