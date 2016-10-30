#' @export

bftest<-function(fit,group,alpha=.05){

  f<-fit$fitted
  e<-fit$res
  e1<-e[group==unique(group)[1]]
  e2<-e[group==unique(group)[2]]
  d1<-abs(e1-median(e1))
  d2=abs(e2-median(e2))
  n1<-length(e1)
  n2<-length(e2)
  n<-length(group)################# deghat, barasi, neveshtan stop

  s=sqrt( (  (n1-1)*var(d1)+   (n2-1)*var(d2)   )/(n-2))
  t=(mean(d1)-mean(d2))/(s*sqrt((1/n1)+(1/n2))   )

  out<-cbind(t.value=abs(t),P.Value=2*(1-pt(abs(t),n-2)),alpha=alpha,df=(n-2))###????
  return(out)
}
