#' @export

satterthwaite<-function(c,MSE,df,alpha=0.05){
  l=c*MSE
  L=sum(c*MSE)
  dff<-((L^2)/sum((l^2)/df))
  dff2<-round(dff)
  if (dff2==0) dff2=1

  lower<-(dff*L)/(qchisq(1-alpha/2,(dff2 )))
  upper<-(dff*L)/(qchisq(alpha/2,(dff2)))
  return(cbind(estimate=L,df=(dff),lower=lower,upper=upper))
}
