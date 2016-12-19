#' @export

aligned.dot.plot2<-function(y,factor1,factor2){


  ufactor1<-unique(factor1)
  ufactor2<-unique(factor2)

  ci<-c()
  cj<-c()

  for(i in ufactor1){
    for(j in ufactor2){
      ci<-c(ci,ufactor1[i])
      cj<-c(cj,ufactor2[j])
    }
  }
  rn<-paste0(ci,' - ' ,cj)

  yy=y[factor1==ufactor1[1] & factor2==ufactor2[1]]
  plot(yy,rep(1,length(yy)) ,yaxt='n',col='white', xlab=c('') , ylim =c(0,length(table(factor1))*length(table(factor2))),xlim=range(y),ylab=c('treatment') )
  axis(2,1:c(length(table(factor1))*length(table(factor2)) ) , labels = rn)
  abline(h=1:c(length(table(factor1))*length(table(factor2))), lty = 2)

  s=1
  for(i in ufactor1){
    for(j in ufactor2){
      yy=y[factor1==ufactor1[i] & factor2==ufactor2[j]]
      points(yy,rep(s,length(yy)))
      s=s+1
    }
  }



}
