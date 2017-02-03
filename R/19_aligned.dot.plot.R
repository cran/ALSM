#' @export

aligned.dot.plot2<-function(y,factor1,factor2=NULL){
  factor1<-as.numeric(factor1)

#factor2<-match.arg(factor2)
  if (!is.null(factor2)){
    factor2<-as.numeric(factor2)
    factor1<-as.numeric(factor1)
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
  abline(h=1:c(length(table(factor1))*length(table(factor2))), lty = 2,col='gray75')

  s=1
  for(i in ufactor1){
    for(j in ufactor2){
      yy=y[factor1==ufactor1[i] & factor2==ufactor2[j]]
      points(yy,rep(s,length(yy)))
      s=s+1
    }
  }
}else{#######################################
  factor1=as.numeric(factor1)
  ufactor1<-unique(factor1)
  ci<-c()
    for(i in ufactor1){

      ci<-c(ci,ufactor1[i])


  }
  rn<-paste0(ci)

  yy=y[factor1==ufactor1[1] ]
  plot(yy,rep(1,length(yy)) ,yaxt='n',col='white', xlab=c('') , ylim =c(0,length(table(factor1))+.5  ),xlim=range(y),ylab=c('treatment') )
  axis(2,1:c(length(table(factor1)) ) , labels = rn)
  abline(h=1:c(length(table(factor1))), lty = 2,col='gray75')

  s=1
  for(i in ufactor1){

      yy=y[factor1==ufactor1[i] ]
      points(yy,rep(s,length(yy)))
      s=s+1

  }
}


}
