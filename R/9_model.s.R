#' @export
#' @import leaps


model.s<-function(x,y){
  n<-ncol(x)
x<-as.matrix(x)
y<-as.vector(y)
  xaic<-c()
  xsbc<-c()
  xpress<-c()
  xsse<-c()
  c<-leaps(x,y)$which
  for (i in 1:nrow(c)){
    model<-lm(y~  x[,(1:n)[c[i,]==T]]  )
    smodel<-summary(model)


    xaic<-c(xaic,AICp(model))
    xsbc<-c(xsbc,SBCp(model))
    xpress<-c(xpress,pressc(model))
    xsse<-c(xsse,deviance(model))
#    xcp<-c(xcp, cpc(model) )


  }
  m1 <- leaps(x,y,method='Cp')
  m2 <- leaps(x,y,method='r2')
  m3 <- leaps(x,y,method='adjr2')

  cbind(p=m1$size,m1$which,SSEp=xsse,r2=m2$r2,r2.adj=m3$adjr2,Cp=m1$Cp,AICp=xaic,SBCp=xsbc,PRESSp=xpress)

}
