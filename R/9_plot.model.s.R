#' @export

plotmodel.s<-function(x,y){
  n<-ncol(x)
x<-as.matrix(x)
y<-as.vector(y)

  xaic<-c()
  xsbc<-c()
  xpress<-c()

  c<-leaps(x,y)$which
  for (i in 1:nrow(c)){
    model<-lm(y~  x[,(1:n)[c[i,]==T]]  )
    #smodel<-summary(model)

    xaic<-c(xaic,AICp(model))
    xsbc<-c(xsbc,SBCp(model))
    xpress<-c(xpress,pressc(model))
  }
  m1 <- leaps(x,y,method='Cp')
  m2 <- leaps(x,y,method='r2')
  m3 <- leaps(x,y,method='adjr2')

    p<-m1$size
  #cbind(p=m1$size,m1$which,r2=m2$r2,r2.adj=m3$adjr2,Cp=m1$Cp,AICp=xaic,SBCp=xsbc,PRESSp=xpress)
#par(mfrow=c(3,2))

    par(mfrow=c(1,1))

e<-max(p)
  plot(m2$r2 ~ p,    xlab = "p", ylab = "R2",main='plot 1 of 6')
  lines(2:e, tapply(m2$r2, p, max), lwd = 2,col=2)

  plot(m3$adjr2 ~ p,    xlab = "p", ylab = "R2adj",main='plot 2 of 6')
  lines(2:e, tapply(m3$adjr2, p, max), lwd = 2,col=2)

  plot(xaic ~ p,    xlab = "p", ylab = "AIC",main='plot 3 of 6')
  lines(2:e, tapply(xaic, p, min), lwd = 2,col=2)

  plot(xsbc ~ p,    xlab = "p", ylab = "SBC",main='plot 4 of 6')
  lines(2:e, tapply(xsbc, p, min), lwd = 2,col=2)

  plot(m1$Cp ~ p,    xlab = "p", ylab = "Cp",main='plot 5 of 6')
  lines(2:e, tapply(m1$Cp, p, min), lwd = 2,col=2)

  plot(xpress ~ p,    xlab = "p", ylab = "PRESS",main='plot 6 of 6')
  lines(2:e, tapply(xpress, p, min), lwd = 2,col=2)
#par(mfrow=c(1,1))
  }
