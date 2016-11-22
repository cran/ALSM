#' @export

############ you can add mse log lik ---------
boxcox.sse<-function(x,y,l=seq(-2,2,.1)){

  s<-c()
  l<-l[l!=0]
  k2=(prod(y))^(1/length(y))
for(j in 1:length(l)){
      k1=1/(  (l[j])*(k2^(l[j]-1))   )
      w=k1*((y^l[j]) -1)
      s<-c(s,deviance(lm(w~x)))
}

w=k2*log(y)
s0<-c(deviance(lm(w~x)))
s<-c(s,s0)
l<-c(l,0)
out<-data.frame(lambda=l,SSE=s)
out<-out[order(l),]
plot(out,ylab = 'SSE',xlab=expression(lambda),type='l')

return(out)
}
