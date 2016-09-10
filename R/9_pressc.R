#' @export
pressc<-function(fit){
  x<-cbind( rep(1,  nrow(as.matrix(fit$model[,-1])) ),fit$model[,-1] )
  x<-as.matrix(x)
  h<-x%*%solve(t(x)%*%x)%*%(t(x))
  e<-fit$residuals
  return(sum(    (e/(1-diag(h)))^2    ) )

}
