#' @export
SBCp<- function(model){
  n <- sum(anova(model)[,1])+1
  p <- n-df.residual(model)
  #print(n)
  #print(p)
  return(n*log(deviance(model))-n*log(n)+(log(n))*p)

}
