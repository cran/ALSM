#' @export
#'
cpc<- function(r,f){
  n <- sum(anova(f)[,1])+1
  p <- n-df.residual(r)
  #print(n)
  #print(p)
  return((deviance(r)/(deviance(f)/df.residual(f)))-(n-2*p))

}
