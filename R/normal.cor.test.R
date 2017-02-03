#' @export

normal.cor.test<-function(residuals,MSE){
   w<-1:length(residuals)
   r<-cor(sort(residuals), sqrt(MSE)*(qnorm((w-0.375)/(length(residuals)+.25))) )
   return(r)
}
