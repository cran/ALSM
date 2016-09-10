#' @export
#' @importFrom graphics lines par plot
#' @importFrom stats anova deviance df.residual lm median predict pt qf qt var
#' @importFrom leaps leaps


ci.reg<-function(model, newdata, type = c("b", "s","w","n","m","nm","gn"), alpha = 0.05,m=1){

  type<-match.arg(type)

  newdata<-as.data.frame(newdata)
  if ( dim(newdata)[2] == length(names(model$coeff))   ){
    colnames(newdata)<-names(model$coeff)
  }else{
    colnames(newdata)<-names(model$coeff)[-1]

  }


  CI<-predict(model, newdata, se.fit = T)



  g<-nrow(newdata)
  p<-ncol(newdata)+1
  syh<-CI$se.fit
  spred<-sqrt( CI$residual.scale^2 + (CI$se.fit)^2 )  #     (2.38)
  spredmean<-sqrt( (CI$residual.scale^2)/m + (CI$se.fit)^2 )

  b<-qt(1 - alpha/(2*g), model$df)          # B = (4.9a)
  s<-sqrt(g * qf(1 - alpha, g, model$df))     # S = (4.8a)
  w<-sqrt(p*qf(1 - alpha, p, model$df))
  if (match.arg(type) == "b") {
    s<-syh
    z<-b
  }else if(match.arg(type) == "s") {
    s<-spred
    z<-s
  }else if(match.arg(type) == "w") {
    s<-syh
    z<-w
  }else if(match.arg(type) == "n") {
    s<-spred
    z<-qt(1 - alpha/2, model$df)
  }else if(match.arg(type) == "nm") {
    s<-spredmean
    z<-qt(1 - alpha/2, model$df)
  }else if(match.arg(type) == "m") {
    s<-syh
    z<-qt(1 - alpha/2, model$df)
  }else if(match.arg(type) == "gn") {
    s<-spred
    z<-b}


  x<-data.frame(newdata,Fit=CI$fit,Lower.Band=CI$fit-z*s,Upper.Band=CI$fit+z*s)

  return(x)
}
