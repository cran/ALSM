#' @export

BestSub<-function(x,y,method=c('r2','r2adj','sse','cp','press','aic','sbc'),num=2){

  method<-match.arg(method)
  x<-as.matrix(x)
  y<-as.matrix(y)

  m<-model.s(x,y)


if (match.arg(method) == "r2") {
  q<-dim(m)[2]-5


  n<-cbind(1:dim(m)[1],m[,1],m[,q])[order(m[,q],decreasing = T),]
  g<-unlist(tapply(n[,1],n[,2],function(x) x[1:num]))
  p<-rep(2:max(m[,1]),each=num)[!is.na(g)]
  g<-g[!is.na(g)]
  return(cbind(p,m[g,][,-1]))

}else if(match.arg(method) == "r2adj") {
  q<-dim(m)[2]-4


  n<-cbind(1:dim(m)[1],m[,1],m[,q])[order(m[,q],decreasing = T),]
  g<-unlist(tapply(n[,1],n[,2],function(x) x[1:num]))
  p<-rep(2:max(m[,1]),each=num)[!is.na(g)]
  g<-g[!is.na(g)]
  return(cbind(p,m[g,][,-1]))

}else if(match.arg(method) == "sse") {
  q<-dim(m)[2]-6
}else if(match.arg(method) == "cp") {
  q<-dim(m)[2]-3
}else if(match.arg(method) == "aic") {
  q<-dim(m)[2]-2
}else if(match.arg(method) == "sbc") {
  q<-dim(m)[2]-1
}else if(match.arg(method) == "press") {
  q<-dim(m)[2]-0
}

  n<-cbind(1:dim(m)[1],m[,1],m[,q])[order(m[,q],decreasing = F),]
  g<-unlist(tapply(n[,1],n[,2],function(x) x[1:num]))
  p<-rep(2:max(m[,1]),each=num)[!is.na(g)]
  g<-g[!is.na(g)]
  return(cbind(p,m[g,][,-1]))

}


