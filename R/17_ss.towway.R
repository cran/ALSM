#' @export




ssea.oneway<-function(number.group,mc,sigma,n.weight,ci.width,type=c('s','b'), alpha=0.05){

  type<-match.arg(type)
k<-dim(mc)[1]
r<-number.group
v<-sigma^2
o<-0
n=2 ############## !!!!!!!!!!!

while(o!=1){
  #print(n)
  mn<-n*n.weight
    s<-c()
      for (i in 1:k){
        s<-c(s,  sqrt(     v*(  sum(    ((mc[i,])^2)/mn)  ) )     )
      }
    if (match.arg(type)=='b'){
      st<-qt(1 - alpha/(2*k),  sum(mn)-r)
    }else{
      st<-sqrt((r - 1)*qf(1 - alpha, r - 1,  sum(mn)-r))
    }
      if( sum((2*st*s)<=ci.width)==k ){
        o=1
      }else{
        n=n+1
      }
}
sample.size<-cbind(1:r,mn)
colnames(sample.size)<-c('treatment','n_i')
ci<-cbind(1:k,st*s*2,ci.width )
colnames(ci)<-c('Contrast','width of CI','expected CI')
out<-list(CI=ci,  n=sample.size )

return(out)
}



