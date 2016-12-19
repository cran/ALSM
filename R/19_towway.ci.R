#' @export




towway.ci<-function(y,x1,x2,mc=NULL,mp=NULL,mt=NULL,mse=NULL,interact=T,alpha=0.05){





    out.f.a<-NULL
  out.f.b<-NULL
  out.c.a<-NULL
  out.t.a<-NULL
  out.t.b<-NULL
  out.bt.a<-NULL
  out.bt.b<-NULL
  out.s.a<-NULL
  out.sb.a<-NULL
  out.tr.sim.b<-NULL
  out.i.b<-NULL
  out.i.t<-NULL
  out.i.c<-NULL
  out.i.s.s<-NULL
  out.i.s.b<-NULL

  #simultaneous<-match.arg(simultaneous)
 # interact<-match.arg(interact)


  fit <- lm(y ~ factor(x1)*factor(x2))######!!!!!
  if (   is.null(mse)    ){
    mse<-(deviance(fit))/(fit$df.residual)
  }

  a<-length(table(x1))
  b<-length(table(x2))
  ab<-c(a,b)

  mean.a<-tapply(y,x1,mean)
  mean.b<-tapply(y,x2,mean)
  m<-list(mean.a,mean.b)
  rv.a<-as.integer(names(mean.a))
  rv.b<-as.integer(names(mean.b))

  n<-sum(x1==rv.a[1] & x2==rv.b[1]   )

  ci.a<-c()
  cj.a<-c()

  for (i in 1:(a-1)){
    ii<-i+1
    for ( j in ii:a){
      ci.a<-c(ci.a,rv.a[i])
      cj.a<-c(cj.a,rv.a[j])
    }
  }
  rn.a<-paste0(ci.a,'-' ,cj.a)

  ci.b<-c()
  cj.b<-c()

  for (i in 1:(b-1)){
    ii<-i+1
    for ( j in ii:b){
      ci.b<-c(ci.b,rv.b[i])
      cj.b<-c(cj.b,rv.b[j])
    }
  }
  rn.b<-paste0(ci.b,'-' ,cj.b)


if (interact==F){################################## intreact ==F
  ############################# factor level mean
  s.a<-sqrt(mse/(b*n))
  s.b<-sqrt(mse/(a*n))

  row.a<-as.integer(names(mean.a))
  row.b<-as.integer(names(mean.b))

  out.f.a<-cbind(level=row.a,lower=mean.a-qt(1-alpha/2,(n-1)*a*b)*s.a,upper=mean.a+qt(1-alpha/2,(n-1)*a*b)*s.a)
  out.f.b<-cbind(level=row.b,lower=mean.b-qt(1-alpha/2,(n-1)*a*b)*s.b,upper=mean.b+qt(1-alpha/2,(n-1)*a*b)*s.b)
  ############################ tukey a
  d<-mean.a[ci.a]-mean.a[cj.a]
  t<-(qtukey(1-alpha,a,(n-1)*a*b))/sqrt(2)
  s<-sqrt(  (2*mse)/(b*n)   )
  q<-(sqrt(2)*d)/s
  #pv<-round(2*(1-ptukey(abs(q),a,(n-1)*a*b)),5)
  out.t.a<-cbind(d,lower=d-t*s,upper=d+t*s,q)
  row.names(out.t.a)<-rn.a

  ############################ tukey b
  d<-mean.b[ci.b]-mean.b[cj.b]
  t<-(qtukey(1-alpha,b,(n-1)*a*b))/sqrt(2)
  s<-sqrt(  (2*mse)/(a*n)   )
  q<-(sqrt(2)*d)/s
  #pv<-round(2*(1-ptukey(abs(q),b,(n-1)*a*b)),5)
  out.t.b<-cbind(d,lower=d-t*s,upper=d+t*s,q)
  row.names(out.t.b)<-rn.b
  ################################ BON tow a

  g<-choose(a,2) ###!!!!!!!!!

  d<-mean.a[ci.a]-mean.a[cj.a]
  t<-qt(1-alpha/(2*g),(n-1)*a*b)
  s<-sqrt(  (2*mse)/(b*n)   )
  q<-(d)/s
  pv<-round(2*(1-pt(abs(q) ,(n-1)*a*b )),5)
  out.bt.a<-cbind(d,lower=d-t*s,upper=d+t*s,q,pv)
  row.names(out.bt.a)<-rn.a
  ################################ BON tow b
  g<-choose(b,2) ###!!!!!!!!!

  d<-mean.b[ci.b]-mean.b[cj.b]
  t<-qt(1-(alpha/(2*g)),(n-1)*a*b)
  s<-sqrt(  (2*mse)/(a*n)   )
  q<-(d)/s
  pv<-round(2*(1-pt(abs(q) ,(n-1)*a*b ) ),5)
  out.bt.b<-cbind(d,lower=d-t*s,upper=d+t*s,q,pv)
  row.names(out.bt.b)<-rn.b





    if(   !is.null(mt)){#####################teratment T sim bon
      qq<-c()
      g<-dim(mt)[1]
      for(i in 1:g){
      qq<-c(qq, mean( y[x1==mt[i,1] & x2==mt[i,2] ]    )  )
      }
      se<-qt(1-alpha/(2*g),(n-1)*a*b)

      out.tr.sim.b<-cbind(mean=qq,lower=qq-se*sqrt(mse/n) ,upper=qq+se*sqrt(mse/n) )
      row.names(out.tr.sim.b)<-paste0(mt[,1],' & ',mt[,2])




}


  if (!is.null(mc)){#################### if is null mc
    z<-dim(mc)[1]
    c(1:z)[-c(1:(z/3)*3)]



    qq<-colSums(t(mc[c(1:z)[-c(1:(z/3)*3)],]))!=0
    qqr<-rep(1:2,length(qq)/2)[qq]
    qqn<-rep(1:2,length(qq)/2)[!qq]
    mc<-mc[  c(1:z)[colSums(t(mc)==0)==0]  ,]


 ################################### schefe
    out.s.a<-NULL

    out.s.a<-matrix(1,1,5)

    for( i in 1:c(z/3)){
      l<-sum(mc[i*2,]*(m[[qqr[i]]][mc[i*2-1,]]))
      s.a<-sqrt(   (mse/(ab[qqn[i]]*n))*(sum(mc[i*2,]^2))       )
      s<-sqrt(  (ab[qqr[i]]-1)*(qf(1-alpha,ab[qqr[i]]-1,(n-1)*a*b)) )
      q<-(l^2)/((ab[qqr[i]]-1)*(s.a^2))
      pv<-round(1-pf(q,ab[qqr[i]]-1,(n-1)*a*b),5)
      out<-cbind(num=i,lower=l-s*s.a,upper=l+s*s.a,q,pv)
      out.s.a<-rbind(out.s.a,out)
    }
    out.s.a<-out.s.a[-1,]
    ###############################BON
    g<-z/3
    out.sb.a<-matrix(1,1,4)

    for( i in 1:c(z/3)){
      l<-sum(mc[i*2,]*(m[[qqr[i]]][mc[i*2-1,]]))
      s.a<-sqrt(   (mse/(ab[qqn[i]]*n))*(sum(mc[i*2,]^2))       )
      s<-qt(1-alpha/(2*g),(n-1)*a*b)
      q<-l/s.a
      pv<-round(2*(1-pt(q,(n-1)*a*b)),5)
      out<-cbind(num=i,L=l,lower=l-s*s.a,upper=l+s*s.a)
      out.sb.a<-rbind(out.sb.a,out)
    }
    out.sb.a<-out.sb.a[-1,]
    ############################## contrast
    out.c.a<-NULL
    out.c.a<-matrix(1,1,4)

    for( i in 1:c(z/3)){
      l<-sum(mc[i*2,]*(m[[qqr[i]]][mc[i*2-1,]]))
      s.ab<-sqrt(   (mse/(ab[qqn[i]]*n))*(sum(mc[i*2,]^2))       )
      out<-cbind(num=i,L=l,lower=l-qt(1-alpha/2,(n-1)*a*b)*s.ab,upper=l+qt(1-alpha/2,(n-1)*a*b)*s.ab)
      out.c.a<-rbind(out.c.a,out)
    }
    out.c.a<-out.c.a[-1,]

}#### end if !is.null(mc)

  o<-list(factor.level.mean.factor.1=out.f.a,factor.level.mean.factor.2=out.f.b,contrast.NOT.simultaneous=out.c.a,tukey.pairwise.factor.1=out.t.a,tukey.pairwise.factor.2=out.t.b,BON.tow.factor.1=out.bt.a,BON.tow.factor.2=out.bt.b,schefe=out.s.a,bonferroni=out.sb.a,     treatment.Means.Simultaneous.Bonferroni=out.tr.sim.b)
  return(o)

}else{ ################################################# intreact ==T sim f

  if(!is.null(mp)){
   ##################### INTER tukey
  out.i.b<-NULL
  out.i.t<-NULL


    rn<-paste0(mp[,1],'-',mp[,2],"  ",mp[,3],'-',mp[,4])
    d<-c()
    for ( i in 1:dim(mp)[1]){
      d<-c(d,mean(y[x1==mp[,1][i] & x2==mp[,2][i]   ]) -mean( y[x1==mp[,3][i] & x2==mp[,4][i]   ]     ))

    }

    s<-sqrt((mse*2)/n)
    t<-(qtukey(1-alpha,a*b,(n-1)*a*b))/sqrt(2)
    q<-(sqrt(2)*d)/s
    pv<-round(2*(1-ptukey(abs(q),a*b,(n-1)*a*b)),5)
    out.i.t<-cbind(d,d-t*s,d+t*s,q,pv)
    row.names(out.i.t)<-rn
    ##################### INTER bon
    g<-dim(mp)[1]
    t<-qt(1-alpha/(2*g),(n-1)*a*b)
    q<-(d)/s
    pv<-round(2*(1-pt( abs(q) ,(n-1)*a*b )),5)
    out.i.b<-cbind(d,d-t*s,d+t*s,q,pv)

    row.names(out.i.b)<-rn
} ##### end if!is.null(mp)
  if (!is.null(mc)){

  ################################################ schefffe
    ll<-c()
    se<-c()
    for (i in 1:(dim(mc)[1]/3)  ){
    l<-c()

    for ( j in 1:dim(mc)[2]){
    l<-sum(  l, mc[i*3,j]*mean(y[x1==mc[i*3-2,j] & x2==mc[i*3-1,j]   ])    )

    }
  ll<-c(ll,l)
  se<-c(se,sqrt( (mse/n)*sum(mc[  i*3 , ]^2)   ))
  }
    S<-sqrt( (a*b-1)*qf(1-alpha,a*b-1,(n-1)*a*b)  )

  out.i.s.s<-cbind(L=ll,lower=ll-S*se,uper=ll+S*se)

  ###########################not simul
  S<-qt(1-alpha/2,(n-1)*a*b)

  out.i.c<-cbind(L=ll,lower=ll-S*se,uper=ll+S*se)

  ######################################## bon
  g<-dim(mc)[1]/3
  S<-qt(1-alpha/(2*g),(n-1)*a*b )
  #S<-sqrt( (a*b-1)*qf(1-alpha,a*b-1,(n-1)*a*b)  )
  #se<-sqrt( (mse/n)*(apply(mc[(1:(dim(mc)[1]/3))*3,],1,function(xx)sum(xx^2)))  )
  out.i.s.b<-cbind(L=ll,lower=ll-S*se,upper=ll+S*se)


} ##### end if !is.null(mc)

      o<-list(tukey.pairwise.Simultaneos=out.i.b,bonferroni.pairwise.Simultaneos=out.i.t,contrast.NOT.simultaneous=out.i.c,sheffe.simultaneous.contrast=out.i.s.s,bonferroni.simultaneous.contrast=out.i.s.b)

    return(o)
    }

}########################################################### end function

