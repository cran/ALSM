#' @export

twoway.ci<-function(y,x1,x2,mc=NULL,mp=NULL,mt=NULL,mse=NULL,alpha=0.05){

  if (1==1 ){
  a<-length(table(x1))
  b<-length(table(x2))
  ab<-c(a,b)

  mean.a<-tapply(y,x1,mean)
  mean.b<-tapply(y,x2,mean)
  m<-list(mean.a,mean.b)
  rv.a<-as.integer(names(mean.a))
  rv.b<-as.integer(names(mean.b))

  n<-aggregate(y,list(x1,x2),length)[,3]

  m.n<-tapply(y,list(x1,x2),length)
  m.mean<-tapply(y,list(x1,x2),mean)

  ma<-apply(m.mean,1,mean)
  mb<-apply(m.mean,2,mean)



  out.t.c<-NULL
  out.f.a<-NULL
  out.f.b<-NULL
  out.bt.a<-NULL
  out.bt.b<-NULL
  out.c.a<-NULL
  out.s.a<-NULL
  out.sb.a<-NULL
  out.t<-NULL
  out.t.s<-NULL
  out.t.sb<-NULL
  out.tpb<-NULL
  out.tpt<-NULL


    ################## chekkkkkkkkk
  if (sum(n==1)==length(y) )   {
    fit <- lm(y ~ factor(x1)+factor(x2))######!!!!!
    df.er=(a-1)*(b-1)
  }else{
   fit <- lm(y ~ factor(x1)*factor(x2))######!!!!!
    df.er=length(y)-a*b
  }
##################### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 # if (   is.null(mse)    ){
    mse<-(deviance(fit))/(fit$df.residual)
  #}



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

###############

  T.a<-(qtukey(1-alpha,a,df.er))/sqrt(2)
  T.b<-(qtukey(1-alpha,b,df.er))/sqrt(2)

  t.a<-qt(1-alpha/2,df.er)
  t.b<-qt(1-alpha/2,df.er)
}


 ############################################ factor level
  row.a<-as.integer(names(mean.a))
  row.b<-as.integer(names(mean.b))

  out.f.a<-matrix(1,1,3)
  out.f.b<-matrix(1,1,3)
    for ( i in 1:a){

      s.a<-sqrt((mse*(apply(1/m.n,1,sum)[i])   )/(b^2))

      out<-cbind(level=row.a[i],lower=ma[i]-t.a*s.a,upper=ma[i]+t.a*s.a)
      out.f.a<-rbind(out.f.a,out)

    }
  out.f.a<- out.f.a[-1,]

  for ( i in 1:b){
      s.b<-sqrt((mse*(apply(1/m.n,2,sum)[i])   )/(a^2))
      out<-cbind(level=row.b[i],lower=mb[i]-t.b*s.b,upper=mb[i]+t.b*s.b)
      out.f.b<-rbind(out.f.b,out)
    }

  out.f.b<- out.f.b[-1,]

    ############################ tukey a
    d.a<-ma[ci.a]-ma[cj.a]
    s.a<-c()
    for ( i in 1:length(ci.a)){
      ss<-sum(apply(1/m.n,1,function(x) sum(x)  )[c(ci.a[i],cj.a[i])])
        s.a<-c(s.a,sqrt(   (mse*ss)/(b^2)    )      )
    }
    out.t.a<-cbind(d.a,lower=d.a-T.a*s.a,upper=d.a+T.a*s.a)
    row.names(out.t.a)<-rn.a

    d25=out.t.a
    qwe<-dim(d25)[1]

    plot(-10,-10,ylim=c(0,dim(d25)[1]+.5),yaxt='n',xlim=range(d25[,2:3]),xlab='',ylab='',main=paste("CI Tukey factor x1")) ####### -10 -10 !!!!!!
    axis(2,1:qwe,labels=row.names(d25))
    arrows(d25[,2],1:qwe,d25[,3],1:qwe,code=3,angle = 90,length = .1)


    ############################ tukey b
    d.b<-mb[ci.b]-mb[cj.b]
    s.b<-c()
    for ( i in 1:length(ci.b)){
      ss<-sum(apply(1/m.n,2,sum)[c(ci.b[i],cj.b[i])]  )
        s.b<-c(s.b,sqrt((mse*ss)/(a^2))      )
    }
    out.t.b<-cbind(d.b,lower=d.b-T.b*s.b,upper=d.b+T.b*s.b)
    row.names(out.t.b)<-rn.b

    d25=out.t.b
    qwe<-dim(d25)[1]

    plot(-10,-10,ylim=c(0,dim(d25)[1]+.5),yaxt='n',xlim=range(d25[,2:3]),xlab='',ylab='',main=paste("CI Tukey factor x2")) ####### -10 -10 !!!!!!
    axis(2,1:qwe,labels=row.names(d25))
    arrows(d25[,2],1:qwe,d25[,3],1:qwe,code=3,angle = 90,length = .1)

    ################################ BON tow a
    g<-choose(a,2) ###!!!!!!!!!
    t<-qt(1-alpha/(2*g),df.er)
    out.bt.a<-cbind(d.a,lower=d.a-t*s.a,upper=d.a+t*s.a)
    row.names(out.bt.a)<-rn.a
    ################################ BON tow b
    g<-choose(b,2) ###!!!!!!!!!
    t<-qt(1-alpha/(2*g),df.er)
    out.bt.b<-cbind(d.b,lower=d.b-t*s.b,upper=d.b+t*s.b)
    row.names(out.bt.b)<-rn.b
    if (!is.null(mc)){#################### if is null mc
      mm<-list(ma,mb)
      z<-dim(mc)[1]

      ss<-apply(mc!=0,1,any)
      zz<-c(1:z)[-c(1:(z/3)*3)]
      qq<-ss[zz]

      e<-rep(1:2,length=length(qq))[qq]

      mc2<-mc[ ss,]

       z<-dim(mc2)[1]

  if ( !sum(qq)==length(zz) ) {
        ############################## contrast
    z<-dim(mc2)[1]
    e<-rep(1:2,length=length(qq))[qq]
    t<-qt(1-alpha/2,df.er)

    g<-z/2
    tb<-qt(1-alpha/(2*g),df.er)

        out.c.a<-matrix(1,1,4)
        out.s.a<-matrix(1,1,4)

        out.sb.a<-matrix(1,1,4)

        for( i in 1:c(z/2)){

          l<-sum(   (mm[[e[i]]][mc2[i*2-1,]])*(mc2[i*2,])      )

          ss<-sum(    (mc2[i*2,]^2)*(apply(1/m.n,e[i],sum)[mc2[i*2-1,] ] )  )
          s.a<-sqrt(   (mse/(rev(ab)[e[i]]^2))*ss  )

          s<-sqrt((ab[e[i]]-1)*qf(1-alpha,ab[e[i]]-1,df.er ) )

          out<-cbind(num=i,L=l,lower=l-t*s.a,upper=l+t*s.a)
          out.c.a<-rbind(out.c.a,out)

          out<-cbind(num=i,L=l,lower=l-s*s.a,upper=l+s*s.a)
          out.s.a<-rbind(out.s.a,out)

          out<-cbind(num=i,L=l,lower=l-tb*s.a,upper=l+tb*s.a)
          out.sb.a<-rbind(out.sb.a,out)

          }
        out.c.a<-out.c.a[-1,]
        out.s.a<-out.s.a[-1,]
        out.sb.a<-out.sb.a[-1,]


    }else{
      z<-dim(mc)[1]
      out.t.s<-matrix(1,1,4)
      out.t.c<-matrix(1,1,4)
      out.t.sb<-matrix(1,1,4)

      s<-sqrt(   (a*b -1)*qf(1-alpha,a*b-1,df.er)   )
     g<-z/3
      t<-qt(1-alpha/(2*g),df.er)
      ttt<-qt(1-alpha/(2),df.er)
      for ( i in 1:(z/3)){
l<-sum(apply(mc[c(i*3-2):c(i*3),],2,function(x) sum(m.mean[x[1],x[2]]*x[3])   ))
se2<-mse*sum(apply(mc[c(i*3-2):c(i*3),],2,function(x) sum((x[3]^2)/m.n[x[1],x[2]]) ))
se<-sqrt(se2)

out<-cbind(num=i,L=l,lower=l-s*se,upper=l+s*se)
out.t.s<-rbind(out.t.s,out)

out<-cbind(num=i,L=l,lower=l-ttt*se,upper=l+ttt*se)
out.t.c<-rbind(out.t.c,out)

out<-cbind(num=i,L=l,lower=l-t*se,upper=l+t*se)
out.t.sb<-rbind(out.t.sb,out)
}
      out.t.s<-out.t.s[-1,]
      out.t.sb<-out.t.sb[-1,]
      out.t.c<-out.t.c[-1,]
      } ### treat
    } ### end mc


    if(   !is.null(mt)){#####################teratment BON
      out.t<-matrix(1,ncol=5)
      z<-dim(mt)[1]
      for ( i in 1:z){
        s<-sqrt(mse/(m.n[  mt[i,1],  mt[i,2]]))
        t<-qt(1-alpha/(2*z),df.er)
        e<-m.mean[  mt[i,1],  mt[i,2]   ]
        out<-cbind(i=mt[i,1],j=mt[i,2],estimate=e,lower= e-t*s , upper= e+t*s )
        out.t<-rbind(out.t,out)
        }
      out.t<-out.t[-1,]
      }

    if(   !is.null(mp)){#####################teratment
      out.tpb<-matrix(1,ncol=3)
      out.tpt<-matrix(1,ncol=3)
      z<-dim(mp)[1]
      Tu<-(qtukey(1-alpha,a*b,df.er))/sqrt(2)
      tb<-qt(1-alpha/(2*z),df.er)

      for ( i in 1:z){
        s<-sqrt(mse*(  1/(m.n[  mp[i,1],  mp[i,2] ])+1/(m.n[  mp[i,3],  mp[i,4] ])))

        e<- (m.mean[  mp[i,1],  mp[i,2]]-m.mean[  mp[i,3],  mp[i,4]] )

        out<-cbind(estimate=e,lower= e-tb*s , upper= e+tb*s )
        out.tpb<-rbind(out.tpb,out)

        out<-cbind(estimate=e,lower= e-Tu*s , upper= e+Tu*s )
        out.tpt<-rbind(out.tpt,out)
      }
      out.tpb<-out.tpb[-1,]
      out.tpt<-out.tpt[-1,]
    }
    o<-list( factor.a=out.f.a,
             factor.b=out.f.b,

             bonferroni.simultaneous.pairwise.a=out.bt.a,
             bonferroni.simultaneous.pairwise.b=out.bt.b,

             tukey.simultaneous.pairwise.a=out.t.a,
             tukey.simultaneous.pairwise.b=out.t.b,

             NOT.simultaneous.Contrast=out.c.a,
             Sheffe.simultaneous.Contrast=out.s.a,
             bonferroni.simultaneous.Contrast=out.sb.a,

             treatment.Bonferroni=out.t,

             bonferroni.simultaneous.pairwise.treatment=out.tpb,
             tukey.simultaneous.pairwise.treatment=out.tpt,

             NOT.simultaneous.Contrast.treatment=out.t.c,
             Sheffe.simultaneous.Contrast.treatment=out.t.s,
             bonferroni.simultaneous.Contrast.treatment=out.t.sb)
    return(o)
}########################################################### end function

