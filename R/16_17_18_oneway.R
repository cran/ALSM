#' @export
#'
#' @import SuppDists
#'
#'


oneway <- function(y,group,alpha=0.05,MSE=NULL,c.value=0,mc=NULL,residual=c('simple','semistudentized','studentized','studentized.deleted'),omission.variable=NULL){



  x<-group
  means<-tapply(y,x,mean)
  r<-length(table(x))
  residual<-match.arg(residual)
  a<-alpha
  fit <- lm(y ~ factor(x)) ## ok ##BF
  nt<-length(y)
  n<-tapply(y,x,length)

   if (   is.null(match.arg(MSE))    ){
    mse<-(deviance(fit))/(fit$df.residual)
  }else{
    mse<-MSE
  }


  rvo<-as.integer(names(table(x)))
ci<-c()
cj<-c()
for (i in 1:(r-1)){
  ii<-i+1
  for ( j in ii:r){
    ci<-c(ci,rvo[i])
    cj<-c(cj,rvo[j])
  }
}
rn<-paste0(ci,' - ' ,cj)
############################### change x
j<-1
xx<-x
for( i in rvo){
  x[x==i]<-j
  j<-j+1
}

  rv.s<-1:r
  ci<-c()
  cj<-c()
  for (i in 1:(r-1)){
    ii<-i+1
    for ( j in ii:r){
      ci<-c(ci,rv.s[i])
      cj<-c(cj,rv.s[j])
    }
  }

  ##################### descr
out.des<-  cbind(Group=rvo,n=n,mean=tapply(y,x,mean),median=tapply(y,x,median),Var=tapply(y,x,var),SD=tapply(y,x,sd), min=tapply(y,x,min),max=tapply(y,x,max))
  ########### res
  res<-fit$residuals
  sem<-res/sqrt(mse)


  s<-sqrt( (mse*(n-1))/(n)   )
  stu<-res/s[x]


  del<-rstudent(fit)

  if ( (residual=='simple')){
    e<-res
    tt<-'residuals'
  }else if((residual=='semistudentized')){
    e<-sem   # semi
    tt<-'semistudentized residuals'
  }else if((residual=='studentized')){
    e<-stu
    tt<-'studentized residuals'
  }else if((residual=='studentized.deleted')){
    e<-del
    tt<-'studentized deleted residuals'
  }
  ########### omisssion variable
  if ( !is.null(omission.variable) ){
    o1<-omission.variable==unique(omission.variable)[1]
    o2<-omission.variable==unique(omission.variable)[2]

    plot(e[o1]~fit$fitted.values[o1],xlim=range(fit$fitted.values),ylim=range(e),xlab='Yhat',ylab=tt,main=paste0(tt,' Plot against Fitted Values categorize Omission Variable'))
    points(fit$fitted.values[o2],e[o2],pch=20)

    plot(e[o1],x[o1],yaxt='n',frame.plot=F  ,xlim=range(e),ylim=c(0,r),ylab='Group',xlab=tt)

    abline(h = 1:r, lty = 2, col = "gray40", lwd = 1)
    axis(2,1:r,labels=rvo)
    points(e[o2],x[o2],pch=20)
    title(paste0('Aligned ',tt, ' Dot Plot categorize Omission Variable '))
  }
  ######################## seq
  plot(e, type = "b", lty = 2, xlab = "Run", ylab = tt,pch=16)
  title("Sequence Plot")


  ############################ plot
  qqnorm(e,main = paste0('Normal Q-Q Plot ',tt ))
  qqline(e)
  boxplot(e,main=paste0('Box plot ',tt ))
  hist(e,xlab=tt,main="")


  for (i in rvo){
    ee<-e[xx==i]
    qqnorm(ee,main = paste0('Normal Q-Q Plot ',tt,' Group ',i,' '))
    qqline(ee)
    hist(ee,main = paste0(' Group ',i,' '),xlab=tt)
  }

  boxplot(e~xx,xlab='Group',main=paste0('Box plot ',tt ))
  plot(e~fit$fitted.values,xlab='Yhat',ylab=tt)
  plot(e,rep(0,nt),frame.plot=F,yaxt='n',xlab=tt,ylab='',ylim = c(0,1),main = paste0('  Dot Plot ',tt))


  plot(e,x,yaxt='n',frame.plot=F  ,ylim=c(0,r),ylab='Group')
  abline(h = 1:r, lty = 2, col = "gray40", lwd = 1)
  axis(2,1:r,labels=rvo)
  title(paste0('Aligned ',tt, ' Dot Plot '))

  #plot(TukeyHSD(aov(y~x)))
  plot(means,rep(0,r),frame.plot=F,yaxt='n',xlim = range(y),ylab='',ylim = c(0,1))
  text(means,0,rvo,pos=3)
  title(" Dot plot level means ")


  plot(y,x,yaxt='n',frame.plot=F  ,ylim=c(0,r),ylab='Group')
  abline(h = 1:r, lty = 2, col = "gray40", lwd = 1)
  axis(2,1:r,labels=rvo)
  title(paste0('Aligned response variable Dot Plot ',tt))

  boxplot(y~xx,main='Boxplot respnse by groups ')
  barplot(means, xlab = "Group")
  title(" Means level Bar Graph")

  plot(means~rvo, type = "o", pch = 19,  xlab = "Group")
  abline(h = mean(y), lty = 2, col = 2, lwd = 1)
  title(main = " Main Effects Plot")
  ##################### single factor sf
  t<-qt(1-a/2,nt-r)
  s<-sqrt(mse/n)
  tv<-(means-c.value)/s
  pv<-2*(1-pt(abs(tv),nt-r))

  out.sf<-cbind(rvo,n,means,means-t*s,means+t*s,tv,round(pv,6) )
  colnames(out.sf)<- c('Group','size','mean', 'lower', 'upper','t','p-value')
  row.names(out.sf)<-NULL
  ################## plot
  diff<-t*s
  bar <- barplot(means, xlab = 'Group',ylim=c(min(means-2*diff,0),max(means+2*diff,y)))

  arrows(bar, means+diff, bar, means-diff, angle = 90, code = 3)
  tt<-paste0(' Bar-Interval Graph ',  1-a ,' percent confidence limits for each factor level')
  title(tt)

  plot(means~rvo, xlim=c(min(rvo-1),max(rvo+1)),ylim=c( min(means-diff)-min(diff) , max(means+diff)+min(diff) ),xlab = "Design")
  arrows(rvo, means+diff, rvo, means-diff, angle = 90, code = 3)
  abline(h = mean(y), lty = 2, col =2, lwd = 2)
  tt<-paste0('Interval Plot  ',  1-a ,'  percent confidence limits for each factor level')
  title(tt)


  ############# lsd  lsd

  d<-means[ci]-means[cj]
  s<-sqrt( mse*(1/(n[ci])  +1/(n[cj]))  )
  t<-qt(1-a/2,nt-r)
  tv<-(d-c.value)/s
  pv<-2*(1-pt(abs(tv),nt-r))

  out.lsd<- cbind(d,d-t*s,d+t*s,tv,round(pv,6))
  colnames(out.lsd)<- c('diffrence', 'lower', 'upper','t','p-value')
  rownames(out.lsd)<- rn

  ##################################### contrast one

  if ( !is.null(mc) ){
    out.c1<-matrix(1:5,1,5)

    for ( q in 1:dim(mc)[1]){
      l<-sum(means*mc[q,])
      s<-sqrt( mse*sum( (mc[q,]^2)/(n)   )   )
      tv<-(l)/s
      t<-qt(1-a/2,nt-r)
      pv<-2*(1-pt(abs(tv),nt-r))
      pv<-round(pv,6)
      out<- cbind(l,l-t*s,l+t*s,tv,pv)

      out.c1<-rbind(out.c1,out)
    }

    colnames(out.c1)<- c('L', 'lower', 'upper','t','p-value')
    out.c1<-out.c1[-1,]
  }



  ###############Tukey


 # d<-means[ci]-means[cj]
  #s<-sqrt( mse*(1/(n[ci])  +1/(n[cj]))  )
  #t<-qtukey(1-a,r,nt-r)/sqrt(2)

  #q<-(sqrt(2)*d)/s
  #pv<-2*(1-ptukey(abs(q),r,nt-r)) #########   p-v chek shavad

  #out.tky<- cbind(d,d-t*s,d+t*s,q,round(pv,4))
  #colnames(out.tky)<- c('diffrence', 'lower', 'upper','q*','p-value')
  #rownames(out.tky)<- rn

  out.tky<-TukeyHSD(aov(y~factor(x)),conf.level=1-a)
plot(out.tky<-TukeyHSD(aov(y~factor(x)),conf.level=1-a))

  #################################### shefffe  c.value =0 hamishee ,p.v  TEST

  if ( !is.null(mc) ){
    outsh<-matrix(1:5,1,5)

    for ( q in 1:dim(mc)[1]){
      l<-sum(means*mc[q,])
      S<-sqrt( (r - 1)*qf(1- a,r-1,nt - r)   )
      s<-sqrt( mse*sum( ((mc[q,])^2)/(n)   )   )
      fv<-(l^2)/((r-1)*(s^2))
      pv<-1-pf(fv,r-1,nt-r)
      out<- cbind(l,l-S*s,l+S*s,fv,pv)

      outsh<-rbind(outsh,out)
    }

    colnames(outsh)<- c('L', 'lower', 'upper','F','p-value')
    out.sh<-outsh[-1,]
  }



  ############# bon c==0  pv chekkkkk

  if ( !is.null(mc) ){



        out.b<-matrix(1:5,1,5)
    g<-dim(mc)[1]
    for ( q in 1:dim(mc)[1]){
      l<-sum(means*mc[q,])
      B<-qt(1-a/(2*g),nt - r)
      s<-sqrt( mse*sum( ((mc[q,])^2)/(n)   )   )
      tv<-l/s
      pv<-2*(1-pt(abs(tv),nt-r))
      out<- cbind(l,l-B*s,l+B*s,tv,pv)

      out.b<-rbind(out.b,out)
    }


    colnames(out.b)<- c('L', 'lower', 'upper','t','p-value')
    out.b<-out.b[-1,]


  }



  ############# hartly test

  df<-as.integer(mean(n))
  s2<-tapply(y,x,var)
  H<-max(s2)/min(s2)
  pv<- 1- pmaxFratio(H,  df-1,r)

  if ( is.numeric(pv)){
    out.ht<-cbind(H,pv)
    colnames(out.ht)<-c('H','p-value')
  }else{
    out.ht<-NULL
  }
  ############### bron forsy test


  d<-unlist(tapply(y,x,function(x) abs(x-median(x))))
  names(d)<-NULL
  di<-tapply(d,x,mean)
  dii<-mean(d)
  f<-anova(lm(d~factor(xx)) )
  out.bf<-cbind(f$`F value`[1],f$`Pr(>F)`[1] )
  colnames(out.bf)<-c('F value','p-value')
  ########## non para F test

  d<-rank(y)
  f<-anova(lm(d~factor(xx)) )
  out.n<-cbind(f$F[1],f$P[1] )
  colnames(out.n)<- c('F', 'p.value')

  ############ pair wise non para

  means_n<-tapply(d,x,mean)
  d<-means_n[ci]-means_n[cj]
  g<-(r*(r-1))/2
  s<-sqrt(   nt*(nt+1)*(1/(n[ci])  +1/(n[cj]) )*(1/12)   )
  B<-qnorm(1-a/(2*g))

  out.np<-cbind(d,d-B*s,d+s*B )
  colnames(out.np)<- c('difference', 'lower', 'upper')
  row.names(out.np)<-rn
  ########### outlier
  t.outli<-qt(1-a/(2*nt),nt-r-1)
  case<-c(1:nt)[abs(rstudent(fit))>t.outli]
  g<-y[case]
  if ( length(case!=0)){
    out.ot<-cbind(case=case,y=g,studentized.deleted.residual=rstudent(fit)[case],t.value=rep(t.outli,length(case) ))
  }else{
    out.ot<-paste0('t value=',t.outli,' ,do not exist outlier')
  }
####### ANOM

mmm<-mean(means)
tt<-qt(1-alpha/(2*r),nt-r)
s=c()
for ( i in 1:r){
    s<-c(s,sqrt((mse/n[i]) * ((r-1)/r)^2 + (mse/r^2) * sum(1/n[-i])))
}

out.an<-cbind(rvo,means-mmm-tt*s , means-mmm+tt*s)
out.an2<-cbind(rvo,mmm-tt*s , mmm+tt*s)
colnames(out.an)<- c('factor level', 'lower', 'upper')

dd<-diff(range(means))/4
plot(x = seq(r), means, pch = 20, xlim=c(.5,r+.5),ylim = c(min(out.an2[,2],means,mmm), max(out.an2[,3],means,mmm)), xlab = "Levels of Design", ylab = "Mean", xaxt = 'n')
axis(1, seq(r),labels = rvo)
segments(seq(r), mmm, seq(r), means)
lines(seq(1, r+.5, 0.5), rep(out.an2[,3], each = 2), type = "S")
lines(seq(1, r+.5, 0.5), rep(out.an2[,2], each = 2), type = "S")
abline(h = mmm)


############
  if ( !is.null(mc) ){
    o<-list(descriptive=out.des,fit=summary(fit),anova=anova(fit),  Single.factor.level=out.sf,Contrast.NOT.simultaneous=out.c1 ,LSD=out.lsd,Tukey=out.tky,Scheffe=out.sh ,Bonferroni=out.b ,Nonparametric.Rank.F.Test=out.n,Nonparametric.Rank.F.Test.Pairwise=out.np ,Hartley.Test=out.ht,Brown.Forsythe.test=out.bf,Bonfferoni.Test.Outlier=out.ot,ANOM.Bonferroni=out.an,residuals=res ,semistudentized.residuals=sem ,studentized.residuals=stu ,studentized.deleted.residuals= del)
  }else{
    o<-list( descriptive=out.des,fit=summary(fit),anova=anova(fit),  Single.factor.level=out.sf,LSD=out.lsd,Tukey=out.tky,Nonparametric.Rank.F.Test=out.n,Nonparametric.Rank.F.Test.Pairwise=out.np ,Hartley.Test=out.ht,Brown.Forsythe.test=out.bf,Bonfferoni.Test.Outlier=out.ot,ANOM.Bonferroni=out.an,residuals=res ,semistudentized.residuals=sem ,studentized.residuals=stu ,studentized.deleted.residuals= del)
  }
  return(o)
}








