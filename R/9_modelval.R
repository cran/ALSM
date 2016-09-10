#' @export

######## DO NOT CALCULATE CP******************************************
modelval<-function(building.set,response.building, prediction.set, response.prediction){
  building.set<-as.matrix(building.set)
  response.building<-as.matrix(response.building)
  prediction.set<-as.matrix(prediction.set)
  response.prediction<-as.matrix(response.prediction)


  model1<-lm(response.building~building.set)
  model2<-lm(response.prediction~prediction.set)
  smodel1<-summary(model1)$coeff[,1:2]
  smodel2<-summary(model2)$coeff[,1:2]
  #leaps(model.buildi11g.set,y,method = 'Cp')
  #cp1<-cpc(model1)
  #cp2<-cpc(model2)
  p1<-pressc(model1)
  p2<-pressc(model2)
  a1<-AICp(model1)
  a2<-AICp(model2)
  s1<-SBCp(model1)
  s2<-SBCp(model2)
  r1<-summary(model1)$r.squared
  r2<-summary(model2)$r.squared
  r2a1<-summary(model1)$adj.r.squared
  r2a2<-summary(model2)$adj.r.squared

Model.Training<-c(smodel1[,1],smodel1[,2],deviance(model1)/df.residual(model1),r1,r2a1,a1,s1,p1)
Model.Validation<-c(smodel2[,1],smodel2[,2],deviance(model2)/df.residual(model2),r2,r2a2,a2,s2,p2)
v<-cbind(Model.Training,Model.Validation)
namE<-paste('b',0:ncol(building.set),sep = "")
namSE<-paste('SE b',0:ncol(building.set),sep = "")
rownames(v)<-c(namE,namSE,'MSE','R2','R2.adj','AICp','SBCp','PRESSp')
return(v)
}

