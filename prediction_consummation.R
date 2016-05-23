library(nlme)
library(mgcv)
library(ggplot2)
library(magrittr)

# 1. Installer des données ------------------------------------------------


setwd("~/Documents/sim2/sim2_regression_1")
data=readRDS("data_elec0.RDS")
#plot(data$date,data$Zone12,type='l',col="slateblue3",xlab="Date",ylab="Zone12",main="Figure de Zone12",pch=10)


data=data[which(data$Hour==20),]
#plot(data$Zone12,type='l',col="slateblue3",xlab="Date",ylab="Zone12",main="Figure de Zone12",pch=10)
#plot(data$Station1,data$Zone12,col="slateblue3",xlab="Station1",ylab="Zone12",main="Figure de Station1-Zone12",pch=3)
#plot((data$Zone12)[1:30],type='l',col="violetred3",xlab="Date",ylab="Zone12",main="Figure de Zone12 en janvier 2004",pch=10)


head(data)
zone12=data[,20]
station= data[,29:39]
Data0a=data[1:(1405-365),]
Data0b=data[-c(1:(1405-365)),]##l'an dernier
#summary(factor(data$daytype))


# 2. Fourier --------------------------------------------------------------


w=2*pi/(24*365)
Nfourier=2
for(i in c(1:Nfourier))
{
  assign(paste("cos",i,sep=""),cos(w*data$Time*i))
  assign(paste("sin",i,sep=""),sin(w*data$Time*i))
}
cos=paste('cos',c(1:Nfourier),sep="",collapse = ",")
sin=paste('sin',c(1:Nfourier),sep="",collapse = ",")

Zone12.24=data$Zone12
Zone12.24[2:nrow(data)]=data$Zone12[1:(nrow(data)-1)]
Data0=eval(parse(text=paste("data.frame(data,",cos,",",sin,")",sep="")))
Data0=data.frame(Data0,Zone12.24)

zone12=Data0[,20]
station= Data0[,29:39]
Data0a=Data0[1:(1405-365),]
Data0b=Data0[-c(1:(1405-365)),]##l'an dernier???


cos=paste(c('cos'),c(1:2),sep="")
sin=paste(c('sin'),c(1:2),sep="")
fourier=paste(c(cos,sin),collapse = "+")

# 2. Modélisation ---------------------------------------------------------

##Validation Croisée
eq=list()
forecast=c()
CV=c()
forecast1=c()
CV1=c()
forecast2=c()
CV2=c()
for(i in c(1:11))
{
  station=paste("Station",i,sep="")
  cos=paste(c('cos'),c(1:2),sep="")
  sin=paste(c('sin'),c(1:2),sep="")
  fourier=paste(c(cos,sin),collapse = "+")
  eq[[i]]=as.formula(paste("Zone12~Toy+dow+Month+Zone12.24+",fourier,"+",station,seq=""))
  glm.fit=glm(eq[[i]],data=Data0a)
  CV[i]=cv.glm(Data0a,glm.fit,K=10)$delta[1]
  forecast[i]=predict.glm(glm.fit,newdata = Data0b,se.fit = TRUE)
}

pre.rmse.cv=lapply(forecast,function(x){sqrt(mean((Data0b$Zone12-x)^2))})%>%unlist

par(mfrow=c(1,2))
plot(CV,type='b',xlab="Stations",ylab="CV",main="Figure de CV",pch=20)
abline(v=3,col="red",lty=2)
plot(pre.rmse.cv,type='b',xlab="Stations",ylab="pre.rmse",main="Figure de RMSE (CV-pre)",pch=20)
abline(v=5,col="red",lty=2)


v=combn(1:11,2)
for(i in c(1:ncol(v)))
{
  cos=paste(c('cos'),c(1:2),sep="")
  sin=paste(c('sin'),c(1:2),sep="")
  fourier=paste(c(cos,sin),collapse = "+")
  station<-paste("Station",v[,i],sep="",collapse = "+")
  eq[[i]]=as.formula(paste("Zone12~Toy+dow+Month+Zone12.24+",fourier,"+",station,seq=""))
  glm.fit=glm(eq[[i]],data=Data0a)
  CV1[i]=cv.glm(Data0a,glm.fit,K=10)$delta[1]
  forecast1[i]=predict.glm(glm.fit,newdata = Data0b,se.fit = TRUE)
}
o1=order(CV1)
pre.rmse.cv2=lapply(forecast1,function(x){sqrt(mean((Data0b$Zone12-x)^2))})%>%unlist
o2=order(pre.rmse.cv2)
##RMSE
par(mfrow=c(1,2))
plot(CV1[o1],type='b',pch=20,axes=F,xlab="Stations",ylab="CV2",main="Figure de CV")
axis(1,c(1:55),paste(v[1,o1],v[2,o1]),las=2)
axis(2)

plot(pre.rmse.cv2[o2],type='b',pch=20,axes=F,xlab="Stations",ylab="pre.rmse.cv2",main="Figure de RMSE (CV)")
axis(1,c(1:55),paste(v[1,o2],v[2,o2]),las=2)
axis(2)
##prédiction
par(mfrow=c(1,1))
plot(Data0a$Zone12,type='l',xlab="Date",ylab="Zone12",main="Figure de Zone12",xlim=c(1,1405))
lines(c(1041:1405),unlist(forecast1[which.min(CV)]),type='l',col="red")

##GAM=====================================================================
eq=list()
gam=list()
pregam=list()


for(i in c(1:11))
{ 
  stationstr=paste("Station",i,sep="")
  eq[[i]]=as.formula(paste("Zone12~s(",stationstr,",bs='ps',k=10)+s(Toy,bs='cc',k=10)+dow+Month+Zone12.24+",fourier,seq=""))
  gam[[i]]=gam(eq[[i]],data=Data0a)
  pregam[[i]]=predict(gam[[i]],newdata=Data0b)
}

fit.rmse=lapply(gam,function(x){sqrt(mean((Data0a$Zone12-x$fitted)^2))})%>%unlist
pre.rmse=lapply(pregam,function(x){sqrt(mean((Data0b$Zone12-x)^2))})%>%unlist

par(mfrow=c(1,2))
plot(fit.rmse,type='b',xlab="Stations",ylab="fit.rmse",main="Figure de RMSE (GAM)",pch=20)
abline(v=5,col="red",lty=2)
plot(pre.rmse,type='b',xlab="Stations",ylab="pre.rmse",main="Figure de RMSE (GAM-pre)",pch=20)
abline(v=5,col="red",lty=2)


##
eq2=list()
gam2=list()
pregam2=list()

v=combn(1:11,2)
for(i in c(1:ncol(v)))
{ 
  station1=paste("Station",v[1,i],sep="")
  station2=paste("Station",v[2,i],sep="")
  eq2[[i]]=as.formula(paste("Zone12~s(",station1,",bs='ps',k=10)+s(",station2,",bs='ps',k=10)+s(Toy,bs='cc',k=10)+dow+Month+Zone12.24+",fourier,seq=""))
  gam2[[i]]=gam(eq2[[i]],data=Data0a)
  pregam2[[i]]=predict(gam2[[i]],newdata=Data0b,se.fit = F)
}


fit.rmse2=lapply(gam2,function(x){sqrt(mean((Data0a$Zone12-x$fitted)^2))})%>%unlist
pre.rmse2=lapply(pregam2,function(x){sqrt(mean((Data0b$Zone12-x)^2))})%>%unlist

o1=order(fit.rmse2)
o2=order(pre.rmse2)

par(mfrow=c(1,2))
plot(fit.rmse2[o1],type='b',axes=F,pch=20,xlab="Stations",ylab="fit.rmse2",main="Figure de RMSE (GAM)")
axis(1,c(1:55),paste(v[1,o1],v[2,o1]),las=2)
axis(2)
#lines(pre.rmse2,type='b',col='red')

plot(pre.rmse2[o2],type='b',axes=F,pch=20,xlab="Stations",ylab="pre.rmse2",main="Figure de RMSE (GAM-prediction)")
axis(1,c(1:55),paste(v[1,o2],v[2,o2]),las=2)
axis(2)

fit.rmse[which.min(fit.rmse)]
pre.rmse[which.min(pre.rmse)]

fit.rmse2[which.min(fit.rmse2)]
pre.rmse2[which.min(pre.rmse2)]


##prédiction
par(mfrow=c(1,2))
plot(Data0a$Zone12,type='l',xlab="Date",ylab="Zone12",main="Figure de Zone12",xlim=c(1,1405))
lines(c(1041:1405),unlist(pregam[[5]]),type='l',col="blue")
abline(v=1041,col="orangered",lty=2)

plot(Data0a$Zone12,type='l',xlab="Date",ylab="Zone12",main="Figure de Zone12",xlim=c(1,1405))
lines(c(1041:1405),pregam2[[40]],type='l',col="red")
abline(v=1041,col="orangered",lty=2)

##résidus
par(mfrow=c(1,2),oma = c(0, 0, 3, 0))
plot(Data0$Zone12[c(1041:1405)]-pregam[[5]],type='l',xlab="Date",ylab="Zone12",main="Residus de prédictions d'un station",col="maroon2")
lines(Data0$Zone12[c(1041:1405)]-unlist(forecast1[5]),type='l',col="royalblue2")

plot(Data0$Zone12[c(1041:1405)]-pregam2[[40]],type='l',xlab="Date",ylab="Zone12",main="Residus de prédictions de deux stations",col="maroon2")
lines(Data0$Zone12[c(1041:1405)]-unlist(forecast1[40]),type='l',col="royalblue2")

legend("bottomright", legend = c("residus_gam ", "residus_cv "), col=c("maroon2","royalblue2"),lty=1)

# write.csv2(fit.rmse,file="erreur_gam1.csv",sep = " ",row.names = F,col.names = F)
# #read.csv2(fit.rmse,file="erreur.csv")
# 
# write.csv2(pre.rmse,file="erreur_gam_pre1.csv",sep = " ",row.names = F,col.names = F)
# #read.csv2(pre.rmse,file="erreur.csv")
# 
# write.csv2(fit.rmse2,file="erreur_gam2.csv",sep = " ",row.names = F,col.names = F)
# #read.csv2(fit.rmse,file="erreur.csv")
# 
# write.csv2(pre.rmse2,file="erreur_gam_pre2.csv",sep = " ",row.names = F,col.names = F)
# #read.csv2(fit.rmse,file="erreur.csv")


# 4.Bootstrap -------------------------------------------------------------

library(boot)
CV=function(formula,data,indices)
{
  d=Data0a[indices,] #allows boot to select sample
  fit=glm(formula,data=d)
  CV=cv.glm(Data0a,fit,K=10)$delta[1]
  return (CV)
}
GAM=function(formula,data,indices)
{
  d=Data0a[indices,] #allows boot to select sample
  fit=gam(formula,data=d)
  
  fit.rmse=sqrt(mean((Data0a$Zone12-fit$fitted)^2))
  return (fit.rmse)
}
eq=as.formula(paste("Zone12~dow+Month+Toy+Zone12.24+Station5+Station11+",fourier,seq=""))
botglm=boot(data=Data0a,statistic=CV,R=100,formula=eq)
boot.ci(botglm)

#bootstrapping with 100 replications
eq=as.formula(paste("Zone12~dow+Month+Zone12.24+s(Toy,bs='cc',k=10)+s(Station5,bs='ps',k=10)+s(Station11,bs='ps',k=10)","+",fourier,seq=""))
botgam=boot(data=Data0a,statistic=GAM,R=100,formula=eq)
boot.ci(botgam)




