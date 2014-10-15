#E4_R_Code
#Mivi-only pots, no empty pots; comptrt=N/densitytrt=L0-L4
#dataset = e4n.txt
setwd("~/Desktop/E4_R")
##########################
library(MASS)
library(lme4)
library(LMERConvenienceFunctions)
library(lattice)
library(nlme)
########################
data <- read.table("e4n.txt",header=T)
#dataset cleaning
data2 <- data[!is.na(data$nitrifd),] #elim rows with NA in nitrifd column
data3 <- data2[is.na(data2$notes),] #elim rows without NA in the notes column
e4n <- data4









###
data <- read.table("e4n.txt",header=T)
#dataset cleaning
data1 <- data[!is.na(data$mivi),] #elim rows with NA in Mivi biom column
data2 <- data1[!is.na(data1$nitrifd),] #elim rows with NA in nitrifd column
data3 <- data2[is.na(data2$notes),] #elim rows without NA in the notes column
e4n <- data3

summary(e4n)

#distribution of mivi
shapiro.test(e4n$mivi) #normal

#distribution of nhd
nhd.log <- log(e4n$nhd)
shapiro.test(nhd.log) # p=0.03
nhd.sqrt <- sqrt(e4n$nhd)
shapiro.test(nhd.sqrt) # normal

#distribution of nod
shapiro.test(e4n$nod) # normal

#distribution of nitrifd
shapiro.test(e4n$nitrifd) # p=0.03
nitrifd.log <- log(e4n$nitrifd+1)
shapiro.test(nitrifd.log) # p=0.005
nitrifd.sqrt <- sqrt(e4n$nitrifd+2)
shapiro.test(nitrifd.sqrt) # p=0.01

#distribution of soilmoi
shapiro.test(e4n$soilmoi) # p<0.0001
soilmoi.log <- log(e4n$soilmoi)
shapiro.test(soilmoi.log) # p<0.0001

bk1<-as.factor(e4n$bk)

xyplot(mivi~miviind, data=e4n)




#Model to predict mivi based on percmiviind
xyplot(mivi~percmiviind|bk, data=e4n)
modelbiom<-lmer(mivi~percmiviind+(1|bk1), data=e4n)
mcmc.mivibiom <- mcmcsamp(modelbiom, saveb=T, n=5000)
mivibiomCI <- HPDinterval(mcmc.mivibiom,prob=0.95)$fixef
mivibiomCI

modelbiom1<-lme(fixed=mivi~1+percmiviind, random=~1|bk1, data=e4n, method="ML")
summary(modelbiom1)
fixef(modelbiom1)[1]->b0
fixef(modelbiom1)[2]->b1
ranef(modelbiom1)[,1]-> u0i
block<-seq(1:length(unique(e4n$bk)))
intercept<-b0+u0i
slope<-rep(b1,length(unique(e4n$bk)))
ran<-cbind(block,intercept,slope) 
plot(mivi~percmiviind, data=e4n, ylab='Mivi biomass (g)', xlab='# Mivi individuals per pot')
apply(ran, 1, function(y) curve(y[2]+ y[3]*x, col='dodgerblue4', add=T, from=0, to=100)) -> my.curves.added
abline(c(b0, b1), col='green', lwd=2)
legend('topleft', c('pop avg', 'sub specific'), col=c('green','dodgerblue4'), lty=1, cex=.9, bty='n', lwd=c(2,1))
mtext("slope p<0.0001",line=-11, adj=1)

#Model to predict nitrif 
xyplot(nitrifd~percmiviind|bk, data=e4n)
modelmiviind<-lmer(nitrifd~percmiviind+(1|bk1), data=e4n)
modelmivi<-lmer(nitrifd~mivi+(1|bk1), data=e4n)
fullmodel1<-lmer(nitrifd~percmiviind+nhdi+nodi+(1|bk1), data=e4n)
fullmodel2<-lmer(nitrifd~mivi+nhdi+nodi+(1|bk1), data=e4n)
mcmc.miviind <- mcmcsamp(modelmiviind, saveb=T, n=5000)
miviindCI <- HPDinterval(mcmc.miviind,prob=0.95)$fixef
miviindCI
mcmc.mivi <- mcmcsamp(modelmivi, saveb=T, n=5000)
miviCI <- HPDinterval(mcmc.mivi,prob=0.95)$fixef
miviCI
mcmc.full1 <- mcmcsamp(fullmodel1, saveb=T, n=5000)
full1CI <- HPDinterval(mcmc.full1,prob=0.95)$fixef
full1CI
mcmc.full2 <- mcmcsamp(fullmodel2, saveb=T, n=5000)
full2CI <- HPDinterval(mcmc.full2,prob=0.95)$fixef
full2CI

modelnitrif1<-lme(fixed=nitrifd~1+percmiviind, random=~1|bk1, data=e4n, method="ML")
summary(modelnitrif1)
fixef(modelnitrif1)[1]->b0
fixef(modelnitrif1)[2]->b1
ranef(modelnitrif1)[,1]-> u0i
block<-seq(1:length(unique(e4n$bk)))
intercept<-b0+u0i
slope<-rep(b1,length(unique(e4n$bk)))
ran<-cbind(block,intercept,slope) 
plot(nitrifd~percmiviind, data=e4n, ylab='Nitrifd', xlab='# Mivi individuals per pot')
apply(ran, 1, function(y) curve(y[2]+ y[3]*x, col='dodgerblue4', add=T, from=0, to=100)) -> my.curves.added
abline(c(b0, b1), col='green', lwd=2)
legend('topleft', c('pop avg', 'sub specific'), col=c('green','dodgerblue4'), lty=1, cex=.9, bty='n', lwd=c(2,1))
mtext("slope ns",line=-11, adj=1)

#Model to predict nodi
xyplot(nodi~percmiviind|bk, data=e4n)
modelmivi<-lmer(nodi~percmiviind+(1|bk1), data=e4n)
modelnhdi<-lmer(nodi~nhdi+(1|bk1), data=e4n)
modelnitrifd<-lmer(nodi~nitrifd+(1|bk1), data=e4n)
modelsoilmoi<-lmer(nodi~soilmoi+(1|bk1), data=e4n)
fullmodel<-lmer(nodi~percmiviind+nhdi+nitrifd+(1|bk1), data=e4n)

modelnodi1<-lme(fixed=nodi~1+percmiviind, random=~1|bk1, data=e4n, method="ML")
summary(modelnodi1)
fixef(modelnodi1)[1]->b0
fixef(modelnodi1)[2]->b1
ranef(modelnodi1)[,1]-> u0i
block<-seq(1:length(unique(e4n$bk)))
intercept<-b0+u0i
slope<-rep(b1,length(unique(e4n$bk)))
ran<-cbind(block,intercept,slope) 
plot(nodi~percmiviind, data=e4n, ylab='Nitrate Conc', xlab='# Mivi individuals per pot')
apply(ran, 1, function(y) curve(y[2]+ y[3]*x, col='dodgerblue4', add=T, from=0, to=100)) -> my.curves.added
abline(c(b0, b1), col='green', lwd=2)
legend('topleft', c('pop avg', 'sub specific'), col=c('green','dodgerblue4'), lty=1, cex=.9, bty='n', lwd=c(2,1))
mtext("slope ns",line=-11, adj=1)

#Other Plots
#nhd by percmiviind (bk)
xyplot(nhdi~percmiviind|bk, data=e4n)
#soilmoi by percmiviind (bk)
xyplot(soilmoi~percmiviind|bk, data=e4n)

