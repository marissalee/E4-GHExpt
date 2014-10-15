#E4_R_Code
#Comp-only pots, no empty or full pots; comptrt=PS/densitytrt=L1-L4
#dataset = e4ps.txt
library(MASS)
library(lme4)
library(lattice)
library(nlme)


setwd("~/Desktop/E4_R")
data <- read.table("e4ps.txt",header=T)
#dataset cleaning
data1 <- data[!is.na(data$mivi),] #elim rows with NA in Mivi biom column
data2 <- data1[!is.na(data1$nitrifd),] #elim rows with NA in nitrifd column
data3 <- data2[is.na(data2$notes),] #elim rows without NA in the notes column
e4ps <- data3

summary(e4ps)

#distribution of mivi
hist(e4ps$mivi)
shapiro.test(e4ps$mivi) # p<0.0001

#distribution of nod
hist(e4ps$nod)
shapiro.test(e4ps$nod) # p=0.0002

#distribution of nitrifd
hist(e4ps$nitrifd)
shapiro.test(e4ps$nitrifd) # p=0.0096

bk1<-as.factor(e4ps$bk)

#Model to predict mivi based on percmiviind
xyplot(mivi~percmiviind|bk+comptrt, data=e4ps)
modelbiom<-lmer(mivi~percmiviind+comptrt+percmiviind*comptrt+(1|bk1), data=e4ps)
mcmc.mivibiom <- mcmcsamp(modelbiom, saveb=T, n=5000)
mivibiomCI <- HPDinterval(mcmc.mivibiom,prob=0.95)$fixef
mivibiomCI

modelbiom1<-lme(fixed=mivi~1+percmiviind+comptrt+percmiviind*comptrt, random=~1|bk1, data=e4ps, method="ML")
summary(modelbiom1)

fixef(modelbiom1)[1]->b0
fixef(modelbiom1)[2]->b1
fixef(modelbiom1)[3]->b2
fixef(modelbiom1)[4]->b3
ranef(modelbiom1)[,1]-> u0i
block<-seq(1:length(unique(e4ps$bk)))
intercept<-b0+u0i
slope<-rep(b1,length(unique(e4ps$bk)))
ranP<-cbind(block,intercept,slope) 
ranP
intercept<-b0+b2+u0i
slope<-rep(b1+b3,length(unique(e4ps$bk)))
ranS<-cbind(block,intercept,slope)
ranS
plot(mivi~percmiviind, data=e4ps, ylab='Mivi biomass (g)', xlab='# Mivi individuals per pot')
apply(ranP, 1, function(y) curve(y[2]+ y[3]*x, col='dodgerblue4', add=T, from=0, to=100)) -> my.curves.added
apply(ranS, 1, function(y) curve(y[2]+ y[3]*x, col='purple', add=T, from=0, to=100)) -> my.curves.added
abline(c(b0, b1), col='green', lwd=2) #P
abline(c(b0+b2, b1+b3), col='red', lwd=2) #S
legend('topleft', c('P pop avg', 'P sub specific', 'S pop avg','S sub specific'), col=c('green','dodgerblue4','purple','red'), lty=1, cex=.9, bty='n', lwd=c(2,1))

#Model to predict nitrif based on percmiviind
modelnitrif1<-lme(fixed=nitrifd~1+percmiviind+comptrt+percmiviind*comptrt, random=~1|bk1, data=e4ps, method="ML")
summary(modelnitrif1)
fixef(modelnitrif1)[1]->b0
fixef(modelnitrif1)[2]->b1
fixef(modelnitrif1)[3]->b2
fixef(modelnitrif1)[4]->b3
ranef(modelnitrif1)[,1]-> u0i
block<-seq(1:length(unique(e4ps$bk)))
intercept<-b0+u0i
slope<-rep(b1,length(unique(e4ps$bk)))
ranP<-cbind(block,intercept,slope) 
ranP
intercept<-b0+b2+u0i
slope<-rep(b1+b3,length(unique(e4ps$bk)))
ranS<-cbind(block,intercept,slope)
ranS
plot(nitrifd~percmiviind, data=e4ps, ylab='nitrif', xlab='# Mivi individuals per pot')
apply(ranP, 1, function(y) curve(y[2]+ y[3]*x, col='dodgerblue4', add=T, from=0, to=100)) -> my.curves.added
apply(ranS, 1, function(y) curve(y[2]+ y[3]*x, col='purple', add=T, from=0, to=100)) -> my.curves.added
abline(c(b0, b1), col='green', lwd=2) #P
abline(c(b0+b2, b1+b3), col='red', lwd=2) #S
legend('bottomleft', c('P pop avg', 'P sub specific', 'S pop avg','S sub specific'), col=c('green','dodgerblue4','purple','red'), lty=1, cex=.9, bty='n', lwd=c(2,1))

#Model to predict nitrif based on mivi
modelnitrif1<-lme(fixed=nitrifd~1+mivi+comptrt+mivi*comptrt, random=~1|bk1, data=e4ps, method="ML")
summary(modelnitrif1)
fixef(modelnitrif1)[1]->b0
fixef(modelnitrif1)[2]->b1
fixef(modelnitrif1)[3]->b2
fixef(modelnitrif1)[4]->b3
ranef(modelnitrif1)[,1]-> u0i
block<-seq(1:length(unique(e4ps$bk)))
intercept<-b0+u0i
slope<-rep(b1,length(unique(e4ps$bk)))
ranP<-cbind(block,intercept,slope) 
ranP
intercept<-b0+b2+u0i
slope<-rep(b1+b3,length(unique(e4ps$bk)))
ranS<-cbind(block,intercept,slope)
ranS
plot(nitrifd~mivi, data=e4ps, ylab='nitrif', xlab='Mivi biomass (g)')
apply(ranP, 1, function(y) curve(y[2]+ y[3]*x, col='dodgerblue4', add=T, from=0, to=100)) -> my.curves.added
apply(ranS, 1, function(y) curve(y[2]+ y[3]*x, col='purple', add=T, from=0, to=100)) -> my.curves.added
abline(c(b0, b1), col='green', lwd=2) #P
abline(c(b0+b2, b1+b3), col='red', lwd=2) #S
legend('bottomleft', c('P pop avg', 'P sub specific', 'S pop avg','S sub specific'), col=c('green','dodgerblue4','purple','red'), lty=1, cex=.9, bty='n', lwd=c(2,1))

#Model to predict nitrif based on percmivibiom
modelnitrif1<-lme(fixed=nitrifd~1+percmivibiom+comptrt+percmivibiom*comptrt, random=~1|bk1, data=e4ps, method="ML")
summary(modelnitrif1)
fixef(modelnitrif1)[1]->b0
fixef(modelnitrif1)[2]->b1
fixef(modelnitrif1)[3]->b2
fixef(modelnitrif1)[4]->b3
ranef(modelnitrif1)[,1]-> u0i
block<-seq(1:length(unique(e4ps$bk)))
intercept<-b0+u0i
slope<-rep(b1,length(unique(e4ps$bk)))
ranP<-cbind(block,intercept,slope) 
ranP
intercept<-b0+b2+u0i
slope<-rep(b1+b3,length(unique(e4ps$bk)))
ranS<-cbind(block,intercept,slope)
ranS
plot(nitrifd~percmivibiom, data=e4ps, ylab='nitrif', xlab='Percent Mivi biomass (%)')
apply(ranP, 1, function(y) curve(y[2]+ y[3]*x, col='dodgerblue4', add=T, from=0, to=100)) -> my.curves.added
apply(ranS, 1, function(y) curve(y[2]+ y[3]*x, col='purple', add=T, from=0, to=100)) -> my.curves.added
abline(c(b0, b1), col='green', lwd=2) #P
abline(c(b0+b2, b1+b3), col='red', lwd=2) #S
legend('bottomleft', c('P pop avg', 'P sub specific', 'S pop avg','S sub specific'), col=c('green','dodgerblue4','purple','red'), lty=1, cex=.9, bty='n', lwd=c(2,1))

modelnitrifa<-lme(fixed=nitrifd~1+percmiviind+comptrt+percmiviind*comptrt, random=~1|bk1, data=e4ps, method="ML")
summary(modelnitrif1)
modelnitrifb<-lme(fixed=nitrifd~1+mivi+comptrt+mivi*comptrt, random=~1|bk1, data=e4ps, method="ML")
summary(modelnitrif1)
modelnitrifc<-lme(fixed=nitrifd~1+percmivibiom+comptrt+percmivibiom*comptrt, random=~1|bk1, data=e4ps, method="ML")
summary(modelnitrif1)
anova(modelnitrifa, modelnitrifb, modelnitrifc)
