#e4model_lme4.R -- model includes total plant biomass
# 4/7/13; 3/26/13

setwd("~/Desktop/E4_R")
library(lme4)
require(data.table)

##################################
#import data
data<-read.table('e4_trimmedData.txt',header=T)
data$total<-data$Mvabund + data$pavi + data$sobi
data$Compabund<-data$pavi + data$sobi
#make datasets
Empty<-data[data$type=='Empty',]
Mivi<-data[data$type=='Mivi',]
Pavi<-data[data$type=='Pavi',]
Sobi<-data[data$type=='Sobi',]
CompEmpty<-data[data$type=='CompEmpty',]
CompPavi<-data[data$type=='CompPavi',]
CompSobi<-data[data$type=='CompSobi',]

################################
#Make Fig2
datam<-rbind(Empty, Pavi, Sobi, Mivi)
#xs
datam$plant<-rep(NA,length(datam$type)) #initialize new column
datam[which(datam$type=='Empty'),19]<-1
datam[which(datam$type=='Mivi'),19]<-2
datam[which(datam$type=='Pavi'),19]<-3
datam[which(datam$type=='Sobi'),19]<-4
plant<-datam$plant
#key: 1= Unplanted, 2=M.v., 3= P.v., 4=S.b.
plantlabels<-c('Unplanted','M.v.','P.v.','S.b.')

#ys
soilmoi<-datam$soilmoi
nodi<-datam$nodi
nitrifd<-datam$nitrifd

#other
total<-datam$total


View(datam)
data <- data.frame(soilmoi,nodi,nitrifd, plant, total)
dat <- data.table(data)

######################################
par(mfrow = c(2,2), mar=c(4,4,1,1))
xlims <- c(0.5,4.5)

sum<-dat[,list(mean=mean(soilmoi), se=sd(soilmoi)/sqrt(length(soilmoi))),by=plant]
sum
plantvec<-sum$plant
mean<-sum$mean
se<-sum$se
sum
plot(soilmoi~plant, data, type = "n", xaxt = "n",
     xlab = "Plant type",
     ylab = "Soil moisture (%)",
     xlim = xlims,
     ylim = NULL)
axis(1, at=1:4, labels=plantlabels)
points(y=data$soilmoi,x=jitter(data$plant, .5), type = "p")
points(y=mean,x=plantvec+.2, type = "p", pch=18)
arrows(plantvec+.2,mean+se, plantvec+.2,mean-se,angle=90, code=3, length=0, col=1)

sum<-dat[,list(mean=mean(nodi), se=sd(nodi)/sqrt(length(nodi))),by=plant ]
sum
plantvec<-sum$plant
mean<-sum$mean
se<-sum$se
plot(nodi~plant, data, type = "n", xaxt = "n",
     xlab = "Plant type",
     ylab = "Nitrate (ugN/G)",
     xlim = xlims,
     ylim = NULL)
axis(1, at=1:4, labels=plantlabels)
points(y=data$nodi,x=jitter(data$plant, .5), type = "p")
points(y=mean,x=plantvec+.2, type = "p", pch=18)
arrows(plantvec+.2,mean+se, plantvec+.2,mean-se,angle=90, code=3, length=0, col=1)

sum<-dat[,list(mean=mean(nitrifd), se=sd(nitrifd)/sqrt(length(nitrifd))),by=plant]
sum
plantvec<-sum$plant
mean<-sum$mean
se<-sum$se
sum
plot(nitrifd~plant, data, type = "n", xaxt = "n",
     xlab = "Plant type",
     ylab = "Nitrification (ugN/G*d)",
     xlim = xlims,
     ylim = NULL)
axis(1, at=1:4, labels=plantlabels)
points(y=data$nitrifd,x=jitter(data$plant, .5), type = "p")
points(y=mean,x=plantvec+.2, type = "p", pch=18)
arrows(plantvec+.2,mean+se, plantvec+.2,mean-se,angle=90, code=3, length=0, col=1)

#for total pot plant biomass
xlims <- c(0.5,4.5)
sum<-dat[,list(mean=mean(total), se=sd(total)/sqrt(length(total))),by=plant]
plantvec<-sum$plant
mean<-sum$mean
se<-sum$se
sum
plot(total~plant, data, type = "n", xaxt = "n",
     xlab = "Plant type",
     ylab = "Plant biomass (g)",
     xlim = xlims,
     ylim = NULL)
axis(1, at=1:4, labels=plantlabels)
points(y=data$total,x=jitter(data$plant, .5), type = "p")
points(y=mean,x=plantvec+.2, type = "p", pch=18)
arrows(plantvec+.2,mean+se, plantvec+.2,mean-se,angle=90, code=3, length=0, col=1)


##################################
#Model structure
# response ~ Mvtrt * comptrt * Mvtrtxcomptrt
# response ~ Mvtrt * totalbiomass* Mvtrtxtotalbiomass

#where...
# totalbiomass is a continuous, positive measure
# Mvtrt is a factor with 4 levels: 1/6 Mv, 2/6 Mv, 4/6 Mv, 5/6 Mv
# comptrt is a factor with 3 levels: N, P, S
# responses include: 1) Mvabund (PS), 2) Compabund (PS), 3) soilmoi (PS), 4) nodi (PS), 5) no.nh (PS), 6) nitrifd (N), 7) minzd (N)

# Set-up data
dataa<-rbind(CompEmpty, CompPavi, CompSobi)
dataa$bk<-as.factor(dataa$bk)
#dataa$Mvtrt<-as.ordered(dataa$Mvtrt)
#set up mvtrt contrasts ... need to manipulate because didn't test level 3
#tmp<-contr.poly(5)
#mvtrt.contrasts<-tmp[-3,-(3:4)]
#mvtrt.contrasts
#contrasts(dataa$Mvtrt)<-mvtrt.contrasts
#transform totalplant biomass
hist(log(dataa$total))#transform total
shapiro.test(log(dataa$total)) #almost normal
dataa$logtotal<-log(dataa$total)

#xs
logtotal<-dataa$logtotal #x1
mvtrt<-dataa$Mvtrt #x2
comptrt<-dataa$comptrt #x3
bk<-dataa$bk #block
#ys
soilmoi<-dataa$soilmoi 
nodi<-dataa$nodi
nhno<-dataa$nh.no
nitrifd<-dataa$nitrifd
minzd<-dataa$minzd
response<-cbind(soilmoi,nodi,nhno,nitrifd,minzd)
dim(response)

##################################
#Look for correlations between predictors
plot(logtotal~mvtrt)
lm1<-lm(logtotal~mvtrt); anova(lm1) #not correlated

plot(logtotal~comptrt)
lm2<-lm(logtotal~comptrt); anova(lm2) #correlated

plot(dataa$Mvabund~mvtrt)
lm3<-lm(dataa$Mvabund~mvtrt); anova(lm3) #correlated
summary(lm3) #linear

plot(dataa$MVrelabund~mvtrt)
lm4<-lm(dataa$MVrelabund~mvtrt); anova(lm4) #correlated
summary(lm4) #linear

##################################
# #Loop through all responses - for mcmc p-vals
# models1<-list()
# models2<-list()
# 
# for (i in 1:dim(response)[2]){ #loop through each response
#   data1 <- data.frame(response[,i], logtotal, mvtrt, comptrt, bk) #define the dataframe
#   responsenow<-response[,i]
#   
#   model1 <- lmer(responsenow ~ mvtrt + comptrt + mvtrt:comptrt + mvtrt^2 +(1|bk), data=data1)
#   mcmc1<-pvals.fnc(model1, nsim=10000, withMCMC=TRUE)
#   models1[[i]]<-mcmc1$fixed #store the model results  
#   
#   model2 <- lmer(responsenow ~ mvtrt + logtotal + mvtrt:logtotal+ logtotal^2 + (1|bk), data=data1)
#   mcmc2<-pvals.fnc(model2, nsim=10000, withMCMC=TRUE)
#   models2[[i]]<-mcmc2$fixed #store the model results  
# }
# names(models1)<-colnames(response)
# names(models2)<-colnames(response)
# table1<-do.call('rbind',models1)
# table2<-do.call('rbind',models2)
# table1
# table2
# 
# write.table(table1, file='lme4_model1_mv1to5.txt', sep='\t', row.names=T, col.names=T)
# write.table(table2, file='lme4_model2_mv1to5.txt', sep='\t', row.names=T, col.names=T)


######################################################
#Figures
library(coefplot2)

######################
#Soilmoi
data1 <- data.frame(response[,1], logtotal, mvtrt, comptrt, bk)
model1b <- lmer(response[,1] ~ mvtrt + comptrt + mvtrt:comptrt+(1|bk), data=data1)
model1 <- lmer(response[,1] ~ mvtrt + comptrt + mvtrt:comptrt + I(mvtrt^2) +(1|bk), data=data1)
anova(model1,model1b) #no different
model2b <- lmer(response[,1] ~ mvtrt + logtotal + mvtrt:logtotal+(1|bk), data=data1)
model2 <- lmer(response[,1] ~ mvtrt + logtotal + mvtrt:logtotal + I(logtotal^2) +(1|bk), data=data1)
anova(model2,model2b) #different ... need to include logtotal^2
model2c <- lmer(response[,1] ~ mvtrt + logtotal + mvtrt:logtotal + I(logtotal^2) + I(mvtrt^2) +(1|bk), data=data1)
anova(model2,model2c) #not different
#final models
model1<-lmer(response[,1] ~ mvtrt + comptrt + mvtrt:comptrt+(1|bk), data=data1)
model1b<-lmer(response[,1] ~ mvtrt + comptrt + mvtrt:comptrt+(1|logtotal), data=data1)
model2<-lmer(response[,1] ~ mvtrt + logtotal + mvtrt:logtotal + I(logtotal^2) +(1|bk), data=data1)

par(mfrow = c(1,3))
coefplot2(model1, main='Soil moisture (%), \n Model1', vertical=F, mar=c(6,4,4,4)) #95% and 50% CI
coefplot2(model1b, main='Soil moisture (%), \n Model1b', vertical=F, mar=c(6,4,4,4))
coefplot2(model2, main='Soil moisture (%), \n Model2', vertical=F, mar=c(6,4,4,4))

#Plot with data with both models
cs1 <- fixef(model1) 
re1 <- ranef(model1)
data1$resid1<-resid(model1) 
cs2 <- fixef(model2) 
re2 <- ranef(model2)
data1$resid2<-resid(model2) 
#assign groups
data1N<-subset(data1, comptrt=='N')
data1P<-subset(data1, comptrt=='P')
data1S<-subset(data1, comptrt=='S')
# Plot model1
par(mfrow = c(1,2))
plot(data1[,1] ~ mvtrt, data1, ylim=c(50,100),type='n', ylab='Soil moisture (%)', xlab='M.v. individuals per pot')
points(y=data1N[,1], x=data1N$mvtrt, col=1)
points(y=data1P[,1], x=data1P$mvtrt, col=2)
points(y=data1S[,1], x=data1S$mvtrt, col=3)
curve(cs1[1] + cs1[2]*x, min(mvtrt), max(mvtrt), add=T, col=1) #group mean
curve(cs1[1] + (cs1[2]+cs1[5])*x + cs1[3], min(mvtrt), max(mvtrt), add=T, col=2) #group mean
curve(cs1[1] + (cs1[2]+cs1[6])*x + cs1[4], min(mvtrt), max(mvtrt), add=T, col=3) #group mean
legend('topright', legend=c('Model1: None','Model1: P.v.','Model1: S.b.'), lty=1, col=c(1,2,3), cex=.7)
#model2
cs2
plot(data1[,1] ~ logtotal, data1, type='n',ylim=c(50,100),ylab='Soil moisture (%)', xlab='Log-transformed \n Plant biomass per pot')
points(y=data1N[,1], x=data1N$logtotal, col=1)
points(y=data1P[,1], x=data1P$logtotal, col=2)
points(y=data1S[,1], x=data1S$logtotal, col=3)
curve(cs2[1] + cs2[2] + (cs2[3]+cs2[5])*x + cs2[4]*x^2,  min(logtotal), max(logtotal), add=T, col='gray')
legend('topright', legend=c('Model2: Plant biomass'), lty=1, col=c('gray'), cex=.7)

#########################
######################
#Nodi
data1 <- data.frame(response[,2], logtotal, mvtrt, comptrt, bk)
model1b <- lmer(response[,2] ~ mvtrt + comptrt + mvtrt:comptrt+(1|bk), data=data1)
model1 <- lmer(response[,2] ~ mvtrt + comptrt + mvtrt:comptrt + I(mvtrt^2) +(1|bk), data=data1)
anova(model1,model1b) #no different
model2b <- lmer(response[,2] ~ mvtrt + logtotal + mvtrt:logtotal+(1|bk), data=data1)
model2 <- lmer(response[,2] ~ mvtrt + logtotal + mvtrt:logtotal + I(logtotal^2) +(1|bk), data=data1)
anova(model2,model2b) #no different (don't include logtotal^2)
model2c <- lmer(response[,2] ~ mvtrt + logtotal + mvtrt:logtotal + I(mvtrt^2) +(1|bk), data=data1)
anova(model2b,model2c) #not different (don't include mvtrt^2)
#final models
model1<-lmer(response[,2] ~ mvtrt + comptrt + mvtrt:comptrt+(1|bk), data=data1)
model1b<-lmer(response[,2] ~ mvtrt + comptrt + mvtrt:comptrt+(1|logtotal), data=data1)
model2<-lmer(response[,2] ~ mvtrt + logtotal + mvtrt:logtotal +(1|bk), data=data1)

#test of fixed effects - Model1
model10<-lmer(response[,2] ~ mvtrt + I(mvtrt^2)+ comptrt + mvtrt:comptrt+(1|bk), data=data1)
model1<-lmer(response[,2] ~ mvtrt + comptrt + mvtrt:comptrt+(1|bk), data=data1)
model1a<-lmer(response[,2] ~ mvtrt + comptrt + (1|bk), data=data1)
model1b<-lmer(response[,2] ~ mvtrt + (1|bk), data=data1)
model1c<-lmer(response[,2] ~ 1 + (1|bk), data=data1)
anova(model10,model1)
anova(model1,model1a) # interaction ns
anova(model1a,model1b) # comptrt signif
anova(model1b,model1c) # mvtrt ns

#test of fixed effects - Model2
model20<-lmer(response[,1] ~ mvtrt + I(mvtrt^2) + logtotal + I(logtotal^2) + mvtrt:logtotal +(1|bk), data=data1)
model2<-lmer(response[,1] ~ mvtrt + logtotal + I(logtotal^2) + mvtrt:logtotal +(1|bk), data=data1)
model2a<-lmer(response[,1] ~ mvtrt + logtotal +  I(logtotal^2) +(1|bk), data=data1)
model2b<-lmer(response[,1] ~ mvtrt + logtotal +(1|bk), data=data1)
model2c<-lmer(response[,1] ~ mvtrt +(1|bk), data=data1)
model2d<-lmer(response[,1] ~ 1 +(1|bk), data=data1)
anova(model20,model2)
anova(model2,model2a) # interaction is marginally significant
anova(model2a,model2b) # logtotal^2 is significant
anova(model2b,model2c) # logtotal is significant
anova(model2c,model2d) # mvtrt is not significant

par(mfrow = c(1,3))
coefplot2(model1, main='Nitrate (ugN/G), \n Model1', vertical=F, mar=c(6,4,4,4)) #95% and 50% CI
coefplot2(model1b, main='Nitrate (ugN/G), \n Model1b', vertical=F, mar=c(6,4,4,4))
coefplot2(model2, main='Nitrate (ugN/G), \n Model2', vertical=F, mar=c(6,4,4,4))


#Plot with data with both models
cs1 <- fixef(model1) 
re1 <- ranef(model1)
data1$resid1<-resid(model1) 
cs2 <- fixef(model2) 
re2 <- ranef(model2)
data1$resid2<-resid(model2) 
#assign groups
data1N<-subset(data1, comptrt=='N')
data1P<-subset(data1, comptrt=='P')
data1S<-subset(data1, comptrt=='S')
# Plot
par(mfrow = c(1,2))
plot(data1[,1] ~ mvtrt, data1,type='n', ylim=c(0,100), ylab='Soil nitrate (ugN/G)', xlab='M.v. individuals per pot')
points(y=data1N[,1], x=data1N$mvtrt, col=1)
points(y=data1P[,1], x=data1P$mvtrt, col=2)
points(y=data1S[,1], x=data1S$mvtrt, col=3)
curve(cs1[1] + cs1[2]*x, min(mvtrt), max(mvtrt), add=T, col=1) #group mean
curve(cs1[1] + (cs1[2]+cs1[5])*x + cs1[3], min(mvtrt), max(mvtrt), add=T, col=2) #group mean
curve(cs1[1] + (cs1[2]+cs1[6])*x + cs1[4], min(mvtrt), max(mvtrt), add=T, col=3) #group mean
legend('topright', legend=c('Model1: None','Model1: P.v.','Model1: S.b.'), lty=1, col=c(1,2,3), cex=.7)
#model2
plot(data1[,1] ~ logtotal, data1, type='n',ylim=c(0,100),ylab='Soil nitrate (ugN/G)', xlab='Log-transformed \n Plant biomass per pot')
points(y=data1N[,1], x=data1N$logtotal, col=1)
points(y=data1P[,1], x=data1P$logtotal, col=2)
points(y=data1S[,1], x=data1S$logtotal, col=3)
curve(cs2[1] + cs2[2] + (cs2[3]+cs2[4])*x,  min(logtotal), max(logtotal), add=T, col='gray')
legend('topright', legend=c('Model2: Plant biomass'), lty=1, col=c('gray'), cex=.7)

#########################
######################
#NhNo
data1 <- data.frame(response[,3], logtotal, mvtrt, comptrt, bk)
model1b <- lmer(response[,3] ~ mvtrt + comptrt + mvtrt:comptrt+(1|bk), data=data1)
model1 <- lmer(response[,3] ~ mvtrt + comptrt + mvtrt:comptrt + I(mvtrt^2) +(1|bk), data=data1)
anova(model1,model1b) #no different
model2b <- lmer(response[,3] ~ mvtrt + logtotal + mvtrt:logtotal+(1|bk), data=data1)
model2 <- lmer(response[,3] ~ mvtrt + logtotal + mvtrt:logtotal + I(logtotal^2) +(1|bk), data=data1)
anova(model2,model2b) #different (include logtotal^2)
model2c <- lmer(response[,3] ~ mvtrt + logtotal + mvtrt:logtotal + I(logtotal^2)+ I(mvtrt^2) +(1|bk), data=data1)
anova(model2b,model2c) #different (include mvtrt^2)
#final models
model1<-lmer(response[,3] ~ mvtrt + comptrt + mvtrt:comptrt+(1|bk), data=data1)
model1b<-lmer(response[,3] ~ mvtrt + comptrt + mvtrt:comptrt+(1|logtotal), data=data1)
model2<-lmer(response[,3] ~ mvtrt + logtotal + mvtrt:logtotal+ I(logtotal^2)+ I(mvtrt^2) + (1|bk), data=data1)

par(mfrow = c(1,3))
coefplot2(model1, main='Ammonium: nitrate ratio, \n Model1', vertical=F, mar=c(6,4,4,4)) #95% and 50% CI
coefplot2(model1b, main='Ammonium: nitrate ratio, \n Model1b', vertical=F, mar=c(6,4,4,4))
coefplot2(model2, main='Ammonium: nitrate ratio, \n Model2', vertical=F, mar=c(6,4,4,4))

#Plot with data with both models
cs1 <- fixef(model1) 
re1 <- ranef(model1)
data1$resid1<-resid(model1) 
cs2 <- fixef(model2) 
re2 <- ranef(model2)
data1$resid2<-resid(model2) 
#assign groups
data1N<-subset(data1, comptrt=='N')
data1P<-subset(data1, comptrt=='P')
data1S<-subset(data1, comptrt=='S')
# Plot
par(mfrow = c(1,2))
plot(data1[,1] ~ mvtrt, data1,ylim=c(0,1.7),type='n', ylab='Soil ammonium: nitrate', xlab='M.v. individuals per pot')
points(y=data1N[,1], x=data1N$mvtrt, col=1)
points(y=data1P[,1], x=data1P$mvtrt, col=2)
points(y=data1S[,1], x=data1S$mvtrt, col=3)
curve(cs1[1] + cs1[2]*x, min(mvtrt), max(mvtrt), add=T, col=1) #group mean
curve(cs1[1] + (cs1[2]+cs1[5])*x + cs1[3], min(mvtrt), max(mvtrt), add=T, col=2) #group mean
curve(cs1[1] + (cs1[2]+cs1[6])*x + cs1[4], min(mvtrt), max(mvtrt), add=T, col=3) #group mean
legend('topright', legend=c('Model1: None','Model1: P.v.','Model1: S.b.'), lty=1, col=c(1,2,3), cex=.7)
#model2
plot(data1[,1] ~ logtotal, data1, ylim=c(0,1.7),type='n',ylab='Soil ammonium: nitrate', xlab='Log-transformed \n Plant biomass per pot')
points(y=data1N[,1], x=data1N$logtotal, col=1)
points(y=data1P[,1], x=data1P$logtotal, col=2)
points(y=data1S[,1], x=data1S$logtotal, col=3)
curve(cs2[1] + cs2[2] + (cs2[3]+cs2[6])*x + cs2[4]*(x^2) + cs2[5],  min(logtotal), max(logtotal), add=T, col='gray')
legend('topright', legend=c('Model2: Plant biomass'), lty=1, col=c('gray'), cex=.7)


#########################
######################
#Nitrifd
data1 <- data.frame(response[,4], logtotal, mvtrt, comptrt, bk)
model1b <- lmer(response[,4] ~ mvtrt + comptrt + mvtrt:comptrt+(1|bk), data=data1)
model1 <- lmer(response[,4] ~ mvtrt + comptrt + mvtrt:comptrt + I(mvtrt^2) +(1|bk), data=data1)
anova(model1,model1b) #no different
model2b <- lmer(response[,4] ~ mvtrt + logtotal + mvtrt:logtotal+(1|bk), data=data1)
model2 <- lmer(response[,4] ~ mvtrt + logtotal + mvtrt:logtotal + I(logtotal^2) +(1|bk), data=data1)
anova(model2,model2b) # no different (don't include logtotal^2)
model2c <- lmer(response[,4] ~ mvtrt + logtotal + mvtrt:logtotal + I(mvtrt^2) +(1|bk), data=data1)
anova(model2b,model2c) #no different (don't include mvtrt^2)
#final models
model1<-lmer(response[,4] ~ mvtrt + comptrt + mvtrt:comptrt+(1|bk), data=data1)
model1b<-lmer(response[,4] ~ mvtrt + comptrt + mvtrt:comptrt+(1|logtotal), data=data1)
model2<-lmer(response[,4] ~ mvtrt + logtotal + mvtrt:logtotal+ (1|bk), data=data1)

#test of fixed effects - Model1
model10<-lmer(response[,2] ~ mvtrt + I(mvtrt^2)+ comptrt + mvtrt:comptrt+(1|bk), data=data1)
model1<-lmer(response[,2] ~ mvtrt + comptrt + mvtrt:comptrt+(1|bk), data=data1)
model1a<-lmer(response[,2] ~ mvtrt + comptrt + (1|bk), data=data1)
model1b<-lmer(response[,2] ~ mvtrt + (1|bk), data=data1)
model1c<-lmer(response[,2] ~ 1 + (1|bk), data=data1)
anova(model10,model1)
anova(model1,model1a) # interaction ns
anova(model1a,model1b) # comptrt signif
anova(model1b,model1c) # mvtrt ns

#test of fixed effects - Model2
model20<-lmer(response[,1] ~ mvtrt + I(mvtrt^2) + logtotal + I(logtotal^2) + mvtrt:logtotal +(1|bk), data=data1)
model2<-lmer(response[,1] ~ mvtrt + logtotal + I(logtotal^2) + mvtrt:logtotal +(1|bk), data=data1)
model2a<-lmer(response[,1] ~ mvtrt + logtotal +  I(logtotal^2) +(1|bk), data=data1)
model2b<-lmer(response[,1] ~ mvtrt + logtotal +(1|bk), data=data1)
model2c<-lmer(response[,1] ~ mvtrt +(1|bk), data=data1)
model2d<-lmer(response[,1] ~ 1 +(1|bk), data=data1)
anova(model20,model2)
anova(model2,model2a) # interaction is marginally significant
anova(model2a,model2b) # logtotal^2 is significant
anova(model2b,model2c) # logtotal is significant
anova(model2c,model2d) # mvtrt is not significant


par(mfrow = c(1,3))
coefplot2(model1, main='Nitrification (ugN/G*d), \n Model1', vertical=F, mar=c(6,4,4,4)) #95% and 50% CI
coefplot2(model1b, main='Nitrification (ugN/G*d), \n Model1b', vertical=F, mar=c(6,4,4,4))
coefplot2(model2, main='Nitrification (ugN/G*d), \n Model2', vertical=F, mar=c(6,4,4,4))


#Plot with data with both models
cs1 <- fixef(model1) 
re1 <- ranef(model1)
data1$resid1<-resid(model1) 
cs2 <- fixef(model2) 
re2 <- ranef(model2)
data1$resid2<-resid(model2) 
#assign groups
data1N<-subset(data1, comptrt=='N')
data1P<-subset(data1, comptrt=='P')
data1S<-subset(data1, comptrt=='S')
# Plot
par(mfrow = c(1,2))
plot(data1[,1] ~ mvtrt, data1,type='n', ylim=c(-5,15), ylab='Nitrification (ugN/G*d)', xlab='M.v. individuals per pot')
points(y=data1N[,1], x=data1N$mvtrt, col=1)
points(y=data1P[,1], x=data1P$mvtrt, col=2)
points(y=data1S[,1], x=data1S$mvtrt, col=3)
curve(cs1[1] + cs1[2]*x, min(mvtrt), max(mvtrt), add=T, col=1) #group mean
curve(cs1[1] + (cs1[2]+cs1[5])*x + cs1[3], min(mvtrt), max(mvtrt), add=T, col=2) #group mean
curve(cs1[1] + (cs1[2]+cs1[6])*x + cs1[4], min(mvtrt), max(mvtrt), add=T, col=3) #group mean
legend('topright', legend=c('Model1: None','Model1: P.v.','Model1: S.b.'), lty=1, col=c(1,2,3), cex=.7)
#model2
plot(data1[,1] ~ logtotal, data1, type='n',ylim=c(-5,15),ylab='Nitrification (ugN/G*d)', xlab='Log-transformed \n Plant biomass per pot')
points(y=data1N[,1], x=data1N$logtotal, col=1)
points(y=data1P[,1], x=data1P$logtotal, col=2)
points(y=data1S[,1], x=data1S$logtotal, col=3)
curve(cs2[1] + cs2[2] + (cs2[3]+cs2[4])*x,  min(logtotal), max(logtotal), add=T, col='gray')
legend('topright', legend=c('Model2: Plant biomass'), lty=1, col=c('gray'), cex=.7)

#########################
######################
#Minzd
data1 <- data.frame(response[,5], logtotal, mvtrt, comptrt, bk)
model1b <- lmer(response[,5] ~ mvtrt + comptrt + mvtrt:comptrt+(1|bk), data=data1)
model1 <- lmer(response[,5] ~ mvtrt + comptrt + mvtrt:comptrt + I(mvtrt^2) +(1|bk), data=data1)
anova(model1,model1b) #no different
model2b <- lmer(response[,5] ~ mvtrt + logtotal + mvtrt:logtotal+(1|bk), data=data1)
model2 <- lmer(response[,5] ~ mvtrt + logtotal + mvtrt:logtotal + I(logtotal^2) +(1|bk), data=data1)
anova(model2,model2b) # no different (don't include logtotal^2)
model2c <- lmer(response[,5] ~ mvtrt + logtotal + mvtrt:logtotal + I(mvtrt^2) +(1|bk), data=data1)
anova(model2b,model2c) #no different (don't include mvtrt^2)
#final models
model1<-lmer(response[,5] ~ mvtrt + comptrt + mvtrt:comptrt+(1|bk), data=data1)
model1b<-lmer(response[,5] ~ mvtrt + comptrt + mvtrt:comptrt+(1|logtotal), data=data1)
model2<-lmer(response[,5] ~ mvtrt + logtotal + mvtrt:logtotal+ (1|bk), data=data1)

#test of fixed effects - Model1
model1<-lmer(response[,2] ~ mvtrt + comptrt + mvtrt:comptrt+(1|bk), data=data1)
model1a<-lmer(response[,2] ~ mvtrt + comptrt + (1|bk), data=data1)
model1b<-lmer(response[,2] ~ mvtrt + (1|bk), data=data1)
model1c<-lmer(response[,2] ~ 1 + (1|bk), data=data1)
anova(model1,model1a) # interaction ns
anova(model1a,model1b) # comptrt signif
anova(model1b,model1c) # mvtrt ns

#test of fixed effects - Model2
model2<-lmer(response[,2] ~ mvtrt + logtotal + mvtrt:logtotal +(1|bk), data=data1)
model2a<-lmer(response[,2] ~ mvtrt + logtotal +(1|bk), data=data1)
model2b<-lmer(response[,2] ~ mvtrt +(1|bk), data=data1)
model2c<-lmer(response[,2] ~ 1 +(1|bk), data=data1)
anova(model2,model2a) # interaction ns
anova(model2a,model2b) # comptrt signif
anova(model2b,model2c) # mvtrt ns

par(mfrow = c(1,3))
coefplot2(model1, main='Mineralization (ugN/G*d), \n Model1', vertical=F, mar=c(6,4,4,4)) #95% and 50% CI
coefplot2(model1b, main='Mineralization (ugN/G*d), \n Model1b', vertical=F, mar=c(6,4,4,4))
coefplot2(model2, main='Mineralization (ugN/G*d), \n Model2', vertical=F, mar=c(6,4,4,4))

#Plot with data with both models
cs1 <- fixef(model1) 
re1 <- ranef(model1)
data1$resid1<-resid(model1) 
cs2 <- fixef(model2) 
re2 <- ranef(model2)
data1$resid2<-resid(model2) 
#assign groups
data1N<-subset(data1, comptrt=='N')
data1P<-subset(data1, comptrt=='P')
data1S<-subset(data1, comptrt=='S')
# Plot
par(mfrow = c(1,2))
plot(data1[,1] ~ mvtrt, data1,ylim=c(-5,15), type='n', ylab='Mineralization (ugN/G*d)', xlab='M.v. individuals per pot')
points(y=data1N[,1], x=data1N$mvtrt, col=1)
points(y=data1P[,1], x=data1P$mvtrt, col=2)
points(y=data1S[,1], x=data1S$mvtrt, col=3)
curve(cs1[1] + cs1[2]*x, min(mvtrt), max(mvtrt), add=T, col=1) #group mean
curve(cs1[1] + (cs1[2]+cs1[5])*x + cs1[3], min(mvtrt), max(mvtrt), add=T, col=2) #group mean
curve(cs1[1] + (cs1[2]+cs1[6])*x + cs1[4], min(mvtrt), max(mvtrt), add=T, col=3) #group mean
legend('topright', legend=c('Model1: None','Model1: P.v.','Model1: S.b.'), lty=1, col=c(1,2,3), cex=.7)
#model2
plot(data1[,1] ~ logtotal, data1, ylim=c(-5,15), type='n',ylab='Mineralization (ugN/G*d)', xlab='Log-transformed \n Plant biomass per pot')
points(y=data1N[,1], x=data1N$logtotal, col=1)
points(y=data1P[,1], x=data1P$logtotal, col=2)
points(y=data1S[,1], x=data1S$logtotal, col=3)
curve(cs2[1] + cs2[2] + (cs2[3]+cs2[4])*x,  min(logtotal), max(logtotal), add=T, col='gray')
legend('topright', legend=c('Model2: Plant biomass'), lty=1, col=c('gray'), cex=.7)
