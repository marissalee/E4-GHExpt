#E4 data, incorp randomness in bks

setwd("~/Desktop/Lachat Data/R stuff")

e4 <- read.table("E4_dw.txt",header=T)#read in data
e4.mivi <- e4[e4$comptrt=='N',]
e4.mivi <- e4.mivi[!is.na(e4.mivi[,6]),] #elim rows with NA in Mivi biom column
query1=is.na(e4.mivi[,24:30]) #elim rows with NA in any of the soil columns
query2=apply(query1,1,sum)
e4.mi <- e4.mivi[query2==0,]

e4.mi2 <- e4.mi[!e4.mi$densitytrt=='L5',]
View(e4.mi2)

#comptrt=N dataset with L0-L5 densitytrts
View(e4.mi) 
hist(e4.mi2$nitrifd)

library(MASS)
library(nlme)
library(lme4)
fitdistr(e4.mi2$nitrifd, densfun='normal')
shapiro.test(e4.mi2$nitrifd)

##########################################################################################
#nitrifd = soil nitrification potential
#miviind = num of mivi individuals planted per pot
#bk = 10 bks in total
#hdate = date the pot was harvested
hist(e4.mi$hdate)

#QUESTION1:Fit a variance components model and partition the variability in nitrifd as variability between pots within the same bk and variability between pots from different bks. Where does most of the variability occur?
xyplot(nitrifd~miviind|bk+hdate, data=e4.mi)
modela <- lme(fixed=nitrifd~1, random=~1|factor(bk), data=e4.mi, method="ML")
modelah <- lme(fixed=nitrifd~1, random=~1|hdate, data=e4.mi, method="ML")
bk1<-as.factor(e4.mi2$bk)
modelfull<-lmer(nitrifd~miviind+(1|bk1)+(1|hdate)+(1|hdate:miviind), data=e4.mi2)





modela1 <- lme(fixed=nitrifd~1, random=~1|hdate/factor(bk), data=e4.mi, method="ML")

VarCorr(modela)
tau.sq <- as.numeric(VarCorr(modela)[1,1]) #between bks
sigma.sq <- as.numeric(VarCorr(modela)[2,1]) #within bks

VarCorr(modelah)
tau.sq <- as.numeric(VarCorr(modelah)[1,1]) #between bks
tau.sq
sigma.sq <- as.numeric(VarCorr(modelah)[2,1]) #within bks
sigma.sq

#QUESTION2:Estimate the correlation of the response variable for observations coming from the same bk.
tau.sq/(tau.sq+sigma.sq) #the intraclass correlation coefficient
#the correlation between observations coming from the same bk is 0.33
#the correlation between observations coming from the same harvest is 0.16

Fit a regression model that estimates the overall linear relationship between nitrifd and num of mivi individuals per pot while properly accounting for the structure of the data set (bks).
modelb <- lme(fixed=nitrifd~1+miviind, random=~1|factor(bk), data=e4.mi, method="ML")
modelc <- lme(fixed=nitrifd~1+miviind, random=~1+factor(bk)|factor(bk), data=e4.mi, method="ML") 
summary(modelb) #better model
summary(modelc)

modelbh <- lme(fixed=nitrifd~1+miviind, random=~1|hdate, data=e4.mi2, method="ML")
modelch <- lme(fixed=nitrifd~1+miviind, random=~1+miviind|hdate, data=e4.mi, method="ML") 
summary(modelbh) #better model
summary(modelch)

#nitrifd=b0+(b1*N)+u0i
#pop model y=(b0)+(b1*x)
#sub-specific model y=(b0+u0i)+(b1*x)
fixef(modelbh)[1]->b0
fixef(modelbh)[2]->b1
ranef(modelbh)[,1]->u0i
#matrix of week starts and stops
miviindi.limit <- rep(0,length(unique(e4.mi$bk)))
miviindf.limit <- rep(6,length(unique(e4.mi$bk)))
miviind.limits <- cbind(miviindi.limit, miviindf.limit)

#matrix of random intercepts and slopes and limits
block<-seq(1:length(unique(e4.mi$bk)))
intercept<-b0+u0i
slope<-rep(b1,length(unique(e4.mi$bk)))
ran<-cbind(block,intercept,slope, miviind.limits) 
ran

#set up plot
plot(nitrifd~miviind, data=e4.mi2, ylab='Nitrifd', xlab='# Mivi individuals per pot')
#subject-specific curves - added
apply(ran, 1, function(y) curve(y[2]+ y[3]*x, col='dodgerblue4', add=T, from=y[4], to=y[5])) -> my.curves.added
#population averaged curve - added
abline(c(b0, b1), col='green', lwd=2)

#add legend
legend('topleft', c('pop avg', 'sub specific'), col=c('green','dodgerblue4'), lty=1, cex=.9, bty='n', lwd=c(2,1))

########################

#make a comptrt=P dataset; need to include comptrt=N, densitytrt=L0 because this is a whole mivi pot
e4.pavi <- e4[e4$comptrt=='P',]
e4.pavi1 <- e4.pavi[!is.na(e4.pavi[,7]),] #elim rows with NA in Pavi biom column
#e4.pavi1
query1=is.na(e4.pavi1[,15:21]) #elim rows with NA in any of the soil columns
query2=apply(query1,1,sum)
e4.pavi2 <- e4.pavi1[query2==0,]
#e4.pavi2

e4.miviNL0 <- e4.mivi2[e4.mivi2$densitytrt=='L0',] #comptrt=N, densitytrt=L0
e4.pavi3 <- rbind(e4.pavi2,e4.miviNL0)
#e4.pavi3

########################

#make a comptrt=S dataset; need to include comptrt=N, densitytrt=L0 because this is a whole mivi pot
e4.sobi <- e4[e4$comptrt=='S',]
e4.sobi1 <- e4.sobi[!is.na(e4.sobi[,8]),] #elim rows with NA in Mivi biom column
#e4.sobi1
query1=is.na(e4.sobi1[,15:21]) #elim rows with NA in any of the soil columns
query2=apply(query1,1,sum)
e4.sobi2 <- e4.sobi1[query2==0,]
#e4.sobi2

e4.sobi3 <- rbind(e4.sobi2,e4.miviNL0)
#e4.sobi3