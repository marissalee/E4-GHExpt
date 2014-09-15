#e4 analyses
setwd("~/Desktop/E4_R")
all<-read.table('all.txt',header=T)

#load fxns and libraries
library(nlme)
library(sem)
library(polycor)
summarySE <- function(data=NULL,measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) 
{
  require(doBy)
  length2 <- function (x, na.rm=FALSE) {if (na.rm)sum(!is.na(x)) else length(x)}
  formula <- as.formula(paste(measurevar,paste(groupvars, collapse=" + "), sep=" ~ ")) # Collapse the data
  datac <- summaryBy(formula, data=data,FUN=c(length2,mean,sd), na.rm=na.rm)
  names(datac)[ names(datac) ==paste(measurevar, ".mean",    sep="") ] <- measurevar # Rename columns
  names(datac)[ names(datac) ==paste(measurevar, ".sd",      sep="") ] <-"sd"
  names(datac)[ names(datac) ==paste(measurevar, ".length2", sep="") ] <- "N"
  datac$se <- datac$sd /sqrt(datac$N)  # Calculate standarderror of the mean
  ciMult <- qt(conf.interval/2 + .5,datac$N-1) # Confidence interval multiplier forstandard error # Calculate t-statistic for confidenceinterval: # e.g., if conf.interval is .95, use .975(above/below), and use df=N-1
  datac$ci <- datac$se * ciMult
  return(datac)
}

#make datasets
#dataset1
Mivi<-all[all$type=='Mivi',]
Empty<-all[all$type=='Empty',]
dataset1<-rbind(Empty, Mivi)
#dataset2
Pavi<-all[all$type=='Pavi',]
Sobi<-all[all$type=='Sobi',]
dataset2<-rbind(Mivi, Pavi, Sobi)
dataset2a<-rbind(Empty,Mivi, Pavi, Sobi)
#dataset3
CompEmpty<-all[all$type=='CompEmpty',]
dataset3<-rbind(CompEmpty, Mivi)
#dataset4
CompPavi<-all[all$type=='CompPavi',]
CompSobi<-all[all$type=='CompSobi',]
dataset4<-rbind(CompPavi, CompSobi)

#determine normality
#dataset1 
shapiro.test(dataset1$nodi) #fits normal distb
shapiro.test(dataset1$totdi) #fits normal distb
shapiro.test(dataset1$nitrifd) #fits normal distb
shapiro.test(dataset1$minzd) #fits normal distb
#dataset2
shapiro.test(dataset2$nodi) #fits normal distb
shapiro.test(dataset2$totdi) #fits normal distb
shapiro.test(dataset2$nitrifd) #fits normal distb
shapiro.test(dataset2$minzd) #fits normal distb
#dataset3
dataset3$sqrt_nitrifd<-sqrt(dataset3$nitrifd+6)
dataset3$sqrt_minzd<-sqrt(dataset3$minzd+7)
shapiro.test(dataset3$nodi) #fits normal distb
shapiro.test(dataset3$totdi) #fits normal distb
shapiro.test(sqrt(dataset3$nitrifd+6)) #fits normal distb
shapiro.test(sqrt(dataset3$minzd+7)) #fits normal distb

#dataset4
dataset4$sqrt_nodi<-sqrt(dataset4$nodi)
dataset4$sqrt_nitrifd<-sqrt(dataset4$nitrifd+5)
dataset4$sqrt_minzd<-sqrt(dataset4$minzd+6)
shapiro.test(sqrt(dataset4$nodi)) #fits normal distb
shapiro.test(dataset4$totdi) #fits normal distb
shapiro.test(dataset4$sqrt_nitrifd) #fits normal distb
shapiro.test(dataset4$sqrt_minzd) #fits normal distb

#CompPavi
CompPavi$sqrt_nodi<-sqrt(CompPavi$nodi)
shapiro.test(sqrt(CompPavi$nodi)) 
shapiro.test(CompPavi$totdi) #fits normal distb
shapiro.test(CompPavi$nitrifd) #fits normal distb
shapiro.test(CompPavi$minzd) #fits normal distb

#CompSobi
CompSobi$sqrt_totdi<-sqrt(CompSobi$totdi)
CompSobi$sqrt_nitrifd<-sqrt(CompSobi$nitrifd+3)
CompSobi$sqrt_minzd<-sqrt(CompSobi$minzd+3)
shapiro.test(sqrt(CompSobi$nodi))
shapiro.test(CompSobi$totdi) #fits normal distb
shapiro.test(sqrt(CompSobi$nitrifd+3)) 
shapiro.test(sqrt(CompSobi$minzd+3))

#Q1: Is there more inorganic N and faster N mineralization under M.v. than other species (P.v., S.b.) and Empty pots?
# dataset1 - Empty monocultures, M.v. monocultures
smry <- summarySE(data=dataset1,measurevar="nodi", groupvars=c("type"))
smry <- summarySE(data=dataset1,measurevar="totdi", groupvars=c("type"))
smry <- summarySE(data=dataset1,measurevar="nitrifd", groupvars=c("type"))
smry <- summarySE(data=dataset1,measurevar="minzd", groupvars=c("type"))
mod<-lme(fixed=nodi~type, random=~1|bk, data=dataset1, method="ML") # signif
mod<-lme(fixed=totdi~type, random=~1|bk, data=dataset1, method="ML") # signif
mod<-lme(fixed=nitrifd~type, random=~1|bk, data=dataset1, method="ML") #ns
mod<-lme(fixed=minzd~type, random=~1|bk, data=dataset1, method="ML") #ns
# dataset2 - M.v., P.v.,and S.b. monocultures
smry <- summarySE(data=dataset2,measurevar="nodi", groupvars=c("type"))
smry <- summarySE(data=dataset2,measurevar="totdi", groupvars=c("type"))
smry <- summarySE(data=dataset2,measurevar="nitrifd", groupvars=c("type"))
smry <- summarySE(data=dataset2,measurevar="minzd", groupvars=c("type"))
mod<-lme(fixed=nodi~type, random=~1|bk, data=dataset2, method="ML") # signif
mod<-lme(fixed=totdi~type, random=~1|bk, data=dataset2, method="ML") # signif
mod<-lme(fixed=nitrifd~type, random=~1|bk, data=dataset2, method="ML") #ns
mod<-lme(fixed=minzd~type, random=~1|bk, data=dataset2, method="ML") #ns
# plot - N pools
empty<-jitter(rep(1,10), amount=.1)
mv<-jitter(rep(2,10), amount=.1)
pv<-jitter(rep(3,10), amount=.1)
sb<-jitter(rep(4,10), amount=.1)
x<-c(empty, mv, pv, sb)
plot(x=x,y=dataset2a$totdi, type= 'n', xaxt = "n", xlab = "Monoculture type", ylab = "Soil inorganic N",xlim = c(0.5,4.5),ylim = c(0,120))
axis(1, at=1:4, labels=c('Empty','M.v.','P.v.','S.b.'))
points(x=empty, y=Empty$totdi, pch=19)
points(x=mv, y=Mivi$totdi, pch=19)
points(x=pv, y=Pavi$totdi, pch=19)
points(x=sb, y=Sobi$totdi, pch=19)
points(x=empty, y=Empty$nodi)
points(x=mv, y=Mivi$nodi)
points(x=pv, y=Pavi$nodi)
points(x=sb, y=Sobi$nodi)
legend("topright", legend=c('Total inorganic N', 'Nitrate'), pch=c(19,1), cex=.8)
# plot - N fluxes
plot(x=x,y=dataset2a$minzd, type= 'n', xaxt = "n", xlab = "Monoculture type", ylab = "Net N mineralization",xlim = c(0.5,4.5),ylim = c(-12,12))
axis(1, at=1:4, labels=c('Empty','M.v.','P.v.','S.b.'))
points(x=empty, y=Empty$minzd, pch=19)
points(x=mv, y=Mivi$minzd, pch=19)
points(x=pv, y=Pavi$minzd, pch=19)
points(x=sb, y=Sobi$minzd, pch=19)
points(x=empty, y=Empty$nitrifd)
points(x=mv, y=Mivi$nitrifd)
points(x=pv, y=Pavi$nitrifd)
points(x=sb, y=Sobi$nitrifd)
legend("topright", legend=c('Net N mineralization', 'Nitrification'), pch=c(19,1), cex=.8)

#Q2: Is there more inorganic N and faster N mineralization under M.v. than other species (P.v., S.b.), considering variation in total plant biomass?
# dataset2 - M.v., P.v.,and S.b. monocultures
mod<-lme(fixed=nodi~type+total+type*total, random=~1|bk, data=dataset2, method="ML") # ns
mod<-lme(fixed=totdi~type+total+type*total, random=~1|bk, data=dataset2, method="ML") # ns
mod<-lme(fixed=nitrifd~type+total+type*total, random=~1|bk, data=dataset2, method="ML") #signif
mod_minzd<-lme(fixed=minzd~type+total+type*total, random=~1|bk, data=dataset2, method="ML") #signif
# plot N pool (y) vs total biomass (x) 
plot(totdi~total, data=dataset2, type='n', xlab='Total plant biomass (g)', ylab='Soil inorganic N')
points(x=Mivi$total, y=Mivi$totdi, col=1, pch=19)
points(x=Pavi$total, y=Pavi$totdi, col=2, pch=19)
points(x=Sobi$total, y=Sobi$totdi, col=3, pch=19)
points(x=Mivi$total, y=Mivi$nodi, col=1)
points(x=Pavi$total, y=Pavi$nodi, col=2)
points(x=Sobi$total, y=Sobi$nodi, col=3)
legend('topright', legend=c('M.v. - Total','P.v. - Total','S.b. - Total','M.v. - Nitrate','P.v. - Nitrate','S.b. - Nitrate'), pch=c(19,19,19,1,1,1), col=c(1,2,3,1,2,3), cex=.8)
# plot N minzd (y) vs total biomass (x)
plot(minzd~total, data=dataset2, type='n', xlab='Total plant biomass (g)', ylab='Net N mineralization')
points(x=Mivi$total, y=Mivi$minzd, col=1, pch=19)
points(x=Pavi$total, y=Pavi$minzd, col=2, pch=19)
points(x=Sobi$total, y=Sobi$minzd, col=3, pch=19)
points(x=Mivi$total, y=Mivi$nitrifd, col=1)
points(x=Pavi$total, y=Pavi$nitrifd, col=2)
points(x=Sobi$total, y=Sobi$nitrifd, col=3)
legend('topright', legend=c('M.v. - Minz','P.v. - Minz','S.b. - Minz','M.v. - Nitrif','P.v. - Nitrif','S.b. - Nitrif'), pch=c(19,19,19,1,1,1), col=c(1,2,3,1,2,3), cex=.8)

#Q3: How does a greater amount of M.v. biomass in a pot affect N pools and fluxes (in the absence of other species)?
# dataset3
mod<-lme(fixed=nodi~mivi, random=~1|bk, data=dataset3, method="ML") # ns
mod<-lme(fixed=totdi~mivi, random=~1|bk, data=dataset3, method="ML") # ns
mod<-lme(fixed=sqrt_nitrifd~mivi, random=~1|bk, data=dataset3, method="ML") # ns
mod<-lme(fixed=sqrt_minzd~mivi, random=~1|bk, data=dataset3, method="ML") # ns
# plot N pool (y) vs mivi biomass (x)
plot(totdi~mivi, data=dataset3, type='n', xlab='M.v. biomass (g)', ylab='Soil inorganic N', ylim=c(0,120))
points(x=dataset3$mivi, y=dataset3$totdi, pch=19)
points(x=dataset3$mivi, y=dataset3$nodi)
legend('topright', legend=c('M.v. - Total N','M.v. - Nitrate'), pch=c(19,1), cex=.8)
# plot N minz (y) vs mivi biomass (x)
plot(minzd~mivi, data=dataset3, type='n', xlab='M.v. biomass (g)', ylab='N Mineralization', ylim=c(-10,13))
points(x=dataset3$mivi, y=dataset3$minzd, pch=19)
points(x=dataset3$mivi, y=dataset3$nitrifd)
legend('topright', legend=c('M.v. - Minz','M.v. - Nitrif'), pch=c(19,1), cex=.8)

#Q4: How does a greater amount of M.v. biomass in a pot affect N pools and fluxes in the presence of other species? Compare the importance of total plant biomass, M.v. biomass, and percent M.v. biomass for predicting N pools and fluxes when M.v. was grown with P.v. or S.b.
# CompPavi
mod<-lme(fixed=sqrt_nodi~mivi+pavi+total, random=~1|bk, data=CompPavi, method="ML") # ns
mod<-lme(fixed=totdi~mivi+pavi+total, random=~1|bk, data=CompPavi, method="ML") # ns
mod<-lme(fixed=nitrifd~mivi+pavi+total, random=~1|bk, data=CompPavi, method="ML") # ns
mod<-lme(fixed=minzd~mivi+pavi+total, random=~1|bk, data=CompPavi, method="ML") # ns
# CompSobi
mod<-lme(fixed=nodi~mivi+sobi+total, random=~1|bk, data=CompSobi, method="ML") # ns
mod<-lme(fixed=sqrt_totdi~mivi+sobi+total, random=~1|bk, data=CompSobi, method="ML") # ns
mod<-lme(fixed=sqrt_nitrifd~mivi+sobi+total, random=~1|bk, data=CompSobi, method="ML") # signif
mod<-lme(fixed=sqrt_minzd~mivi+sobi+total, random=~1|bk, data=CompSobi, method="ML") # signif
#dataset3
mod<-lme(fixed=nodi~percmivibiom+mivi+total+type, random=~1|bk, data=dataset4, method="ML") # ns
mod<-lme(fixed=totdi~percmivibiom+mivi+total+type, random=~1|bk, data=dataset4, method="ML") # signif for total plant biomass
mod<-lme(fixed=sqrt_nitrifd~percmivibiom+mivi+total+type, random=~1|bk, data=dataset4, method="ML") # signif for total plant biomass
mod<-lme(fixed=sqrt_minzd~percmivibiom+mivi+total+type, random=~1|bk, data=dataset4, method="ML") # signif for total plant biomass

# plot of N pools (y) vs mvbiomass (x)
plot(totdi~mivi, data=dataset4, type='n', xlab='M.v. biomass (g)', ylab='Soil inorganic N', xlim=c(0,40), ylim=c(0,110))
points(x=CompPavi$mivi, y=CompPavi$totdi, col=1, pch=19)
points(x=CompSobi$mivi, y=CompSobi$totdi, col=2, pch=19)
points(x=CompPavi$mivi, y=CompPavi$nodi, col=1)
points(x=CompSobi$mivi, y=CompSobi$nodi, col=2)
legend('topright', legend=c('M.v. & P.v. - Total N','M.v. & S.b. - Total N','M.v. & P.v. - Nitrate','M.v. & S.b. - Nitrate'), pch=c(19,19,1,1), col=c(1,2,1,2), cex=.8)
# plot of N fluxes (y) vs mvbiomass (x)
plot(minzd~mivi, data=dataset4, type='n', xlab='M.v. biomass (g)', ylab='N Mineralization', xlim=c(0,40), ylim=c(-7,13))
points(x=CompPavi$mivi, y=CompPavi$minzd, col=1, pch=19)
points(x=CompSobi$mivi, y=CompSobi$minzd, col=2, pch=19)
points(x=CompPavi$mivi, y=CompPavi$nitrifd, col=1)
points(x=CompSobi$mivi, y=CompSobi$nitrifd, col=2)
legend('topright', legend=c('M.v. & P.v. - Minz','M.v. & S.b. - Minz','M.v. & P.v. - Nitrif','M.v. & S.b. - Nitrif'), pch=c(19,19,1,1), col=c(1,2,1,2), cex=.8)

# plot of N pools (y) vs percmivibiom (x)
plot(totdi~percmivibiom, data=dataset4, type='n', xlab='Percent M.v. biomass (%)', ylab='Soil inorganic N', xlim=c(0,100), ylim=c(0,110))
points(x=CompPavi$percmivibiom, y=CompPavi$totdi, col=1, pch=19)
points(x=CompSobi$percmivibiom, y=CompSobi$totdi, col=2, pch=19)
points(x=CompPavi$percmivibiom, y=CompPavi$nodi, col=1)
points(x=CompSobi$percmivibiom, y=CompSobi$nodi, col=2)
legend('topright', legend=c('M.v. & P.v. - Total N','M.v. & S.b. - Total N','M.v. & P.v. - Nitrate','M.v. & S.b. - Nitrate'), pch=c(19,19,1,1), col=c(1,2,1,2), cex=.8)
# plot of N fluxes (y) vs percmivibiom (x)
plot(minzd~percmivibiom, data=dataset4, type='n', xlab='Percent M.v. biomass (%)', ylab='N Mineralization', xlim=c(0,100), ylim=c(-7,13))
points(x=CompPavi$percmivibiom, y=CompPavi$minzd, col=1, pch=19)
points(x=CompSobi$percmivibiom, y=CompSobi$minzd, col=2, pch=19)
points(x=CompPavi$percmivibiom, y=CompPavi$nitrifd, col=1)
points(x=CompSobi$percmivibiom, y=CompSobi$nitrifd, col=2)
legend('topright', legend=c('M.v. & P.v. - Minz','M.v. & S.b. - Minz','M.v. & P.v. - Nitrif','M.v. & S.b. - Nitrif'), pch=c(19,19,1,1), col=c(1,2,1,2), cex=.8)

########
#SEM model

#specify the model
model.path <- specifyModel()
comptrt -> mivi, cm, NA
comptrt -> percmivibiom, cp, NA
comptrt -> nhdi, cnh, NA
comptrt -> nitrifd, cni, NA
mivi-> nhdi, mnh, NA
mivi -> nitrifd, mni, NA
percmivibiom-> nhdi, pnh, NA
percmivibiom -> nitrifd, pni, NA
total -> nhdi, tnh, NA
total -> nitrifd, tni, NA
nhdi -> nitrifd, hn, NA
nhdi <-> nhdi, enh, NA
nitrifd <-> nitrifd, eni, NA
mivi <-> mivi, em, NA
percmivibiom <-> percmivibiom, ep, NA
total <-> total, et, NA

model.path

#calc covariance matrix
data4<-dataset4[,c(6,8,12,13,14,19)]
#comptrt codes: P=1, S=2 
data4[,1]<-as.ordered(c(rep(1,40),rep(2,40)))
hcor<-function(data) hetcor(data, std.err=FALSE, ML=FALSE, pd=FALSE)$correlations
hcor.d4<-hcor(data4)

polyserial(data4)

hcor.d4

#fit the model
model.path.sem<-sem(model.path, hcor.d4, N=length(dataset4), fixed.x=c("comptrt"))
summary(model.path.sem)
stdCoef(model.path.sem)
pathDiagram(model.path.sem, "model.path.sem", edge.labels="values", digits=2, standardize=TRUE) #standardize prints the standardized effect coefs
system.time(boot <- bootSem(model.path.sem, R=500, cov=hcor.d4, data=d4))
summary(boot, type="norm")  # cf., standard errors to those computed by summary()

########
#individual regressions
#comptrt vs nhdi
plot(nhdi~comptrt, data=data4) #negative
#comptrt vs nitrif
plot(nitrifd~comptrt, data=data4) #negative
#comptrt vs mivi
plot(mivi~comptrt, data=data4) #negative
#comptrt vs percmivibiom
plot(percmivibiom~comptrt, data=data4) #negative
#total vs nhd
plot(nhdi~total, data=data4) #negative
#total vs nitrif
plot(nitrifd~total, data=data4) #negative
#mivi vs nhd
plot(nhdi~mivi, data=data4) #negative?
#mivi vs nitrifd 
plot(nitrifd~mivi, data=data4) #negative?
#percmivibiom vs nhd
plot(nhdi~percmivibiom, data=data4) #negative?
#percmivibiom vs nitrifd
plot(nitrifd~percmivibiom, data=data4) #negative
#nhdi vs nitrifd (+)
plot(nitrifd~nhdi, data=data4) #positive

