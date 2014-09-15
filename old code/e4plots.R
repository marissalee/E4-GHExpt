#e4plots.R
#all.txt

#note= need to change ablineclip slopes to fixed effects

library(nlme)
library(ggplot2)
library(doBy)
setwd("~/Desktop/E4_R")
all0<- read.table("all.txt", header=T)
all1 <- all0[is.na(all0$notes),] #elim rows without NA in the notes column
all <- all1

########################
#subset data by type
Mivi<-all[all$type=='Mivi',]
Empty<-all[all$type=='Empty',]
Pavi<-all[all$type=='Pavi',]
Sobi<-all[all$type=='Sobi',]
CompEmpty<-all[all$type=='CompEmpty',]
CompPavi<-all[all$type=='CompPavi',]
CompSobi<-all[all$type=='CompSobi',]
Monos<-rbind(Empty, Mivi, Pavi, Sobi)
CompE<-rbind(CompEmpty,CompPavi, CompSobi)
Comp<-rbind(CompPavi, CompSobi)
#further cleaning...
temp<-CompEmpty[!is.na(CompEmpty$mivi),]
CompEmpty<-temp
temp<-CompE[!is.na(CompE$mivi),]
CompE<-temp
###########################################################
###########################################################
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
######################################
######################################
######################################
#percmiviind vs mivi
mCount <- summarySE(data=CompE,measurevar="mivi", groupvars=c("comptrt","percmiviind"))
mNCount <- subset(mCount,mCount$comptrt == "N")
mPCount <- subset(mCount,mCount$comptrt == "P")
mSCount <- subset(mCount,mCount$comptrt == "S")
ylims <- c(0, 35)
xlims <- c(0, 100)
plot(mNCount$percmiviind,mNCount$mivi, type = "n",
     xlab = "Initial M.v. occupancy (%)",
     ylab = "M.v. biomass (g)",
     xlim = xlims,
     ylim = ylims)
points(mNCount$percmiviind,mNCount$mivi, type = "p", pch = 19, col='red')
points(mPCount$percmiviind,mPCount$mivi, type = "p", pch = 19, col='blue')
points(mSCount$percmiviind,mSCount$mivi, type = "p", pch = 19, col='purple')
arrows(mNCount$percmiviind,mNCount$mivi+mNCount$se, mNCount$percmiviind, mNCount$mivi-mNCount$se,angle=90, code=3, length=0, col='red')
arrows(mPCount$percmiviind,mPCount$mivi+mPCount$se, mPCount$percmiviind, mPCount$mivi-mPCount$se,angle=90, code=3, length=0, col='blue')
arrows(mSCount$percmiviind,mSCount$mivi+mSCount$se, mSCount$percmiviind, mSCount$mivi-mSCount$se,angle=90, code=3, length=0, col='purple')
#legend('topright', c('M.v.','M.v.+P.v.','M.v.+S.b.'), pch=19, col=c('red', 'blue','purple'), cex=.9)

######################################
######################################
# percmiviind vs soil pools and fluxes
dev.off()
par(mfrow=c(2,3))
######################################
######################################
#percmiviind vs totdi and nodi
nCount <- summarySE(data=CompE,measurevar="totdi", groupvars=c("comptrt","percmiviind"))
nNCount <- subset(nCount,nCount$comptrt == "N")
nPCount <- subset(nCount,nCount$comptrt == "P")
nSCount <- subset(nCount,nCount$comptrt == "S")
Count <- summarySE(data=CompE,measurevar="nodi", groupvars=c("comptrt","percmiviind"))
NCount <- subset(Count,Count$comptrt == "N")
PCount <- subset(Count,Count$comptrt == "P")
SCount <- subset(Count,Count$comptrt == "S")
plot(nNCount$percmiviind,nNCount$totdi, type = "n",
     xlab = "Initial M.v. occupancy (%)",
     ylab = "Inorganic N (ugN/G)",
     xlim = c(0,100),
     ylim = c(15,55))
points(nNCount$percmiviind-2,nNCount$totdi, type = "p", pch = 19, col='red')
points(nPCount$percmiviind-1,nPCount$totdi, type = "p", pch = 19, col='blue')
points(nSCount$percmiviind,nSCount$totdi, type = "p", pch = 19, col='purple')
points(NCount$percmiviind+1,NCount$nodi, type = "p", pch = 2, col='red',lty=2)
points(PCount$percmiviind+2,PCount$nodi, type = "p", pch = 2, col='blue', lty=2)
points(SCount$percmiviind+3,SCount$nodi, type = "p", pch = 2, col='purple', lty=2)
arrows(nNCount$percmiviind-2,nNCount$totdi+nNCount$se, nNCount$percmiviind-2, nNCount$totdi-nNCount$se,angle=90, code=3, length=0, col='red')
arrows(nPCount$percmiviind-1,nPCount$totdi+nPCount$se, nPCount$percmiviind-1, nPCount$totdi-nPCount$se,angle=90, code=3, length=0, col='blue')
arrows(nSCount$percmiviind,nSCount$totdi+nSCount$se, nSCount$percmiviind, nSCount$totdi-nSCount$se,angle=90, code=3, length=0, col='purple')
arrows(NCount$percmiviind+1,NCount$nodi+NCount$se, NCount$percmiviind+1, NCount$nodi-NCount$se,angle=90, code=3, length=0, col='red', lty=2)
arrows(PCount$percmiviind+2,PCount$nodi+PCount$se, PCount$percmiviind+2, PCount$nodi-PCount$se,angle=90, code=3, length=0, col='blue', lty=2)
arrows(SCount$percmiviind+3,SCount$nodi+SCount$se, SCount$percmiviind+3, SCount$nodi-SCount$se,angle=90, code=3, length=0, col='purple', lty=2)
legend('topright', c('Ammonium + Nitrate; M.v.','Ammonium + Nitrate; M.v.+P.v.','Ammonium + Nitrate; M.v.+S.b.','Nitrate; M.v.','Nitrate; M.v.+P.v.','Nitrate; M.v.+S.b.'), col=c('red','blue','purple','red','blue','purple'), lty=c(1,1,1,2,2,2), pch=c(19,19,19,2,2,2), cex=.9)

######################################
#percmiviind vs minzd and nitrifd
nCount <- summarySE(data=CompE,measurevar="minzd", groupvars=c("comptrt","percmiviind"))
nNCount <- subset(nCount,nCount$comptrt == "N")
nPCount <- subset(nCount,nCount$comptrt == "P")
nSCount <- subset(nCount,nCount$comptrt == "S")
Count <- summarySE(data=CompE,measurevar="nitrifd", groupvars=c("comptrt","percmiviind"))
NCount <- subset(Count,Count$comptrt == "N")
PCount <- subset(Count,Count$comptrt == "P")
SCount <- subset(Count,Count$comptrt == "S")
plot(nNCount$percmiviind,nNCount$minzd, type = "n",
     xlab = "Initial M.v. occupancy (%)",
     ylab = "Net N mineralization (ugN/G*d)",
     xlim = c(0,100),
     ylim = c(-2,3.5))
points(nNCount$percmiviind-2,nNCount$minzd, type = "p", pch = 19, col='red')
points(nPCount$percmiviind-1,nPCount$minzd, type = "p", pch = 19, col='blue')
points(nSCount$percmiviind,nSCount$minzd, type = "p", pch = 19, col='purple')
points(NCount$percmiviind+1,NCount$nitrifd, type = "p", pch = 2, col='red')
points(PCount$percmiviind+2,PCount$nitrifd, type = "p", pch = 2, col='blue')
points(SCount$percmiviind+3,SCount$nitrifd, type = "p", pch = 2, col='purple')
arrows(nNCount$percmiviind-2,nNCount$minzd+nNCount$se, nNCount$percmiviind-2, nNCount$minzd-nNCount$se,angle=90, code=3, length=0, col='red')
arrows(nPCount$percmiviind-1,nPCount$minzd+nPCount$se, nPCount$percmiviind-1, nPCount$minzd-nPCount$se,angle=90, code=3, length=0, col='blue')
arrows(nSCount$percmiviind,nSCount$minzd+nSCount$se, nSCount$percmiviind, nSCount$minzd-nSCount$se,angle=90, code=3, length=0, col='purple')
arrows(NCount$percmiviind+1,NCount$nitrifd+NCount$se, NCount$percmiviind+1, NCount$nitrifd-NCount$se,angle=90, code=3, length=0, col='red', lty=2)
arrows(PCount$percmiviind+2,PCount$nitrifd+PCount$se, PCount$percmiviind+2, PCount$nitrifd-PCount$se,angle=90, code=3, length=0, col='blue', lty=2)
arrows(SCount$percmiviind+3,SCount$nitrifd+SCount$se, SCount$percmiviind+3, SCount$nitrifd-SCount$se,angle=90, code=3, length=0, col='purple', lty=2)
#legend('topright', c('Net Minz; M.v.','Net Minz; M.v.+P.v.','Net Minz; M.v.+S.b.','Nitrif; M.v.','Nitrif; M.v.+P.v.','Nitrif; M.v.+S.b.'), col=c('red','blue','purple','red','blue','purple'), lty=c(1,1,1,2,2,2), pch=c(19,19,19,2,2,2), cex=.9)

######################################
#soilmoisture
Count <- summarySE(data=CompE,measurevar="soilmoi", groupvars=c("comptrt","percmiviind"))
NCount <- subset(Count,Count$comptrt == "N")
PCount <- subset(Count,Count$comptrt == "P")
SCount <- subset(Count,Count$comptrt == "S")
xlims <- c(0, max(Count$percmiviind+20))
plot(NCount$percmiviind,NCount$soilmoi, type = "n",
     xlab = "Initial M.v. occupancy (%)",
     ylab = "Soil Moisture (%)",
     xlim = c(0,100),
     ylim = c(64,82))
points(NCount$percmiviind-1,NCount$soilmoi, type = "p", pch = 19, col='red')
points(PCount$percmiviind,PCount$soilmoi, type = "p", pch = 19, col='blue')
points(SCount$percmiviind+1,SCount$soilmoi, type = "p", pch = 19, col='purple')
arrows(NCount$percmiviind-1,NCount$soilmoi+NCount$se, NCount$percmiviind-1, NCount$soilmoi-NCount$se,angle=90, code=3, length=0, col='red')
arrows(PCount$percmiviind,PCount$soilmoi+PCount$se, PCount$percmiviind, PCount$soilmoi-PCount$se,angle=90, code=3, length=0, col='blue')
arrows(SCount$percmiviind+1,SCount$soilmoi+SCount$se, SCount$percmiviind+1, SCount$soilmoi-SCount$se,angle=90, code=3, length=0, col='purple')
#legend('topright', c('M.v.','M.v.+P.v.','M.v.+S.b.'), pch=19, col=c('red','blue','purple'), cex=.9)
########################################




######################################
######################################
#mivi vs totdi and nodi
nCount <- summarySE(data=CompE,measurevar="totdi", groupvars=c("comptrt"))
Count <- summarySE(data=CompE,measurevar="nodi", groupvars=c("comptrt"))
dataN <-subset(CompE, comptrt=='N')
dataP <-subset(CompE, comptrt=='P')
dataS <-subset(CompE, comptrt=='S')
tN<-lm(totdi~mivi, data=dataN)
tP<-lm(totdi~mivi, data=dataP)
tS<-lm(totdi~mivi, data=dataS)
nN<-lm(nodi~mivi, data=dataN)
nP<-lm(nodi~mivi, data=dataP)
nS<-lm(nodi~mivi, data=dataS)
plot(CompE$mivi,CompE$totdi, type = "n",
     xlab = "Final M.v. biomass (g)",
     ylab = "Inorganic N (ugN/G)",
     xlim = NULL,
     ylim = c(0,85))
points(dataN$mivi,dataN$totdi, type = "p", pch = 19, col='red', cex=.5)
points(dataP$mivi,dataP$totdi, type = "p", pch = 19, col='blue', cex=.5)
points(dataS$mivi,dataS$totdi, type = "p", pch = 19, col='purple',cex=.5)
points(dataN$mivi,dataN$nodi, type = "p", pch = 2, col='red',lty=2,cex=.5)
points(dataP$mivi,dataP$nodi, type = "p", pch = 2, col='blue', lty=2, cex=.5)
points(dataS$mivi,dataS$nodi, type = "p", pch = 2, col='purple', lty=2,cex=.5)
#ablineclip(tN, x1=min(dataN$mivi),x2=max(dataN$mivi),y1=min(dataN$totdi),y2=max(dataN$totdi),col='red')
#ablineclip(tP, x1=min(dataP$mivi),x2=max(dataP$mivi),y1=min(dataP$totdi),y2=max(dataP$totdi),col='blue')
#ablineclip(tS, x1=min(dataS$mivi),x2=max(dataS$mivi),y1=min(dataS$totdi),y2=max(dataS$totdi),col='purple')
#ablineclip(nN, x1=min(dataN$mivi),x2=max(dataN$mivi),y1=min(dataN$nodi),y2=max(dataN$nodi),col='red', lty=2)
#ablineclip(nP, x1=min(dataP$mivi),x2=max(dataP$mivi),y1=min(dataP$nodi),y2=max(dataP$nodi),col='blue', lty=2)
#ablineclip(nS, x1=min(dataS$mivi),x2=max(dataS$mivi),y1=min(dataS$nodi),y2=max(dataS$nodi),col='purple', lty=2)
#legend('topright', c('Ammonium + Nitrate; M.v.','Ammonium + Nitrate; M.v.+P.v.','Ammonium + Nitrate; M.v.+S.b.','Nitrate; M.v.','Nitrate; M.v.+P.v.','Nitrate; M.v.+S.b.'), pch=c(19,19,19,1,1,1),col=c('red','blue','purple','red','blue','purple'), cex=.7)

######################################
#mivi vs minzd and nitrifd
nCount <- summarySE(data=CompE,measurevar="minzd", groupvars=c("comptrt"))
Count <- summarySE(data=CompE,measurevar="nitrifd", groupvars=c("comptrt"))
dataN <-subset(CompE, comptrt=='N')
dataP <-subset(CompE, comptrt=='P')
dataS <-subset(CompE, comptrt=='S')
tN<-lm(minzd~mivi, data=dataN)
tP<-lm(minzd~mivi, data=dataP)
tS<-lm(minzd~mivi, data=dataS)
nN<-lm(nitrifd~mivi, data=dataN)
nP<-lm(nitrifd~mivi, data=dataP)
nS<-lm(nitrifd~mivi, data=dataS)
plot(CompE$mivi,CompE$minzd, type = "n",
     xlab = "Final M.v. biomass (g)",
     ylab = "Net N mineralization (ugN/G*d)",
     xlim = NULL,
     ylim = c(-6,13))
points(dataN$mivi,dataN$minzd, type = "p", pch = 19, col='red', cex=.5)
points(dataP$mivi,dataP$minzd, type = "p", pch = 19, col='blue', cex=.5)
points(dataS$mivi,dataS$minzd, type = "p", pch = 19, col='purple',cex=.5)
points(dataN$mivi,dataN$nitrifd, type = "p", pch = 2, col='red',lty=2,cex=.5)
points(dataP$mivi,dataP$nitrifd, type = "p", pch = 2, col='blue', lty=2, cex=.5)
points(dataS$mivi,dataS$nitrifd, type = "p", pch = 2, col='purple', lty=2,cex=.5)
#ablineclip(tN, x1=min(dataN$mivi),x2=max(dataN$mivi),y1=min(dataN$minzd),y2=max(dataN$minzd),col='red')
#ablineclip(tP, x1=min(dataP$mivi),x2=max(dataP$mivi),y1=min(dataP$minzd),y2=max(dataP$minzd),col='blue')
#ablineclip(tS, x1=min(dataS$mivi),x2=max(dataS$mivi),y1=min(dataS$minzd),y2=max(dataS$minzd),col='purple')
#ablineclip(nN, x1=min(dataN$mivi),x2=max(dataN$mivi),y1=min(dataN$nitrifd),y2=max(dataN$nitrifd),col='red', lty=2)
#ablineclip(nP, x1=min(dataP$mivi),x2=max(dataP$mivi),y1=min(dataP$nitrifd),y2=max(dataP$nitrifd),col='blue', lty=2)
#ablineclip(nS, x1=min(dataS$mivi),x2=max(dataS$mivi),y1=min(dataS$nitrifd),y2=max(dataS$nitrifd),col='purple', lty=2)
#legend('topright', c('N minz; M.v.','N minz; M.v.+P.v.','N minz; M.v.+S.b.','Nitrif; M.v.','Nitrif; M.v.+P.v.','Nitrif; M.v.+S.b.'), col=c('red','blue','purple','red','blue','purple'),pch=c(19,19,19,1,1,1), cex=.7)

######################################
#soilmoisture
nCount <- summarySE(data=CompE,measurevar="soilmoi", groupvars=c("comptrt"))
dataN <-subset(CompE, comptrt=='N')
dataP <-subset(CompE, comptrt=='P')
dataS <-subset(CompE, comptrt=='S')
tN<-lm(soilmoi~mivi, data=dataN)
tP<-lm(soilmoi~mivi, data=dataP)
tS<-lm(soilmoi~mivi, data=dataS)
plot(CompE$mivi,CompE$soilmoi, type = "n",
     xlab = "Final M.v. biomass (g)",
     ylab = "Soil Moisture (%)",
     xlim = NULL,
     ylim = c(48,90))
points(dataN$mivi,dataN$soilmoi, type = "p", pch = 19, col='red', cex=.5)
points(dataP$mivi,dataP$soilmoi, type = "p", pch = 19, col='blue', cex=.5)
points(dataS$mivi,dataS$soilmoi, type = "p", pch = 19, col='purple', cex=.5)
ablineclip(tN, x1=min(dataN$mivi),x2=max(dataN$mivi),y1=min(dataN$soilmoi),y2=max(dataN$soilmoi),col='red')
ablineclip(tP, x1=min(dataP$mivi),x2=max(dataP$mivi),y1=min(dataP$soilmoi),y2=max(dataP$soilmoi),col='blue')
ablineclip(tS, x1=min(dataS$mivi),x2=max(dataS$mivi),y1=min(dataS$soilmoi),y2=max(dataS$soilmoi),col='purple')
#legend('topright', c('M.v.','M.v.+P.v.','M.v.+S.b.'), pch=19, col=c('red','blue','purple'), cex=.7)

######################################
#####################################
######################################
dev.off()
par(mfrow=c(1,3))
######################################
# monocultures vs soil N pools and fluxes (Monos dataset)
#type vs totdi and nodi
nCount <- summarySE(data=Monos,measurevar="totdi", groupvars=c("type"))
Count <- summarySE(data=Monos,measurevar="nodi", groupvars=c("type"))
nCount
Count
plot(as.numeric(nCount$type),nCount$totdi, type = "n",xaxt = "n",
     xlab = "Monoculture type",
     ylab = "Inorganic N (ugN/G)",
     xlim = c(3.5,7.5),
     ylim = c(10,110))
axis(1, at=4:7, labels=c('Empty','M.v.','P.v.','S.b.'))
points(as.numeric(nCount$type)-.1,nCount$totdi, type = "p", pch = 19)
points(as.numeric(Count$type),Count$nodi, type = "p", pch = 2)
arrows(as.numeric(nCount$type)-.1,nCount$totdi+nCount$se, as.numeric(nCount$type)-.1, nCount$totdi-nCount$se,angle=90, code=3, length=0)
arrows(as.numeric(Count$type),Count$nodi+Count$se, as.numeric(Count$type), Count$nodi-Count$se,angle=90, code=3, length=0,lty=2)
legend('topright', c('Ammonium + Nitrate','Nitrate'), lty=c(1,2), pch=c(19,2), cex=.9)

#type vs minzd and nitrifd
nCount <- summarySE(data=Monos,measurevar="minzd", groupvars=c("type"))
Count <- summarySE(data=Monos,measurevar="nitrifd", groupvars=c("type"))
plot(as.numeric(nCount$type),nCount$minzd, type = "n",xaxt = "n",
     xlab = "Monoculture type",
     ylab = "Net N Mineralization (ugN/G*d)",
     xlim = c(3.5,7.5),
     ylim = c(-3,3))
axis(1, at=4:7, labels=c('Empty','M.v.','P.v.','S.b.'))
points(as.numeric(nCount$type)-.1,nCount$minzd, type = "p", pch = 19)
points(as.numeric(Count$type),Count$nitrifd, type = "p", pch = 2)
arrows(as.numeric(nCount$type)-.1,nCount$minzd+nCount$se, as.numeric(nCount$type)-.1, nCount$minzd-nCount$se,angle=90, code=3, length=0)
arrows(as.numeric(Count$type),Count$nitrifd+Count$se, as.numeric(Count$type), Count$nitrifd-Count$se,angle=90, code=3, length=0,lty=2)
#legend('topright', c('N Minz','Nitrif'), lty=c(1,2), pch=c(19,1), cex=.9)

#type vs soilmoi
nCount <- summarySE(data=Monos,measurevar="soilmoi", groupvars=c("type"))
plot(as.numeric(nCount$type),nCount$soilmoi, type = "n",xaxt = "n",
     xlab = "Monoculture type",
     ylab = "Soil Moisture (%)",
     xlim = c(3.5,7.5),
     ylim = c(60,87))
axis(1, at=4:7, labels=c('Empty','M.v.','P.v.','S.b.'))
points(as.numeric(nCount$type)-.1,nCount$soilmoi, type = "p", pch = 19)
arrows(as.numeric(nCount$type)-.1,nCount$soilmoi+nCount$se, as.numeric(nCount$type)-.1, nCount$soilmoi-nCount$se,angle=90, code=3, length=0)

######################################