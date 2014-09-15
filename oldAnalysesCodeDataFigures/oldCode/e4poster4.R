#e4poster4.R
#all.txt

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

#########
#further cleaning...
temp<-CompEmpty[!is.na(CompEmpty$mivi),]
CompEmpty<-temp

temp<-CompE[!is.na(CompE$mivi),]
CompE<-temp
###############################################
dev.off()
par(mfrow=c(2,3))
#Initial M.v. occupancy - Mivi alone
plot(nodi~percmiviind, data=CompEmpty, type='n', xlab='Initial M.v. occupancy (%)', ylab='Soil nitrate (ugN/G)')
  points(x=CompEmpty$percmiviind, y=CompEmpty$nodi, pch=16)
  abline(fixef(mod11n))
plot(nitrifd~percmiviind, data=CompEmpty, type='n', xlab='Initial M.v. occupancy (%)', ylab='Nitrification (ugN/G*d-1)')
  points(x=CompEmpty$percmiviind, y=CompEmpty$nitrifd, pch=16)
  abline(fixef(mod10n))
plot(minzd~percmiviind, data=CompEmpty, type='n', xlab='Initial M.v. occupancy (%)', ylab='Mineralization(ugN/G*d-1)')
  points(x=CompEmpty$percmiviind, y=CompEmpty$minzd, pch=16)
  abline(fixef(mod12n))

#Final M.v. occupancy - Mivi alone
plot(nodi~mivi, data=CompEmpty, type='n', xlab='Final M.v. occupancy (g)', ylab='Soil nitrate (ugN/G)')
  points(x=CompEmpty$mivi, y=CompEmpty$nodi, pch=16)
  abline(fixef(mod8n))
plot(nitrifd~mivi, data=CompEmpty, type='n', xlab='Final M.v. occupancy (g)', ylab='Nitrification (ugN/G*d-1)')
  points(x=CompEmpty$mivi, y=CompEmpty$nitrifd, pch=16)
  abline(fixef(mod7n))
plot(minzd~mivi, data=CompEmpty, type='n', xlab='Final M.v. occupancy (g)', ylab='Mineralization(ugN/G*d-1)')
  points(x=CompEmpty$mivi, y=CompEmpty$minzd, pch=16)
  abline(fixef(mod9n))

#Initial M.v. occupancy - Pavi and Sobi
plot(nodi~percmiviind, data=Comp, type='n', xlab='Initial percent M.v. occupancy (%)', ylab='Soil nitrate (ugN/G)', xlim=c(0,100))
  points(x=CompPavi$percmiviind, y=CompPavi$nodi, col='purple', pch=16)
  points(x=CompSobi$percmiviind, y=CompSobi$nodi, col='blue', pch=16)
  abline(fixef(mod5p), col='purple')
  abline(fixef(mod5s), col='blue')
plot(nitrifd~percmiviind, data=Comp, type='n', xlab='Initial M.v. occupancy (%)', ylab='Nitrification (ugN/G*d-1)', xlim=c(0,100))
  points(x=CompPavi$percmiviind, y=CompPavi$nitrifd, col='purple', pch=16)
  points(x=CompSobi$percmiviind, y=CompSobi$nitrifd, col='blue', pch=16)
  abline(fixef(mod4p), col='purple')
  abline(fixef(mod4s), col='blue')
plot(minzd~percmiviind, data=Comp, type='n', xlab='Initial M.v. occupancy (%)', ylab='Mineralization(ugN/G*d-1)', xlim=c(0,100))
  points(x=CompPavi$percmiviind, y=CompPavi$minzd, col='purple', pch=16)
  points(x=CompSobi$percmiviind, y=CompSobi$minzd, col='blue', pch=16)
  abline(fixef(mod6p), col='purple')
  abline(fixef(mod6s), col='blue')

#Final M.v. occupancy - Pavi and Sobi
plot(nodi~percmivibiom, data=Comp, type='n', xlab='Final percent M.v. biomass (%)', ylab='Soil nitrate (ugN/G)',xlim=c(0,100))
  points(x=CompPavi$percmivibiom, y=CompPavi$nodi, col='purple', pch=16)
  points(x=CompSobi$percmivibiom, y=CompSobi$nodi, col='blue', pch=16)
  abline(fixef(mod2p), col='purple')
  abline(fixef(mod2s), col='blue')
plot(nitrifd~percmivibiom, data=Comp, type='n', xlab='Final percent M.v. biomass (%)', ylab='Nitrification (ugN/G*d-1)', xlim=c(0,100))
  points(x=CompPavi$percmivibiom, y=CompPavi$nitrifd, col='purple', pch=16)
  points(x=CompSobi$percmivibiom, y=CompSobi$nitrifd, col='blue', pch=16)
  abline(fixef(mod1p), col='purple')
  abline(fixef(mod1s), col='blue')
plot(minzd~percmivibiom, data=Comp, type='n', xlab='Final percent M.v. biomass (%)', ylab='Mineralization (ugN/G*d-1)', xlim=c(0,100))
  points(x=CompPavi$percmivibiom, y=CompPavi$minzd, col='purple', pch=16)
  points(x=CompSobi$percmivibiom, y=CompSobi$minzd, col='blue', pch=16)
  abline(fixef(mod3p), col='purple')
  abline(fixef(mod3s), col='blue')

plot(nitrifd~percmivibiom, data=CompSobi, type='n', xlab='Final percent M.v. biomass (%)', ylab='Nitrification (ugN/G*d-1)')
  points(x=CompSobi$percmivibiom, y=CompSobi$nitrifd, col='blue', pch=16)
  abline(fixef(mod1s), col='blue')
plot(minzd~percmivibiom, data=CompSobi, type='n', xlab='Final percent M.v. biomass (%)', ylab='Mineralization (ugN/G*d-1)')
  points(x=CompSobi$percmivibiom, y=CompSobi$minzd, col='blue', pch=16)
  abline(fixef(mod3s), col='blue')



#############
library(nlme)
#############

#nitrification
#percmivibiom+comptrt+interactions
mod1<-lme(fixed=nitrifd~percmivibiom+comptrt+percmivibiom*comptrt, random=~1|bk, data=Comp, method="ML")
#investigate each species separately
mod1p<-lme(fixed=nitrifd~percmivibiom, random=~1|bk, data=CompPavi, method="ML")
mod1s<-lme(fixed=nitrifd~percmivibiom, random=~1|bk, data=CompSobi, method="ML")

#nitrate
#percmivibiom+comptrt+interactions
mod2<-lme(fixed=nodi~percmivibiom+comptrt+percmivibiom*comptrt, random=~1|bk, data=Comp, method="ML")
#investigate each species separately
mod2p<-lme(fixed=nodi~percmivibiom, random=~1|bk, data=CompPavi, method="ML")
mod2s<-lme(fixed=nodi~percmivibiom, random=~1|bk, data=CompSobi, method="ML")

#mineralization
#total+mivi+comptrt+interactions
mod3<-lme(fixed=minzd~percmivibiom+comptrt+percmivibiom*comptrt, random=~1|bk, data=Comp, method="ML")
#investigate each species separately
mod3p<-lme(fixed=minzd~percmivibiom, random=~1|bk, data=CompPavi, method="ML")
mod3s<-lme(fixed=minzd~percmivibiom, random=~1|bk, data=CompSobi, method="ML")

############################################################
#############################################################
#same tests, but use percent mivi occupancy

#nitrification
#percmiviind+comptrt+interactions
mod4<-lme(fixed=nitrifd~percmiviind+comptrt+percmiviind*comptrt, random=~1|bk, data=Comp, method="ML")
#investigate each species separately
mod4p<-lme(fixed=nitrifd~percmiviind, random=~1|bk, data=CompPavi, method="ML")
mod4s<-lme(fixed=nitrifd~percmiviind, random=~1|bk, data=CompSobi, method="ML")

#nitrate
#total+mivi+comptrt+interactions
mod5<-lme(fixed=nodi~percmiviind+comptrt+percmiviind*comptrt, random=~1|bk, data=Comp, method="ML")
#investigate each species separately
mod5p<-lme(fixed=nodi~percmiviind, random=~1|bk, data=CompPavi, method="ML")
mod5s<-lme(fixed=nodi~percmiviind, random=~1|bk, data=CompSobi, method="ML")

#mineralization
#total+mivi+comptrt+interactions
mod6<-lme(fixed=minzd~percmiviind+comptrt+percmiviind*comptrt, random=~1|bk, data=Comp, method="ML")
#investigate each species separately
mod6p<-lme(fixed=minzd~percmiviind, random=~1|bk, data=CompPavi, method="ML")
mod6s<-lme(fixed=minzd~percmiviind, random=~1|bk, data=CompSobi, method="ML")

############################################################
#############################################################
#same tests, but Mivi alone

#Mivi biomass
#nitrification
mod7n<-lme(fixed=nitrifd~mivi, random=~1|bk, data=CompEmpty, method="ML")
#nitrate
mod8n<-lme(fixed=nodi~mivi, random=~1|bk, data=CompEmpty, method="ML")
#mineralization
mod9n<-lme(fixed=minzd~mivi, random=~1|bk, data=CompEmpty, method="ML")

#Percent mivi occupancy
#nitrification
mod10n<-lme(fixed=nitrifd~percmiviind, random=~1|bk, data=CompEmpty, method="ML")
#nitrate
mod11n<-lme(fixed=nodi~percmiviind, random=~1|bk, data=CompEmpty, method="ML")
#mineralization
mod12n<-lme(fixed=minzd~percmiviind, random=~1|bk, data=CompEmpty, method="ML")


############################################################
#############################################################
#same tests, but use largest model with Mv vs N, P, S
#Percent mivi occupancy
#nitrification
mod13<-lme(fixed=nitrifd~percmiviind+as.factor(comptrt)+percmiviind*as.factor(comptrt), random=~1|bk, data=CompE, method="ML")
summary(mod13)
#nitrate
mod14<-lme(fixed=nodi~percmiviind+comptrt+percmiviind*comptrt, random=~1|bk, data=CompE, method="ML")
#mineralization
mod15<-lme(fixed=minzd~percmiviind+comptrt+percmiviind*comptrt, random=~1|bk, data=CompE, method="ML")

############################################################
#############################################################
#work with Monos, test plant trt
MonosE<-Monos[!(Monos$type=='Empty'),]
#nitrification
mod16<-lme(fixed=nitrifd~type, random=~1|bk, data=MonosE, method="ML")
#nitrate
mod17<-lme(fixed=nodi~type, random=~1|bk, data=MonosE, method="ML")
#mineralization
mod18<-lme(fixed=minzd~type, random=~1|bk, data=MonosE, method="ML")

