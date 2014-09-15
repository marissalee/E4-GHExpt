#E4poster, take 3
#all.txt
#type = Mivi, Empty, Pavi, Sobi, CompEmpty, CompPavi, CompSobi

### variables:
#miviind
#mivi
#total
#percmivibiom
#nhdi
#nodi
#totdi
#ammonifd
#nitrifd
#minzd
#soilmoi

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

######################
library(lattice)
######################
#Plot the amount of biomass total biomass and mivi biomass per plant type
bwplot(total~type, data=Monos, xlab='Monoculture plant type', ylab='Total shoot biomass (g)', ylim=c(0,130))
bwplot(total~type, data=CompE, xlab='Mixture plant type', ylab='Total shoot biomass (g)', ylim=c(0,130))
bwplot(mivi~type, data=CompE, xlab='Mixture plant type', ylab='Mivi shoot biomass (g)', ylim=c(0,130))

#soil responses by total shoot biomass
par(mfrow=c(3,2), omd=c(0.001,0.98, 0.001, 0.98))
plot(nhdi~total, data=all,xlab='Total shoot biomass (g)', ylab='Soil NH4+ (ugN*g-1)', ylim=c(0,120), pch=16)
plot(nodi~total, data=all, xlab='Total shoot biomass (g)', ylab='Soil NO3- (ugN*g-1)', ylim=c(0,120),pch=16)
plot(ammonifd~total, data=all, xlab='Total shoot biomass (g)', ylab='Net ammonification (ugN*g-1*d-1)', ylim=c(-9,9),pch=16)
plot(nitrifd~total, data=all, xlab='Total shoot biomass (g)', ylab='Net nitrification (ugN*g-1*d-1)', ylim=c(-9,9),pch=16)
plot(minzd~total, data=all,  xlab='Total shoot biomass (g)', ylab='Net N mineralization (ugN*g-1*d-1)', ylim=c(-9,9),pch=16)
plot(soilmoi~total, data=all,  xlab='Total shoot biomass (g)', ylab='Soil moisture (%)', ylim=c(45,85),pch=16)

dev.off()
#soil responses by monoculture plant type
bwplot(nhdi~type, data=Monos, xlab='Monoculture plant type', ylab='Soil NH4+ (ugN*g-1)', ylim=c(0,120))
bwplot(nodi~type, data=Monos, xlab='Monoculture plant type', ylab='Soil NO3- (ugN*g-1)', ylim=c(0,120))
bwplot(ammonifd~type, data=Monos, xlab='Monoculture plant type', ylab='Net ammonification (ugN*g-1*d-1)', ylim=c(-9,9))
bwplot(nitrifd~type, data=Monos, xlab='Monoculture plant type', ylab='Net nitrification (ugN*g-1*d-1)', ylim=c(-9,9))
bwplot(minzd~type, data=Monos, xlab='Monoculture plant type', ylab='Net N mineralization (ugN*g-1*d-1)', ylim=c(-9,9))
bwplot(soilmoi~type, data=Monos, xlab='Monoculture plant type', ylab='Soil moisture (%)', ylim=c(45,85))

#soil responses by mixture plant type (CompEmpty, CompPavi and CompSobi) - abs abund
par(mfrow=c(3,2), omd=c(0.001,0.98, 0.001, 0.98))
plot(nhdi~mivi, data=Comp, type='n',xlab='Abs. abund, Mivi shoot biomass (g)', ylab='Soil NH4+ (ugN*g-1)', ylim=c(0,120), xlim=c(0,40))
points(x=CompEmpty$mivi, y=CompEmpty$nhdi, col='brown', pch=16)
points(x=CompPavi$mivi, y=CompPavi$nhdi, col='purple', pch=16)
points(x=CompSobi$mivi, y=CompSobi$nhdi, col='blue', pch=16)
plot(nodi~mivi, data=Comp, type='n', xlab='Abs. abund, Mivi shoot biomass (g)', ylab='Soil NO3- (ugN*g-1)', ylim=c(0,120),xlim=c(0,40))
points(x=CompEmpty$mivi, y=CompEmpty$nodi, col='brown', pch=16)
points(x=CompPavi$mivi, y=CompPavi$nodi, col='purple', pch=16)
points(x=CompSobi$mivi, y=CompSobi$nodi, col='blue', pch=16)
plot(ammonifd~mivi, data=Comp, type='n', xlab='Abs. abund, Mivi shoot biomass (g)', ylab='Net ammonification (ugN*g-1*d-1)', ylim=c(-9,9),xlim=c(0,40))
points(x=CompEmpty$mivi, y=CompEmpty$ammonifd, col='brown', pch=16)
points(x=CompPavi$mivi, y=CompPavi$ammonifd, col='purple', pch=16)
points(x=CompSobi$mivi, y=CompSobi$ammonifd, col='blue', pch=16)
plot(nitrifd~mivi, data=Comp, type='n', xlab='Abs. abund, Mivi shoot biomass (g)', ylab='Net nitrification (ugN*g-1*d-1)', ylim=c(-9,9), xlim=c(0,40))
points(x=CompEmpty$mivi, y=CompEmpty$nitrifd, col='brown', pch=16)
points(x=CompPavi$mivi, y=CompPavi$nitrifd, col='purple', pch=16)
points(x=CompSobi$mivi, y=CompSobi$nitrifd, col='blue', pch=16)
plot(minzd~mivi, data=Comp, type='n', xlab='Abs. abund, Mivi shoot biomass (g)', ylab='Net N mineralization (ugN*g-1*d-1)', ylim=c(-9,9), xlim=c(0,40))
points(x=CompEmpty$mivi, y=CompEmpty$minzd, col='brown', pch=16)
points(x=CompPavi$mivi, y=CompPavi$minzd, col='purple', pch=16)
points(x=CompSobi$mivi, y=CompSobi$minzd, col='blue', pch=16)
plot(soilmoi~mivi, data=Comp, type='n', xlab='Abs. abund, Mivi shoot biomass (g)', ylab='Soil moisture (%)', ylim=c(45,85), xlim=c(0,40))
points(x=CompEmpty$mivi, y=CompEmpty$soilmoi, col='brown', pch=16)
points(x=CompPavi$mivi, y=CompPavi$soilmoi, col='purple', pch=16)
points(x=CompSobi$mivi, y=CompSobi$soilmoi, col='blue', pch=16)

#soil responses by mixture plant type (CompPavi and CompSobi) - rel abund
par(mfrow=c(3,2))
plot(nhdi~percmivibiom, data=Comp, type='n', xlab='Rel. abund, Mivi shoot biomass (%)', ylab='Soil NH4+ (ugN*g-1)', ylim=c(0,120))
points(x=CompPavi$percmivibiom, y=CompPavi$nhdi, col='purple', pch=16)
points(x=CompSobi$percmivibiom, y=CompSobi$nhdi, col='blue', pch=16)
plot(nodi~percmivibiom, data=Comp, type='n', xlab='Rel. abund, Mivi shoot biomass (%)', ylab='Soil NO3- (ugN*g-1)', ylim=c(0,120))
points(x=CompPavi$percmivibiom, y=CompPavi$nodi, col='purple', pch=16)
points(x=CompSobi$percmivibiom, y=CompSobi$nodi, col='blue', pch=16)
plot(ammonifd~percmivibiom, data=Comp, type='n', xlab='Rel. abund, Mivi shoot biomass (%)', ylab='Net ammonification (ugN*g-1*d-1)', ylim=c(-9,9))
points(x=CompPavi$percmivibiom, y=CompPavi$ammonifd, col='purple', pch=16)
points(x=CompSobi$percmivibiom, y=CompSobi$ammonifd, col='blue', pch=16)
plot(nitrifd~percmivibiom, data=Comp, type='n', xlab='Rel. abund, Mivi shoot biomass (%)', ylab='Net nitrification (ugN*g-1*d-1)', ylim=c(-9,9))
points(x=CompPavi$percmivibiom, y=CompPavi$nitrifd, col='purple', pch=16)
points(x=CompSobi$percmivibiom, y=CompSobi$nitrifd, col='blue', pch=16)
plot(minzd~percmivibiom, data=Comp, type='n', xlab='Rel. abund, Mivi shoot biomass (%)', ylab='Net N mineralization (ugN*g-1*d-1)', ylim=c(-9,9))
points(x=CompPavi$percmivibiom, y=CompPavi$minzd, col='purple', pch=16)
points(x=CompSobi$percmivibiom, y=CompSobi$minzd, col='blue', pch=16)
plot(soilmoi~percmivibiom, data=Comp, type='n', xlab='Rel. abund, Mivi shoot biomass (%)', ylab='Soil moisture (%)', ylim=c(45,85))
points(x=CompPavi$percmivibiom, y=CompPavi$soilmoi, col='purple', pch=16)
points(x=CompSobi$percmivibiom, y=CompSobi$soilmoi, col='blue', pch=16)

###########
plot(nitrifd~mivi, data=Comp, type='n', xlab='Mivi shoot biomass (g)', ylab='Nitrification')
points(x=CompPavi$mivi, y=CompPavi$nhdi, col='purple', pch=16)
points(x=CompSobi$mivi, y=CompSobi$nhdi, col='blue', pch=16)

