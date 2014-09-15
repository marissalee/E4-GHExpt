#E4 Per Capita Analyses... and other stuff

#############################
setwd("~/Desktop/Lachat Data/R stuff")
e4 <- read.table("e4.txt",header=T)

#make a comptrt=N dataset
e4.mivi <- e4[e4$comptrt=='N',]
e4.mivi1 <- e4.mivi[!is.na(e4.mivi[,18]),] #elim rows with NA in Miviind column (no empty pots)
query1=is.na(e4.mivi1[,21:27]) #elim rows with NA in any of the soil columns
query2=apply(query1,1,sum)
e4.mivi2 <- e4.mivi1[query2==0,]

#make a comptrt=N,S,P dataset
e4.a <- e4[!is.na(e4[,18]),] #elim rows with NA in Miviind column (no empty pots)
View(e4.a)
query1=is.na(e4.a[,21:27]) #elim rows with NA in any of the soil columns
query2=apply(query1,1,sum)
e4.a1 <- e4.a[query2==0,]
e4.a2 <- e4.a1[!e4.a1$densitytrt=='L0',] #no pots with only Mv

#make a comptrt=S,P dataset
e4.b <- e4.a2[!e4.a2$comptrt=='N',]

#make a comptrt=N,P dataset
e4.p <- e4.a2[!e4.a2$comptrt=='S',]

#make a comptrt=N,S dataset
e4.s <- e4.a2[!e4.a2$comptrt=='P',]

#############################
library(lattice)
library(car)
###########################
hist(e4.mivi2$miviperind, xlab='Per capita Mivi biomass (g/ind.)', main='Data=N, Mivi monos, no empty pots')
hist(e4.mivi2$miviind) # is this an issue?

hist(e4.p$miviperind, xlab='Per capita Mivi biomass (g/ind.)', main='Data=NP, no Mivi monos, no empty pots')
hist(e4.p$miviind)

hist(e4.s$miviperind, xlab='Per capita Mivi biomass (g/ind.)', main='Data=NS, no Mivi monos, no empty pots')
hist(e4.s$miviind)

hist(e4.b$miviperind, xlab='Per capita Mivi biomass (g/ind.)', main='Data=PS, no Mivi monos, no empty pots')
hist(e4.b$miviind)

#Effect of Mivi density on per capita Mivi biomass - Mivi alone
mod1<-log(miviperind+.5)~miviind*factor(comptrt)
scatterplot(mod1, data=e4.mivi2, smooth=T, reg.line=F, col=3, grid=F, jitter=list(x=1,y=0),
            xlab='Number of Mivi individuals per pot', ylab='Log(Per capita Mivi biomass (g/ind.))', 
            xlim=c(0,6), legend.coord='topright',legend.title='Competitor', bty='n')
summary(lm(miviperind~miviind, data=e4.mivi2))

#Effect of Mivi density on per capita Mivi biomass in presence/absence of competitor
scatterplot(mod1, data=e4.p, smooth=T, reg.line=F, col=c(3,4),grid=F, pch=c(1,1),jitter=list(x=1,y=0),
            xlab='Number of Mivi individuals per pot', ylab='Log(Per capita Mivi biomass (g/ind.))', 
            xlim=c(0,6), legend.coord='topright',legend.title='Competitor', bty='n')
summary(lm(mod1, data=e4.p))
anova(lm(mod1, data=e4.p))
scatterplot(mod1, data=e4.s, smooth=T, reg.line=F, col=c(3,5),grid=F, pch=c(1,1),jitter=list(x=1,y=0),
            xlab='Number of Mivi individuals per pot', ylab='Log(Per capita Mivi biomass (g/ind.))', 
            xlim=c(0,6), legend.coord='topright',legend.title='Competitor', bty='n') 
summary(lm(mod1, data=e4.s))
anova(lm(mod1, data=e4.s))

#Effect of Mivi density on per capita Mivi biomass with competitors of differing identity
scatterplot(mod1, data=e4.b, smooth=T, reg.line=F, col=c(4,5), grid=F, pch=c(1,1),jitter=list(x=1,y=0),
            xlab='Number of Mivi individuals per pot', ylab='Log(Per capita Mivi biomass (g/ind.))', 
            xlim=c(0,6), legend.coord='topright',legend.title='Competitor', bty='n')
summary(lm(mod1, data=e4.b))
anova(lm(mod1, data=e4.b))

#######################################################
hist(e4.a2$total)
hist(e4.a2$nodi)
hist(e4.a2$nitrifd)
hist(e4.a2$soilmoi)

#Effect of total plant biomass on soil responses - NPS, no empty pots or monocultures

#nitrate
scatterplot(nodi~total, data=e4.a2, boxplot=F, col=1, smooth=F, grid=F, xlab='Total biomass (g)', ylab='Soil nitrate concentration')
points(nodi~total, data=e4.a2[e4.a2$comptrt=='N',], col=3)
points(nodi~total, data=e4.a2[e4.a2$comptrt=='P',], col=4)
points(nodi~total,data=e4.a2[e4.a2$comptrt=='S',], col=5)
summary(lm(nodi~total, data=e4.a2))
#nitrification 
scatterplot(nitrifd~total, data=e4.a2, boxplot=F, col=1, smooth=F, grid=F, xlab='Total biomass (g)', ylab='Net nitrification potential')
points(nitrifd~total, data=e4.a2[e4.a2$comptrt=='N',], col=3)
points(nitrifd~total, data=e4.a2[e4.a2$comptrt=='P',], col=4)
points(nitrifd~total,data=e4.a2[e4.a2$comptrt=='S',], col=5)
summary(lm(nitrifd~total, data=e4.a2))
#soilmoisture 
scatterplot(soilmoi~total, data=e4.a2, boxplot=F, col=1, smooth=F, grid=F, xlab='Total biomass (g)', ylab='Soil moisture (%)')
points(soilmoi~total, data=e4.a2[e4.a2$comptrt=='N',], col=3)
points(soilmoi~total, data=e4.a2[e4.a2$comptrt=='P',], col=4)
points(soilmoi~total,data=e4.a2[e4.a2$comptrt=='S',], col=5)
summary(lm(soilmoi~total, data=e4.a2))

#######################################################
hist(e4.b$percmivi)
hist(e4.b$nodi)
hist(e4.b$nitrifd)
hist(e4.b$soilmoi)


#Effect of %Mivi biomass on soil responses - PS, no empty pots or monocultures

#nitrate
scatterplot(nodi~percmivi, data=e4.b, boxplot=F, col=1, smooth=F, grid=F, xlab='%Mivi biomass', ylab='Soil nitrate concentration')
points(nodi~percmivi, data=e4.b[e4.b$comptrt=='P',], col=4)
points(nodi~percmivi,data=e4.b[e4.b$comptrt=='S',], col=5)
summary(lm(nodi~percmivi, data=e4.b))
#nitrification
scatterplot(nitrifd~percmivi, data=e4.b, boxplot=F, col=1, smooth=F, grid=F, xlab='%Mivi biomass', ylab='Net nitrification potential')
points(nitrifd~percmivi, data=e4.b[e4.b$comptrt=='P',], col=4)
points(nitrifd~percmivi,data=e4.b[e4.b$comptrt=='S',], col=5)
summary(lm(nitrifd~percmivi, data=e4.b))
#soilmoisture
scatterplot(soilmoi~percmivi, data=e4.b, boxplot=F, col=1, smooth=F, grid=F, xlab='%Mivi biomass', ylab='Soil moisture (%)')
points(soilmoi~percmivi, data=e4.b[e4.b$comptrt=='P',], col=4)
points(soilmoi~percmivi,data=e4.b[e4.b$comptrt=='S',], col=5)
summary(lm(soilmoi~percmivi, data=e4.b))

#######################################################
hist(e4.a2$miviperind, xlab='Per capita Mivi biomass (g/ind.)', main='Data=NPS, Mivi monos, no empty pots')
hist(e4.a2$nodi)
hist(e4.a2$nitrifd)
hist(e4.a2$soilmoi)

#Effect of Mivi biomass per capita on soil responses - NPS, no empty pots, Mv monoculture ok
#nitrate
scatterplot(nodi~log(miviperind+.5), data=e4.a2, boxplot=F, col=1, smooth=F, grid=F, xlab='Log(Per capita Mivi biomass (g/ind.))', ylab='Soil nitrate concentration')
points(nodi~log(miviperind+.5), data=e4.a2[e4.a2$comptrt=='N',], col=3)
points(nodi~log(miviperind+.5),data=e4.a2[e4.a2$comptrt=='P',], col=4)
points(nodi~log(miviperind+.5),data=e4.a2[e4.a2$comptrt=='S',], col=5)
summary(lm(nodi~log(miviperind+.5), data=e4.a2))
#nitrification
scatterplot(nitrifd~log(miviperind+.5), data=e4.a2, boxplot=F, col=1, smooth=F, grid=F, xlab='Log(Per capita Mivi biomass (g/ind.))', ylab='Net nitrification potential')
points(nitrifd~log(miviperind+.5), data=e4.a2[e4.a2$comptrt=='N',], col=3)
points(nitrifd~log(miviperind+.5), data=e4.a2[e4.a2$comptrt=='P',], col=4)
points(nitrifd~log(miviperind+.5),data=e4.a2[e4.a2$comptrt=='S',], col=5)
summary(lm(nitrifd~log(miviperind+.5), data=e4.a2))
#soilmoisture
scatterplot(soilmoi~log(miviperind+.5), data=e4.a2, boxplot=F, col=1, smooth=F, grid=F, xlab='Log(Per capita Mivi biomass (g/ind.))', ylab='Soil moisture (%)')
points(soilmoi~log(miviperind+.5), data=e4.a2[e4.a2$comptrt=='N',], col=3)
points(soilmoi~log(miviperind+.5), data=e4.a2[e4.a2$comptrt=='P',], col=4)
points(soilmoi~log(miviperind+.5),data=e4.a2[e4.a2$comptrt=='S',], col=5)
summary(lm(soilmoi~log(miviperind+.5), data=e4.a2))



