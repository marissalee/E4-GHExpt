#E4 DeWalt Replacement Series... and other stuff

#############################
setwd("~/Desktop/Lachat Data/R stuff")

#############################
e4l <- read.table("e4_dw_long.txt",header=T)
query1=is.na(e4l[,24:30]) #elim rows with NA in any of the soil columns
query2=apply(query1,1,sum)
e4.dwl <- e4l[query2==0,]
View(e4.dwl)
#############################

e4 <- read.table("e4_dw.txt",header=T)
query1=is.na(e4[,24:30]) #elim rows with NA in any of the soil columns
query2=apply(query1,1,sum)
e4.dw <- e4[query2==0,]

#make a comptrt=N,P dataset with empty and monoculture pots
e4.dw1 <- e4.dw[!e4.dw$comptrt=='S',]
View(e4.dw1)

#make a comptrt=N,S dataset with empty and monoculture pots
e4.dw2 <- e4.dw[!e4.dw$comptrt=='P',]

#make a comptrt=N dataset with empty and monoculture pots
e4.dwn <- e4.dw1[!e4.dw1$comptrt=='P',]
View(e4.dwn)
e4.dwn0 <- e4.dwn[e4.dwn$densitytrt=='L0',]

#make a comptrt=P dataset with empty and monoculture pots
e4.dwp1 <- e4.dw1[!e4.dw1$comptrt=='N',]
e4.dwp <- rbind(e4.dwn0,e4.dwp1)
View(e4.dwp)
#make a comptrt=S dataset with empty and monoculture pots
e4.dws1 <- e4.dw2[!e4.dw2$comptrt=='N',]
e4.dws <- rbind(e4.dwn0,e4.dws1)

#############################
library(lattice)
library(ggplot2)
#############################

dwl <- summarySE(e4.dwl, measurevar="biom", groupvars=c("biomtype","mivipot","comptrt"), na.rm=T)
dwl

#rows that I want to keep for Pavi competition
#biomtype=mivi, mivipot=all, comptrt=N,P
dwl1<-dwl[dwl$biomtype=='mivi',]
dwl1
dwl1.1<- dwl1[!dwl1$comptrt=='S',]
dwl1.1
#biomtype=pavi, mivipot=all, comptrt=P
dwl2<-dwl[dwl$biomtype=='pavi',]
dwl2
dwl2.2<- dwl2[dwl2$comptrt=='P',]
dwl2.2

#combine rows for pavi comp_dw plot
pavicomp<-rbind(dwl1.1,dwl2.2)
pavicomp

pavicomp[selectedrows,] <- 'mivi0'
selectedrows

ggplot(pavicomp, aes(x=mivipot, y=biom, colour=biomtype, group=biomtype) + 
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin=biom-se, ymax=biom+se),
                  width=.1))

##########################
########################## pavi competition
m <- summarySE(e4.dwp, measurevar="mivi", groupvars="mivipot")
m[1,3] <- 0
m
p <- summarySE(e4.dwp, measurevar="pavi", groupvars="mivipot")
p[6,3] <- 0
p

View(e4.dwn)
m0 <- summarySE(e4.dwn, measurevar="mivi", groupvars="mivipot", na.rm=T)
m0

plot(mivi~mivipot, data=m, ylim=c(0,100), ylab='Plant biomass (g)', xlab='Initial proportion of Mivi', col=3, type='o')
points(pavi~mivipot, data=p, col=4, type='o')
points(mivi~mivipot, data=m0, col=6, type='o')
abline(v=50,lty=2)
legend(65,100, c("mivi", "pavi", "mivi-alone"), col = c(3,4,6), lty=1, pch = 1)

# Standard error of the mean
ggplot(m, aes(x=mivipot, y=mivi)) + 
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin=mivi-se, ymax=mivi+se),
                  width=.1)


########################## sobi competition
m2 <- summarySE(e4.dws, measurevar="mivi", groupvars="mivipot")
m2[1,3] <- 0
m2
s <- summarySE(e4.dws, measurevar="sobi", groupvars="mivipot")
s[6,3] <- 0
s

m0 <- summarySE(e4.dwn, measurevar="mivi", groupvars="mivipot", na.rm=T)
m0

plot(mivi~mivipot, data=m2, ylim=c(0,100), ylab='Plant biomass (g)', xlab='Initial proportion of Mivi', col=3, type='o')
points(sobi~mivipot, data=s, col=5, type='o')
points(mivi~mivipot, data=m0, col=6, type='o')
abline(v=50,lty=2)
legend(65,100, c("mivi", "sobi", "mivi-alone"), col = c(3,5,6), lty=1, pch = 1)





plot(mivi~mivipot, data=e4.dwn) #mivi biomass, grown alone
xyplot(mivi~mivipot, data=e4.dwp) #mivi biomass, grown with pavi
xyplot(pavi~mivipot, data=e4.dwp) #pavi biomass, grown with mivi

#x=mivipot

#y1=mivi
#0/6 Mivi, 6/6 Pavi (L5)
#1/6 Mivi, 5/6 Pavi (L4)
#2/6 Mivi, 4/6 Pavi (L3)
#4/6 Mivi, 2/6 Pavi (L2)
#5/6 Mivi, 1/6 Pavi (L1)
#6/6 Mivi, 0/6 Pavi (L0)

#y2=pavi
#0/6 Mivi, 6/6 Pavi (L5)
#1/6 Mivi, 5/6 Pavi (L4)
#2/6 Mivi, 4/6 Pavi (L3)
#4/6 Mivi, 2/6 Pavi (L2)
#5/6 Mivi, 1/6 Pavi (L1)
#6/6 Mivi, 0/6 Pavi (L0)


