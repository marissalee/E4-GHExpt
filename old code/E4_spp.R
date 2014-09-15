##E4
#Effect of plant spp id on N conc and fluxes (w/o competition w/o densitytrt)

#############################
setwd("~/Desktop/Lachat Data/R stuff")
e4 <- read.table("E4_all.txt",header=T)
#make a comptrt=N dataset
e4.mivi <- e4[e4$comptrt=='N',]
e4.mivi1 <- e4.mivi[!is.na(e4.mivi[,6]),] #elim rows with NA in Mivi biom column
query1=is.na(e4.mivi1[,15:21]) #elim rows with NA in any of the soil columns
query2=apply(query1,1,sum)
e4.mivi2 <- e4.mivi1[query2==0,]
#make a comptrt=P dataset; need to include comptrt=N, densitytrt=L0 because this is a whole mivi pot
e4.pavi <- e4[e4$comptrt=='P',]
e4.pavi1 <- e4.pavi[!is.na(e4.pavi[,7]),] #elim rows with NA in Pavi biom column
query1=is.na(e4.pavi1[,15:21]) #elim rows with NA in any of the soil columns
query2=apply(query1,1,sum)
e4.pavi2 <- e4.pavi1[query2==0,]
e4.miviNL0 <- e4.mivi2[e4.mivi2$densitytrt=='L0',] #comptrt=N, densitytrt=L0
e4.pavi3 <- rbind(e4.pavi2,e4.miviNL0)
#make a comptrt=S dataset; need to include comptrt=N, densitytrt=L0 because this is a whole mivi pot
e4.sobi <- e4[e4$comptrt=='S',]
e4.sobi1 <- e4.sobi[!is.na(e4.sobi[,8]),] #elim rows with NA in Mivi biom column
query1=is.na(e4.sobi1[,15:21]) #elim rows with NA in any of the soil columns
query2=apply(query1,1,sum)
e4.sobi2 <- e4.sobi1[query2==0,]
e4.sobi3 <- rbind(e4.sobi2,e4.miviNL0)
#make a dataset with only densitytrt=L5 comptrt=P,S and densitytrt=L0 comptrt=N so that each species is in monoculture at highest density
e4.pavi4<-e4.pavi2[e4.pavi2$densitytrt=='L5',] #isolate monocultures L5
e4.sobi4<-e4.sobi2[e4.sobi2$densitytrt=='L5',] #isolate monocultures L5
e4.spp <- rbind(e4.miviNL0,e4.pavi4,e4.sobi4) #combine spp monocultures

#############################
library(lattice)
library(car)
############################
View(e4.spp) #Each spp w/100% density, no competition

e4.spp[e4.spp$bk==1,]-> bk1
e4.spp[e4.spp$bk==2,]-> bk2
e4.spp[e4.spp$bk==3,]-> bk3
e4.spp[e4.spp$bk==4,]-> bk4
e4.spp[e4.spp$bk==5,]-> bk5
e4.spp[e4.spp$bk==6,]-> bk6
e4.spp[e4.spp$bk==7,]-> bk7
e4.spp[e4.spp$bk==8,]-> bk8
e4.spp[e4.spp$bk==9,]-> bk9
e4.spp[e4.spp$bk==10,]-> bk10

plot(nhdi~comptrt, data=e4.spp)
points(nhdi~comptrt, data=bk1, col=2)
points(nhdi~comptrt, data=bk2, col=3)
points(nhdi~comptrt, data=bk3, col=4)
points(nhdi~comptrt, data=bk4, col=5)
points(nhdi~comptrt, data=bk5, col=6)
points(nhdi~comptrt, data=bk6, col=7)
points(nhdi~comptrt, data=bk7, col=8)
points(nhdi~comptrt, data=bk8, col=9)
points(nhdi~comptrt, data=bk9, col=10)
points(nhdi~comptrt, data=bk10, col=11)


plot(nodi~comptrt, data=e4.spp)
anova(lm(nodi~comptrt, data=e4.spp))

points(nodi~comptrt, data=bk1, col=2)
points(nodi~comptrt, data=bk2, col=3)
points(nodi~comptrt, data=bk3, col=4)
points(nodi~comptrt, data=bk4, col=5)
points(nodi~comptrt, data=bk5, col=6)
points(nodi~comptrt, data=bk6, col=7)
points(nodi~comptrt, data=bk7, col=8)
points(nodi~comptrt, data=bk8, col=9)
points(nodi~comptrt, data=bk9, col=10)
points(nodi~comptrt, data=bk10, col=11)

plot(ammonifd~comptrt, data=e4.spp)
points(ammonifd~comptrt, data=bk1, col=2)
points(ammonifd~comptrt, data=bk2, col=3)
points(ammonifd~comptrt, data=bk3, col=4)
points(ammonifd~comptrt, data=bk4, col=5)
points(ammonifd~comptrt, data=bk5, col=6)
points(ammonifd~comptrt, data=bk6, col=7)
points(ammonifd~comptrt, data=bk7, col=8)
points(ammonifd~comptrt, data=bk8, col=9)
points(ammonifd~comptrt, data=bk9, col=10)
points(ammonifd~comptrt, data=bk10, col=11)

plot(nitrifd~comptrt, data=e4.spp)
anova(lm(nitrifd~comptrt, data=e4.spp))

points(nitrifd~comptrt, data=bk1, col=2)
points(nitrifd~comptrt, data=bk2, col=3)
points(nitrifd~comptrt, data=bk3, col=4)
points(nitrifd~comptrt, data=bk4, col=5)
points(nitrifd~comptrt, data=bk5, col=6)
points(nitrifd~comptrt, data=bk6, col=7)
points(nitrifd~comptrt, data=bk7, col=8)
points(nitrifd~comptrt, data=bk8, col=9)
points(nitrifd~comptrt, data=bk9, col=10)
points(nitrifd~comptrt, data=bk10, col=11)

plot(minzd~comptrt, data=e4.spp)
points(minzd~comptrt, data=bk1, col=2)
points(minzd~comptrt, data=bk2, col=3)
points(minzd~comptrt, data=bk3, col=4)
points(minzd~comptrt, data=bk4, col=5)
points(minzd~comptrt, data=bk5, col=6)
points(minzd~comptrt, data=bk6, col=7)
points(minzd~comptrt, data=bk7, col=8)
points(minzd~comptrt, data=bk8, col=9)
points(minzd~comptrt, data=bk9, col=10)
points(minzd~comptrt, data=bk10, col=11)

plot(soilmoi~comptrt, data=e4.spp)
points(soilmoi~comptrt, data=bk1, col=2)
points(soilmoi~comptrt, data=bk2, col=3)
points(soilmoi~comptrt, data=bk3, col=4)
points(soilmoi~comptrt, data=bk4, col=5)
points(soilmoi~comptrt, data=bk5, col=6)
points(soilmoi~comptrt, data=bk6, col=7)
points(soilmoi~comptrt, data=bk7, col=8)
points(soilmoi~comptrt, data=bk8, col=9)
points(soilmoi~comptrt, data=bk9, col=10)
points(soilmoi~comptrt, data=bk10, col=11)
