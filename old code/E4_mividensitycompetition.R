#E4
#Effect of Mivi density on N conc and fluxes (w/ competition)

################################################
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
###################################################

View(e4.pavi3)#Density treatment, competitor treatment=Pavi
e4.pavi3[e4.pavi3$bk==1,]-> bk1
e4.pavi3[e4.pavi3$bk==2,]-> bk2
e4.pavi3[e4.pavi3$bk==3,]-> bk3
e4.pavi3[e4.pavi3$bk==4,]-> bk4
e4.pavi3[e4.pavi3$bk==5,]-> bk5
e4.pavi3[e4.pavi3$bk==6,]-> bk6
e4.pavi3[e4.pavi3$bk==7,]-> bk7
e4.pavi3[e4.pavi3$bk==8,]-> bk8
e4.pavi3[e4.pavi3$bk==9,]-> bk9
e4.pavi3[e4.pavi3$bk==10,]-> bk10

plot(nhdi~densitytrt, data=e4.pavi3, xlab='densitytrt - pavi')
points(nhdi~densitytrt, data=bk1, col=2)
points(nhdi~densitytrt, data=bk2, col=3)
points(nhdi~densitytrt, data=bk3, col=4)
points(nhdi~densitytrt, data=bk4, col=5)
points(nhdi~densitytrt, data=bk5, col=6)
points(nhdi~densitytrt, data=bk6, col=7)
points(nhdi~densitytrt, data=bk7, col=8)
points(nhdi~densitytrt, data=bk8, col=9)
points(nhdi~densitytrt, data=bk9, col=10)
points(nhdi~densitytrt, data=bk10, col=11)

plot(nodi~densitytrt, data=e4.pavi3, xlab='densitytrt - pavi')
points(nodi~densitytrt, data=bk1, col=2)
points(nodi~densitytrt, data=bk2, col=3)
points(nodi~densitytrt, data=bk3, col=4)
points(nodi~densitytrt, data=bk4, col=5)
points(nodi~densitytrt, data=bk5, col=6)
points(nodi~densitytrt, data=bk6, col=7)
points(nodi~densitytrt, data=bk7, col=8)
points(nodi~densitytrt, data=bk8, col=9)
points(nodi~densitytrt, data=bk9, col=10)
points(nodi~densitytrt, data=bk10, col=11)

plot(ammonifd~densitytrt, data=e4.pavi3, xlab='densitytrt - pavi')
points(ammonifd~densitytrt, data=bk1, col=2)
points(ammonifd~densitytrt, data=bk2, col=3)
points(ammonifd~densitytrt, data=bk3, col=4)
points(ammonifd~densitytrt, data=bk4, col=5)
points(ammonifd~densitytrt, data=bk5, col=6)
points(ammonifd~densitytrt, data=bk6, col=7)
points(ammonifd~densitytrt, data=bk7, col=8)
points(ammonifd~densitytrt, data=bk8, col=9)
points(ammonifd~densitytrt, data=bk9, col=10)
points(ammonifd~densitytrt, data=bk10, col=11)

View(e4.pavi3)
plot(nitrifd~percmivi, data=e4.pavi3, xlab='mivibiomass - pavi')
points(nitrifd~densitytrt, data=bk1, col=2)
points(nitrifd~densitytrt, data=bk2, col=3)
points(nitrifd~densitytrt, data=bk3, col=4)
points(nitrifd~densitytrt, data=bk4, col=5)
points(nitrifd~densitytrt, data=bk5, col=6)
points(nitrifd~densitytrt, data=bk6, col=7)
points(nitrifd~densitytrt, data=bk7, col=8)
points(nitrifd~densitytrt, data=bk8, col=9)
points(nitrifd~densitytrt, data=bk9, col=10)
points(nitrifd~densitytrt, data=bk10, col=11)

plot(minzd~densitytrt, data=e4.pavi3, xlab='densitytrt - pavi')
points(minzd~densitytrt, data=bk1, col=2)
points(minzd~densitytrt, data=bk2, col=3)
points(minzd~densitytrt, data=bk3, col=4)
points(minzd~densitytrt, data=bk4, col=5)
points(minzd~densitytrt, data=bk5, col=6)
points(minzd~densitytrt, data=bk6, col=7)
points(minzd~densitytrt, data=bk7, col=8)
points(minzd~densitytrt, data=bk8, col=9)
points(minzd~densitytrt, data=bk9, col=10)
points(minzd~densitytrt, data=bk10, col=11)

plot(soilmoi~densitytrt, data=e4.pavi3, xlab='densitytrt - pavi')
points(soilmoi~densitytrt, data=bk1, col=2)
points(soilmoi~densitytrt, data=bk2, col=3)
points(soilmoi~densitytrt, data=bk3, col=4)
points(soilmoi~densitytrt, data=bk4, col=5)
points(soilmoi~densitytrt, data=bk5, col=6)
points(soilmoi~densitytrt, data=bk6, col=7)
points(soilmoi~densitytrt, data=bk7, col=8)
points(soilmoi~densitytrt, data=bk8, col=9)
points(soilmoi~densitytrt, data=bk9, col=10)
points(soilmoi~densitytrt, data=bk10, col=11)

scatterplot(nhdi ~ percmivi|bk, data=e4.pavi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(0,140),xlab="Percent Mivi w/ Pavi", legend.coords="bottomright")
summary(lm(nhdi~percmivi, data=e4.pavi3))
abline(lm(nhdi~percmivi, data=e4.pavi3), lty=2)

scatterplot(nodi ~ percmivi|bk, data=e4.pavi3, smooth=F, reg.line=F, boxplots="xy",xlim=c(0,140), xlab="Percent Mivi w/ Pavi",legend.coords="bottomright")
summary(lm(nodi~percmivi, data=e4.pavi3))
abline(lm(nodi~percmivi, data=e4.pavi3), lty=2)

scatterplot(ammonifd ~ percmivi|bk, data=e4.pavi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(0,140),xlab="Percent Mivi w/ Pavi",legend.coords="bottomright")
summary(lm(ammonifd~percmivi, data=e4.pavi3))
abline(lm(ammonifd~percmivi, data=e4.pavi3), lty=2)

scatterplot(nitrifd ~ percmivi|bk, data=e4.pavi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(0,140),xlab="Percent Mivi w/ Pavi",legend.coords="bottomright")
summary(lm(nitrifd~percmivi, data=e4.pavi3))
abline(lm(nitrifd~percmivi, data=e4.pavi3), lty=2)

scatterplot(minzd ~ percmivi|bk, data=e4.pavi3, smooth=F, reg.line=F,boxplots="xy", xlim=c(0,140),xlab="Percent Mivi w/ Pavi",legend.coords="bottomright")
summary(lm(minzd~percmivi, data=e4.pavi3))
abline(lm(minzd~percmivi, data=e4.pavi3), lty=2)

scatterplot(soilmoi ~ percmivi|bk, data=e4.pavi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(0,140),xlab="Percent Mivi w/ Pavi", legend.coords="bottomright")
summary(lm(soilmoi~percmivi, data=e4.pavi3))
abline(lm(soilmoi~percmivi, data=e4.pavi3), lty=2)

###################################################

View(e4.sobi3) #Density treatment, competitor treatment=Sobi
hist(e4.sobi3$percmivi, breaks=50)

View(e4.sobi3)#Density treatment, competitor treatment=Pavi
e4.sobi3[e4.sobi3$bk==1,]-> bk1
e4.sobi3[e4.sobi3$bk==2,]-> bk2
e4.sobi3[e4.sobi3$bk==3,]-> bk3
e4.sobi3[e4.sobi3$bk==4,]-> bk4
e4.sobi3[e4.sobi3$bk==5,]-> bk5
e4.sobi3[e4.sobi3$bk==6,]-> bk6
e4.sobi3[e4.sobi3$bk==7,]-> bk7
e4.sobi3[e4.sobi3$bk==8,]-> bk8
e4.sobi3[e4.sobi3$bk==9,]-> bk9
e4.sobi3[e4.sobi3$bk==10,]-> bk10

plot(nhdi~densitytrt, data=e4.sobi3, xlab='densitytrt - sobi')
points(nhdi~densitytrt, data=bk1, col=2)
points(nhdi~densitytrt, data=bk2, col=3)
points(nhdi~densitytrt, data=bk3, col=4)
points(nhdi~densitytrt, data=bk4, col=5)
points(nhdi~densitytrt, data=bk5, col=6)
points(nhdi~densitytrt, data=bk6, col=7)
points(nhdi~densitytrt, data=bk7, col=8)
points(nhdi~densitytrt, data=bk8, col=9)
points(nhdi~densitytrt, data=bk9, col=10)
points(nhdi~densitytrt, data=bk10, col=11)

plot(nodi~densitytrt, data=e4.sobi3, xlab='densitytrt - sobi')
points(nodi~densitytrt, data=bk1, col=2)
points(nodi~densitytrt, data=bk2, col=3)
points(nodi~densitytrt, data=bk3, col=4)
points(nodi~densitytrt, data=bk4, col=5)
points(nodi~densitytrt, data=bk5, col=6)
points(nodi~densitytrt, data=bk6, col=7)
points(nodi~densitytrt, data=bk7, col=8)
points(nodi~densitytrt, data=bk8, col=9)
points(nodi~densitytrt, data=bk9, col=10)
points(nodi~densitytrt, data=bk10, col=11)

plot(ammonifd~densitytrt, data=e4.sobi3, xlab='densitytrt - sobi')
points(ammonifd~densitytrt, data=bk1, col=2)
points(ammonifd~densitytrt, data=bk2, col=3)
points(ammonifd~densitytrt, data=bk3, col=4)
points(ammonifd~densitytrt, data=bk4, col=5)
points(ammonifd~densitytrt, data=bk5, col=6)
points(ammonifd~densitytrt, data=bk6, col=7)
points(ammonifd~densitytrt, data=bk7, col=8)
points(ammonifd~densitytrt, data=bk8, col=9)
points(ammonifd~densitytrt, data=bk9, col=10)
points(ammonifd~densitytrt, data=bk10, col=11)

P<-e4.sobi3[e4.sobi3$comptrt=='S',]
View(P)
plot(nitrifd~log(mivi), data=P, xlab='mivibiomass - sobi')
plot(nitrifd~log(percmivi), data=P, xlab='percmivi - sobi')
summary(lm(nitrifd~log(percmivi+1), data=P))

plot(nitrifd~densitytrt, data=e4.sobi3, xlab='densitytrt - sobi')
points(nitrifd~densitytrt, data=bk1, col=2)
points(nitrifd~densitytrt, data=bk2, col=3)
points(nitrifd~densitytrt, data=bk3, col=4)
points(nitrifd~densitytrt, data=bk4, col=5)
points(nitrifd~densitytrt, data=bk5, col=6)
points(nitrifd~densitytrt, data=bk6, col=7)
points(nitrifd~densitytrt, data=bk7, col=8)
points(nitrifd~densitytrt, data=bk8, col=9)
points(nitrifd~densitytrt, data=bk9, col=10)
points(nitrifd~densitytrt, data=bk10, col=11)

plot(minzd~densitytrt, data=e4.sobi3, xlab='densitytrt - sobi')
points(minzd~densitytrt, data=bk1, col=2)
points(minzd~densitytrt, data=bk2, col=3)
points(minzd~densitytrt, data=bk3, col=4)
points(minzd~densitytrt, data=bk4, col=5)
points(minzd~densitytrt, data=bk5, col=6)
points(minzd~densitytrt, data=bk6, col=7)
points(minzd~densitytrt, data=bk7, col=8)
points(minzd~densitytrt, data=bk8, col=9)
points(minzd~densitytrt, data=bk9, col=10)
points(minzd~densitytrt, data=bk10, col=11)

plot(soilmoi~densitytrt, data=e4.sobi3, xlab='densitytrt - sobi')
points(soilmoi~densitytrt, data=bk1, col=2)
points(soilmoi~densitytrt, data=bk2, col=3)
points(soilmoi~densitytrt, data=bk3, col=4)
points(soilmoi~densitytrt, data=bk4, col=5)
points(soilmoi~densitytrt, data=bk5, col=6)
points(soilmoi~densitytrt, data=bk6, col=7)
points(soilmoi~densitytrt, data=bk7, col=8)
points(soilmoi~densitytrt, data=bk8, col=9)
points(soilmoi~densitytrt, data=bk9, col=10)
points(soilmoi~densitytrt, data=bk10, col=11)


scatterplot(nhdi ~ percmivi|bk, data=e4.sobi3, smooth=F, reg.line=F, boxplots="xy" ,xlim=c(0,140), xlab="Percent Mivi w/ Sobi", legend.coords="bottomright")
abline(lm(nhdi~percmivi, data=e4.sobi3), lty=2)
summary(lm(nhdi~percmivi, data=e4.sobi3))

scatterplot(nodi ~ percmivi|bk, data=e4.sobi3, smooth=F, reg.line=F, boxplots="xy",xlim=c(0,140), xlab="Percent Mivi w/ Sobi",legend.coords="bottomright")
abline(lm(nodi~percmivi, data=e4.sobi3), lty=2)
summary(lm(nodi~percmivi, data=e4.sobi3))

scatterplot(ammonifd ~ percmivi|bk, data=e4.sobi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(0,140),xlab="Percent Mivi w/ Sobi", legend.coords="bottomright")
abline(lm(ammonifd~percmivi, data=e4.sobi3), lty=2)
summary(lm(ammonifd~percmivi, data=e4.sobi3))

scatterplot(nitrifd ~ percmivi|bk, data=e4.sobi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(0,140),xlab="Percent Mivi w/ Sobi",legend.coords="bottomright")
abline(lm(nitrifd~percmivi, data=e4.sobi3), lty=2)
summary(lm(nitrifd~percmivi, data=e4.sobi3))

scatterplot(minzd ~ percmivi|bk, data=e4.sobi3, smooth=F, reg.line=F,boxplots="xy", xlim=c(0,140), xlab="Percent Mivi w/ Sobi",legend.coords="bottomright")
abline(lm(minzd~percmivi, data=e4.sobi3), lty=2)
summary(lm(minzd~percmivi, data=e4.sobi3))

scatterplot(soilmoi ~ percmivi|bk, data=e4.sobi3, smooth=F, reg.line=F,boxplots="xy", xlim=c(0,140), xlab="Percent Mivi w/ Sobi",legend.coords="bottomright")
abline(lm(soilmoi~percmivi, data=e4.sobi3), lty=2)
summary(lm(soilmoi~percmivi, data=e4.sobi3))
