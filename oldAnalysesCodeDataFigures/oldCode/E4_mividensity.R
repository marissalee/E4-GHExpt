#E4
#Effect of Mivi density on N conc and fluxes (w/o competition)

#############################
setwd("~/Desktop/Lachat Data/R stuff")
e4 <- read.table("E4_all.txt",header=T)
#make a comptrt=N dataset
e4.mivi <- e4[e4$comptrt=='N',]
e4.mivi1 <- e4.mivi[!is.na(e4.mivi[,6]),] #elim rows with NA in Mivi biom column
query1=is.na(e4.mivi1[,15:21]) #elim rows with NA in any of the soil columns
query2=apply(query1,1,sum)
e4.mivi2 <- e4.mivi1[query2==0,]
#############################
library(lattice)
library(car)
############################
View(e4.mivi2) #Density treatment, competitor treatment=None

#by densitytrt
e4.mivi2[e4.mivi2$bk==1,]-> bk1
e4.mivi2[e4.mivi2$bk==2,]-> bk2
e4.mivi2[e4.mivi2$bk==3,]-> bk3
e4.mivi2[e4.mivi2$bk==4,]-> bk4
e4.mivi2[e4.mivi2$bk==5,]-> bk5
e4.mivi2[e4.mivi2$bk==6,]-> bk6
e4.mivi2[e4.mivi2$bk==7,]-> bk7
e4.mivi2[e4.mivi2$bk==8,]-> bk8
e4.mivi2[e4.mivi2$bk==9,]-> bk9
e4.mivi2[e4.mivi2$bk==10,]-> bk10

e4.mivi2[e4.mivi2$densitytrt=='L0',]-> L0
e4.mivi2[e4.mivi2$densitytrt=='L1',]-> L1
e4.mivi2[e4.mivi2$densitytrt=='L2',]-> L2
e4.mivi2[e4.mivi2$densitytrt=='L3',]-> L3
e4.mivi2[e4.mivi2$densitytrt=='L4',]-> L4
e4.mivi2[e4.mivi2$densitytrt=='L5',]-> L5
L0L5<-rbind(L0,L5)
View(L0L5)

mean(L0$soilmoi)->a
mean(L1$soilmoi)->b
mean(L2$soilmoi)->c
mean(L3$soilmoi)->d
mean(L4$soilmoi)->e
mean(L5$soilmoi)->f
c(a,b,c,d,e,f)->means
c('L0','L1','L2','L3','L4','L5')->names
rbind(means,names)->vec
vec
barchart(vec)

plot(soilmoi~densitytrt, data=e4.mivi2)
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

plot(nhdi~densitytrt, data=e4.mivi2)
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

plot(nodi~densitytrt, data=L0L5)
summary(lm(nodi~densitytrt, data=L0L5))

plot(nodi~densitytrt, data=e4.mivi2)
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

plot(nitrifd~densitytrt, data=L0L5)
summary(lm(nitrifd~densitytrt, data=L0L5))

plot(nitrifd~densitytrt, data=e4.mivi2)
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

plot(ammonifd~densitytrt, data=e4.mivi2)
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

plot(minzd~densitytrt, data=e4.mivi2)
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

#by mivi biomass
scatterplot(soilmoi~mivi|bk, data=e4.mivi2, smooth=F, reg.line=F, boxplots="xy",legend.coords="bottomright")
summary(lm(soilmoi~mivi, data=e4.mivi2))
abline(lm(soilmoi~mivi, data=e4.mivi2), lty=2)

scatterplot(nhdi~mivi|bk, data=e4.mivi2, smooth=F, reg.line=F, boxplots="xy",legend.coords="bottomright")
summary(lm(nhdi~mivi, data=e4.mivi2))
abline(lm(nhdi~mivi, data=e4.mivi2), lty=2)

scatterplot(nodi ~ mivi|bk, data=e4.mivi2, smooth=F, reg.line=F, boxplots="xy", legend.coords="bottomright")
summary(lm(nodi~mivi, data=e4.mivi2))
abline(lm(nodi~mivi, data=e4.mivi2), lty=2)

scatterplot(totdi ~ mivi|bk, data=e4.mivi2, smooth=F, reg.line=F, boxplots="xy", legend.coords="bottomright")
summary(lm(totdi~mivi, data=e4.mivi2))
abline(lm(totdi~mivi, data=e4.mivi2), lty=2)

scatterplot(nitrifd ~ mivi|bk, data=e4.mivi2, smooth=F, reg.line=F, boxplots="xy",legend.coords="bottomright")
summary(lm(nitrifd~mivi, data=e4.mivi2))
abline(lm(nitrifd~mivi, data=e4.mivi2), lty=2)

scatterplot(ammonifd ~ mivi|bk, data=e4.mivi2, smooth=F, reg.line=F, boxplots="xy", legend.coords="bottomright")
summary(lm(ammonifd~mivi, data=e4.mivi2))
abline(lm(ammonifd~mivi, data=e4.mivi2), lty=2)

scatterplot(minzd ~ mivi|bk, data=e4.mivi2, smooth=F, reg.line=F,boxplots="xy", legend.coords="bottomright")
summary(lm(minzd~mivi, data=e4.mivi2))
abline(lm(minzd~mivi, data=e4.mivi2), lty=2)

###################################################


