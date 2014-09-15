#E4 data analysis - Notes

setwd("~/Desktop/Lachat Data/R stuff")

#read in veg and soil data
e4v <- read.table("E4_veg.txt",header=T)
e4m <- read.table("E4_minz.txt",header=T)

#combine veg and soil data
e4.1 <- cbind(e4v,e4m)
e4 <- cbind(e4.1[,1:14],e4.1[,20:26])
head(e4)

########################

#make a comptrt=N dataset
e4.mivi <- e4[e4$comptrt=='N',]
e4.mivi1 <- e4.mivi[!is.na(e4.mivi[,6]),] #elim rows with NA in Mivi biom column
#e4.mivi1
query1=is.na(e4.mivi1[,15:21]) #elim rows with NA in any of the soil columns
query2=apply(query1,1,sum)
e4.mivi2 <- e4.mivi1[query2==0,]
#e4.mivi2

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

##################################
#Can I explain N conc and fluxes based on plant biomass alone?  does it help to include spp?

xyplot(nhdi ~ total, data=e4, smooth=F, reg.line=F, legend.coords="bottomright")
abline(lm(nhdi~total, data=e4), lty=2)
summary(lm(nhdi~total, data=e4))

xyplot(nodi ~ total, data=e4, smooth=F, reg.line=F, legend.coords="bottomright")
abline(lm(nhdi~total, data=e4), lty=2)
summary(lm(nhdi~total, data=e4))

xyplot(totdi ~ total, data=e4, smooth=F, reg.line=F, legend.coords="bottomright")
abline(lm(nhdi~total, data=e4), lty=2)
summary(lm(nhdi~total, data=e4))

xyplot(percnodi ~ total, data=e4, smooth=F, reg.line=F, legend.coords="bottomright")
abline(lm(nhdi~total, data=e4), lty=2)
summary(lm(nhdi~total, data=e4))

xyplot(nitrifd ~ total|comptrt, data=e4, smooth=F, reg.line=F, legend.coords="bottomright")
abline(lm(nhdi~total, data=e4), lty=2)
summary(lm(nhdi~total, data=e4))

xyplot(minzd ~ total, data=e4, smooth=F, reg.line=F, legend.coords="bottomright")
abline(lm(nhdi~total, data=e4), lty=2)
summary(lm(nhdi~total, data=e4))


##################################

#make a dataset with only densitytrt=L5 comptrt=P,S and densitytrt=L0 comptrt=N so that each species is in monoculture at highest density
e4.pavi4<-e4.pavi2[e4.pavi2$densitytrt=='L5',] #isolate monocultures L5
e4.sobi4<-e4.sobi2[e4.sobi2$densitytrt=='L5',] #isolate monocultures L5
e4.spp <- rbind(e4.miviNL0,e4.pavi4,e4.sobi4) #combine spp monocultures
View(e4.spp)

library(car)
library(lattice)
xyplot(nhdi ~ comptrt, data=e4.spp, smooth=F, reg.line=F, legend.coords="bottomright")
abline(lm(nhdi~comptrt, data=e4.spp), lty=2)
summary(lm(nhdi~comptrt, data=e4.spp))

xyplot(nodi ~ comptrt, data=e4.spp, smooth=F, reg.line=F, boxplots="xy", legend.coords="bottomright")
abline(lm(nodi~comptrt, data=e4.spp), lty=2)
summary(lm(nodi~comptrt, data=e4.spp))

xyplot(totdi ~ comptrt, data=e4.spp, smooth=F, reg.line=F, boxplots="xy", legend.coords="bottomright")
abline(lm(totdi~comptrt, data=e4.spp), lty=2)
summary(lm(totdi~comptrt, data=e4.spp))

xyplot(percnodi ~ comptrt, data=e4.spp, smooth=F, reg.line=F, boxplots="xy", legend.coords="bottomright")
abline(lm(percnodi~comptrt, data=e4.spp), lty=2)
summary(lm(percnodi~comptrt, data=e4.spp))

xyplot(ammonifd ~ comptrt, data=e4.spp, smooth=F, reg.line=F, boxplots="xy", legend.coords="bottomright")
abline(lm(ammonifd~comptrt, data=e4.spp), lty=2)
summary(lm(ammonifd~comptrt, data=e4.spp))

xyplot(nitrifd ~ comptrt, data=e4.spp, smooth=F, reg.line=F, boxplots="xy", legend.coords="bottomright")
abline(lm(nitrifd~comptrt, data=e4.spp), lty=2)
summary(lm(nitrifd~comptrt, data=e4.spp))

xyplot(minzd ~ comptrt, data=e4.spp, smooth=F, reg.line=F, boxplots="xy", legend.coords="bottomright")
abline(lm(minzd~comptrt, data=e4.spp), lty=2)
summary(lm(minzd~comptrt, data=e4.spp))


#make a dataset with Mivi and compt in monoculture and same biomass (instead of density)
########################

View(e4.mivi2)
#Density treatment, competitor treatment=None

library(car)
scatterplot(nhdi ~ mivi|bk, data=e4.mivi2, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,65), legend.coords="bottomright")
abline(lm(nhdi~mivi, data=e4.mivi2), lty=2)
summary(lm(nhdi~mivi, data=e4.mivi2))

#by density trt
scatterplot(nhdi ~ densitytrt|bk, data=e4.mivi2, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,65), legend.coords="bottomright")
summary(lm(nhdi~densitytrt, data=e4.mivi2))
anova(lm(nhdi~densitytrt, data=e4.mivi2))

scatterplot(nodi ~ mivi|bk, data=e4.mivi2, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,65), legend.coords="bottomright")
abline(lm(nodi~mivi, data=e4.mivi2), lty=2)
summary(lm(nodi~mivi, data=e4.mivi2))

#by density trt
scatterplot(nodi ~ densitytrt|bk, data=e4.mivi2, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,65), legend.coords="bottomright")
summary(lm(nodi~densitytrt, data=e4.mivi2))
anova(lm(nodi~densitytrt, data=e4.mivi2))

scatterplot(totdi ~ mivi|bk, data=e4.mivi2, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,65), legend.coords="bottomright")
abline(lm(totdi~mivi, data=e4.mivi2), lty=2)
summary(lm(totdi~mivi, data=e4.mivi2))

#by density trt
scatterplot(totdi ~ densitytrt|bk, data=e4.mivi2, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,65), legend.coords="bottomright")
summary(lm(totdi~densitytrt, data=e4.mivi2))
anova(lm(totdi~densitytrt, data=e4.mivi2))

scatterplot(nitrifd ~ mivi|bk, data=e4.mivi2, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,65), legend.coords="bottomright")
abline(lm(nitrifd~mivi, data=e4.mivi2), lty=2)
summary(lm(nitrifd~mivi, data=e4.mivi2))

#by density trt
scatterplot(nitrifd ~ densitytrt|bk, data=e4.mivi2, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,65), legend.coords="bottomright")
summary(lm(nitrifd~densitytrt, data=e4.mivi2))
anova(lm(nitrifd~densitytrt, data=e4.mivi2))

scatterplot(ammonifd ~ mivi|bk, data=e4.mivi2, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,65), legend.coords="bottomright")
abline(lm(ammonifd~mivi, data=e4.mivi2), lty=2)
summary(lm(ammonifd~mivi, data=e4.mivi2))

#by density trt
scatterplot(ammonifd ~ densitytrt|bk, data=e4.mivi2, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,65), legend.coords="bottomright")
summary(lm(ammonifd~densitytrt, data=e4.mivi2))
anova(lm(ammonifd~densitytrt, data=e4.mivi2))

scatterplot(minzd ~ mivi|bk, data=e4.mivi2, smooth=F, reg.line=F,boxplots="xy", xlim=c(-5,65), legend.coords="bottomright")
abline(lm(minzd~mivi, data=e4.mivi2), lty=2)
summary(lm(minzd~mivi, data=e4.mivi2))

#by density trt
scatterplot(minzd ~ densitytrt|bk, data=e4.mivi2, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,65), legend.coords="bottomright")
summary(lm(minzd~densitytrt, data=e4.mivi2))
anova(lm(minzd~densitytrt, data=e4.mivi2))

###################################################

View(e4.pavi3)
#Density treatment, competitor treatment=Pavi

scatterplot(nhdi ~ percmivi|bk, data=e4.pavi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,120), xlab="Percent Mivi w/ Pavi", legend.coords="bottomright")
abline(lm(nhdi~percmivi, data=e4.pavi3), lty=2)
summary(lm(nhdi~percmivi, data=e4.pavi3))

#by density trt
scatterplot(nhdi ~ densitytrt|bk, data=e4.pavi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,65), legend.coords="bottomright")
summary(lm(nhdi~densitytrt, data=e4.pavi2))
anova(lm(nhdi~densitytrt, data=e4.pavi2))

scatterplot(nodi ~ percmivi|bk, data=e4.pavi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,120), xlab="Percent Mivi w/ Pavi",legend.coords="bottomright")
abline(lm(nodi~percmivi, data=e4.pavi3), lty=2)
summary(lm(nodi~percmivi, data=e4.pavi3))

#by density trt
scatterplot(nodi ~ densitytrt|bk, data=e4.pavi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,65), legend.coords="bottomright")
summary(lm(nodi~densitytrt, data=e4.pavi2))
anova(lm(nodi~densitytrt, data=e4.pavi2))

scatterplot(totdi ~ percmivi|bk, data=e4.pavi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,120), xlab="Percent Mivi w/ Pavi",legend.coords="bottomright")
abline(lm(totdi~percmivi, data=e4.pavi3), lty=2)
summary(lm(totdi~percmivi, data=e4.pavi3))

#by density trt
scatterplot(totdi ~ densitytrt|bk, data=e4.pavi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,65), legend.coords="bottomright")
summary(lm(totdi~densitytrt, data=e4.pavi2))
anova(lm(totdi~densitytrt, data=e4.pavi2))

scatterplot(nitrifd ~ percmivi|bk, data=e4.pavi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,120), xlab="Percent Mivi w/ Pavi",legend.coords="bottomright")
abline(lm(nitrifd~percmivi, data=e4.pavi3), lty=2)
summary(lm(nitrifd~percmivi, data=e4.pavi3))

#by density trt
scatterplot(nitrifd ~ densitytrt|bk, data=e4.pavi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,65), legend.coords="bottomright")
summary(lm(nitrifd~densitytrt, data=e4.pavi2))
anova(lm(nitrifd~densitytrt, data=e4.pavi2))

scatterplot(ammonifd ~ percmivi|bk, data=e4.pavi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,120), xlab="Percent Mivi w/ Pavi",legend.coords="bottomright")
abline(lm(ammonifd~percmivi, data=e4.pavi3), lty=2)
summary(lm(ammonifd~percmivi, data=e4.pavi3))

#by density trt
scatterplot(ammonifd ~ densitytrt|bk, data=e4.pavi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,65), legend.coords="bottomright")
summary(lm(ammonifd~densitytrt, data=e4.pavi2))
anova(lm(ammonifd~densitytrt, data=e4.pavi2))

scatterplot(minzd ~ percmivi|bk, data=e4.pavi3, smooth=F, reg.line=F,boxplots="xy", xlim=c(-5,120), xlab="Percent Mivi w/ Pavi",legend.coords="bottomright")
abline(lm(minzd~percmivi, data=e4.pavi3), lty=2)
summary(lm(minzd~percmivi, data=e4.pavi3))

#by density trt
scatterplot(minzd ~ densitytrt|bk, data=e4.pavi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,65), legend.coords="bottomright")
summary(lm(minzd~densitytrt, data=e4.pavi2))
anova(lm(minzd~densitytrt, data=e4.pavi2))



###################################################

View(e4.sobi3)
#Density treatment, competitor treatment=Sobi
hist(e4.sobi3$percmivi, breaks=50)

scatterplot(nhdi ~ percmivi|bk, data=e4.sobi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,120), xlab="Percent Mivi w/ Sobi", legend.coords="bottomright")
abline(lm(nhdi~percmivi, data=e4.sobi3), lty=2)
summary(lm(nhdi~percmivi, data=e4.sobi3))

#by density trt
scatterplot(nhdi ~ densitytrt|bk, data=e4.sobi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,65), legend.coords="bottomright")
summary(lm(nhdi~densitytrt, data=e4.sobi2))
anova(lm(nhdi~densitytrt, data=e4.sobi2))

scatterplot(nodi ~ percmivi|bk, data=e4.sobi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,120), xlab="Percent Mivi w/ Sobi",legend.coords="bottomright")
abline(lm(nodi~percmivi, data=e4.sobi3), lty=2)
summary(lm(nodi~percmivi, data=e4.sobi3))

#by density trt
scatterplot(nodi ~ densitytrt|bk, data=e4.sobi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,65), legend.coords="bottomright")
summary(lm(nodi~densitytrt, data=e4.sobi2))
anova(lm(nodi~densitytrt, data=e4.sobi2))

scatterplot(totdi ~ percmivi|bk, data=e4.sobi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,120),xlab="Percent Mivi w/ Sobi", legend.coords="bottomright")
abline(lm(totdi~percmivi, data=e4.sobi3), lty=2)
summary(lm(totdi~percmivi, data=e4.sobi3))

#by density trt
scatterplot(totdi ~ densitytrt|bk, data=e4.sobi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,65), legend.coords="bottomright")
summary(lm(totdi~densitytrt, data=e4.sobi2))
anova(lm(totdi~densitytrt, data=e4.sobi2))

scatterplot(nitrifd ~ percmivi|bk, data=e4.sobi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,120), xlab="Percent Mivi w/ Sobi",legend.coords="bottomright")
abline(lm(nitrifd~percmivi, data=e4.sobi3), lty=2)
summary(lm(nitrifd~percmivi, data=e4.sobi3))

#by density trt
scatterplot(nitrifd ~ densitytrt|bk, data=e4.sobi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,65), legend.coords="bottomright")
summary(lm(nitrifd~densitytrt, data=e4.sobi2))
anova(lm(nitrifd~densitytrt, data=e4.sobi2))

scatterplot(ammonifd ~ percmivi|bk, data=e4.sobi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,120),xlab="Percent Mivi w/ Sobi", legend.coords="bottomright")
abline(lm(ammonifd~percmivi, data=e4.sobi3), lty=2)
summary(lm(ammonifd~percmivi, data=e4.sobi3))

scatterplot(minzd ~ percmivi|bk, data=e4.sobi3, smooth=F, reg.line=F,boxplots="xy", xlim=c(-5,120), xlab="Percent Mivi w/ Sobi",legend.coords="bottomright")
abline(lm(minzd~percmivi, data=e4.sobi3), lty=2)
summary(lm(minzd~percmivi, data=e4.sobi3))

#by density trt
scatterplot(minzd ~ densitytrt|bk, data=e4.sobi3, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,65), legend.coords="bottomright")
summary(lm(minzd~densitytrt, data=e4.sobi2))
summary(aov(minzd~densitytrt, data=e4.sobi2))

##################
#changed to sobi2 to exclude 100% Mv values

scatterplot(nhdi ~ percmivi|bk, data=e4.sobi2, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,25),xlab="Percent Mivi w/ Sobi", legend.coords="bottomright")
abline(lm(nhdi~percmivi, data=e4.sobi2), lty=2)
summary(lm(nhdi~percmivi, data=e4.sobi2))

scatterplot(nodi ~ percmivi|bk, data=e4.sobi2, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,25), xlab="Percent Mivi w/ Sobi",legend.coords="bottomright")
abline(lm(nodi~percmivi, data=e4.sobi2), lty=2)
summary(lm(nodi~percmivi, data=e4.sobi2))

scatterplot(totdi ~ percmivi|bk, data=e4.sobi2, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,25), xlab="Percent Mivi w/ Sobi",legend.coords="bottomright")
abline(lm(totdi~percmivi, data=e4.sobi2), lty=2)
summary(lm(totdi~percmivi, data=e4.sobi2))

scatterplot(nitrifd ~ percmivi|bk, data=e4.sobi2, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,25), xlab="Percent Mivi w/ Sobi",legend.coords="bottomright")
abline(lm(nitrifd~percmivi, data=e4.sobi2), lty=2)
summary(lm(nitrifd~percmivi, data=e4.sobi2))

scatterplot(ammonifd ~ percmivi|bk, data=e4.sobi2, smooth=F, reg.line=F, boxplots="xy", xlim=c(-5,25), xlab="Percent Mivi w/ Sobi",legend.coords="bottomright")
abline(lm(ammonifd~percmivi, data=e4.sobi2), lty=2)
summary(lm(ammonifd~percmivi, data=e4.sobi2))

scatterplot(minzd ~ percmivi|bk, data=e4.sobi2, smooth=F, reg.line=F,boxplots="xy", xlim=c(-5,25), xlab="Percent Mivi w/ Sobi",legend.coords="bottomright")
abline(lm(minzd~percmivi, data=e4.sobi2), lty=2)
summary(lm(minzd~percmivi, data=e4.sobi2))
