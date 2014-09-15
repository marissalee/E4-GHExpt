##E4 Data Sets###########

setwd("~/Desktop/E4_R/E4_R_OldDatasets")
#read in data
e4 <- read.table("E4_all.txt",header=T)

########################

#make a comptrt=N dataset
e4.mivi <- e4[e4$comptrt=='N',]
e4.mivi1 <- e4.mivi[!is.na(e4.mivi[,6]),] #elim rows with NA in Mivi biom column
#e4.mivi1
query1=is.na(e4.mivi1[,15:21]) #elim rows with NA in any of the soil columns
query2=apply(query1,1,sum)
e4.mivi2 <- e4.mivi1[query2==0,]
#e4.mivi2

View(e4.mivi2)
hist(e4.mivi2$nitrifd)


########################

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

########################

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

#make a dataset with only densitytrt=L5 comptrt=P,S and densitytrt=L0 comptrt=N so that each species is in monoculture at highest density
e4.pavi4<-e4.pavi2[e4.pavi2$densitytrt=='L5',] #isolate monocultures L5
e4.sobi4<-e4.sobi2[e4.sobi2$densitytrt=='L5',] #isolate monocultures L5
e4.spp <- rbind(e4.miviNL0,e4.pavi4,e4.sobi4) #combine spp monocultures
View(e4.spp)