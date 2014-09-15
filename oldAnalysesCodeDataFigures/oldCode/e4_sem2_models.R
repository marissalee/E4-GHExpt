#E4 - SEM modeling
setwd("~/Desktop/E4_R")
#Q: How do mivi abundance, relative abundance, and substrate availability affect nitrification rates?

########################
#open e4 dataset, only includes pots with mivi and sobi, no empty pots
data<-read.table('E4_s.txt',header=T)
#variables of interest:
#sobi
#mivi
#total
#percmivibiom
#nhdi
#minzd
dataS<-data[,c(1:5,9)]

########################
#assuming all is well, calculate the covariance matrix
xS <- as.matrix(dataS) # n x p numeric matrix
cov_matrixS<-var(xS)

########################
#specify the model
library(sem)
model.pathS <- specifyModel()
sobi -> nhdi, sh, NA
sobi -> nitrifd, sn, NA
mivi-> nhdi, mh, NA
mivi -> nitrifd, mn, NA
total -> nhdi, th, NA
total -> nitrifd, tn, NA
percmivibiom -> nhdi, ph, NA
percmivibiom -> nitrifd, pn, NA
nhdi -> nitrifd, hn, NA
nitrifd <-> nitrifd, ne, NA

model.pathS

########################
#fit the model
model.path.semS<-sem(model.pathS, cov_matrixS, N=length(dataS), fixed.x=c("sobi","mivi","total","percmivibiom"))

########################
#look at summary output
summary(model.path.semS) #get the coefficient values, and see relevant statistics about various paths
stdCoef(model.path.semS) #check the standardized coefficients

########################
#make a dotfile map
pathDiagram(model.path.semS, "model.path.semS", edge.labels="values", digits=2, standardize=TRUE) #standardize prints the standardized effect coefs

########################
#bootstrap to get effect coef CIs
system.time(boot <- bootSem(model.path.semS, R=500, cov=cov_matrixS, data=xS))
summary(boot, type="norm")  # cf., standard errors to those computed by summary()

###################################################
# for pavi
data<-read.table('E4_p.txt',header=T)
dataP<-data[,c(1:5,9)]
xP <- as.matrix(dataP)
cov_matrixP<-var(xP)

model.pathP <- specifyModel()
pavi -> nhdi, sh, NA
pavi -> nitrifd, sn, NA
mivi-> nhdi, mh, NA
mivi -> nitrifd, mn, NA
total -> nhdi, th, NA
total -> nitrifd, tn, NA
percmivibiom -> nhdi, ph, NA
percmivibiom -> nitrifd, pn, NA
nhdi -> nitrifd, hn, NA
nhdi <-> nhdi, he, NA
nitrifd <-> nitrifd, ne, NA

model.path.semP<-sem(model.pathP, cov_matrixP, N=length(dataP), fixed.x=c("pavi","mivi","total","percmivibiom"))
summary(model.path.semP) 
pathDiagram(model.path.semS, "model.path.semS", edge.labels="values", digits=2, standardize=TRUE) #standardize prints the standardized effect coefs
system.time(boot <- bootSem(model.path.semP, R=1000, cov=cov_matrixP, data=xP))
summary(boot, type="norm")  # cf., standard errors to those computed by summary()

