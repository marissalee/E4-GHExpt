#E4 - SEM modeling
setwd("~/Desktop/E4_R")
#Question: What is a better predictor of net nitrification rate (nitrifd)?
#mivi
#total
#percmivibiom
#nhdi
#soilmoi

#open e4 dataset, only includes pots with two species (mivi and either P/S), no empty pots
dataP<-read.table('e4_semP_data2.txt',header=T)
dataS<-read.table('e4_semS_data2.txt',header=T)

########################
#first, check your data for multivariate normality
library(mvnormtest)
# Graphical Assessment of Multivariate Normality - S
xS <- as.matrix(dataS) # n x p numeric matrix
center <- colMeans(xS) # centroid
n <- nrow(xS); p <- ncol(xS); cov <- cov(xS);
d <- mahalanobis(xS,center,cov) # distances
qqplot(qchisq(ppoints(n),df=p),d,
       main="QQ Plot Assessing Multivariate Normality",
       ylab="Mahalanobis D2")
abline(a=0,b=1) 
mshapiro.test(t(xS))

hist(log(dataS$mivi))
shapiro.test(log(dataS$mivi)) #normal
hist(dataS$total)
shapiro.test(dataS$total) #normal
hist(dataS$percmivibiom)
hist(dataS$soilmoi)
shapiro.test((dataS$soilmoi)^3) #normal

# Graphical Assessment of Multivariate Normality - P
xP <- as.matrix(dataP) # n x p numeric matrix
center <- colMeans(xP) # centroid
n <- nrow(xP); p <- ncol(xP); cov <- cov(xP);
d <- mahalanobis(xP,center,cov) # distances
qqplot(qchisq(ppoints(n),df=p),d,
       main="QQ Plot Assessing Multivariate Normality",
       ylab="Mahalanobis D2")
abline(a=0,b=1) 
mshapiro.test(t(xP))

hist(dataP$mivi)
shapiro.test(sqrt(dataP$mivi)) #normal
hist(dataP$total)
shapiro.test(sqrt(dataP$total)) #normal
hist(dataP$percmivibiom)
shapiro.test(sqrt(dataP$percmivibiom))
hist(dataP$soilmoi)
shapiro.test((dataP$soilmoi)^5) #normal

########################
#assuming all is well, calculate the covariance matrix
cov_matrixS<-var(xS)
cov_matrixP<-var(xP)

cov_matrixS2<-var(xS)
cov_matrixP2<-var(xP)

########################
library(sem)
#specify the model - 1
model.path <- specifyModel()
mivi -> nitrifd, xy1, NA
mivi -> nhdi, x14, NA
total -> nitrifd, xy2, NA
total -> nhdi, x24, NA
percmivibiom -> nitrifd, xy3, NA
percmivibiom -> nhdi, x34, NA
nhdi -> nitrifd, xy4, NA
nhdi <-> nhdi, x4error, NA
nitrifd <-> nitrifd, yerror, NA

model.path

#specify the model - 2
model.path2 <- specifyModel()
mivi -> nitrifd, xy1, NA
mivi -> nhdi, x14, NA
percmivibiom -> nitrifd, xy3, NA
percmivibiom -> nhdi, x34, NA
nhdi -> nitrifd, xy4, NA
nhdi <-> nhdi, x4error, NA
nitrifd <-> nitrifd, yerror, NA

model.path2

########################
#fit the model -1
model.path.semS<-sem(model.path, cov_matrixS, N=40, fixed.x=c("mivi","total","percmivibiom"))
model.path.semP<-sem(model.path, cov_matrixP, N=40, fixed.x=c("mivi","total","percmivibiom"))

#fit the model -2
model.path.semS<-sem(model.path2, cov_matrixS, N=40, fixed.x=c("mivi","percmivibiom"))
model.path.semP<-sem(model.path2, cov_matrixP, N=40, fixed.x=c("mivi","percmivibiom"))

########################
#get the coefficient values, and see relevant statistics about various paths
#and the model as a whole
summary(model.path.semS)
summary(model.path.semP)

#check the standardized coefficients
stdCoef(model.path.semS)
stdCoef(model.path.semP)

#output the model to a dotfile from which you can generate an image
pathDiagram(model.path.semS, "model.path.semS2", edge.labels="values", digits=2, standardize=TRUE)
pathDiagram(model.path.semP, "model.path.semP2", edge.labels="values", digits=2, standardize=TRUE)

########################