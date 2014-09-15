#e4model.R
# 2/28/13
setwd("~/Desktop/E4_R")

library(MCMCpack) #for Bayesian glm analysis
library(nlme) #for mixed effect model, lme()
library(multcomp) #for setting contrasts

data<-read.table('e4_trimmedData.txt',header=T)
data$total<-data$Mvabund + data$pavi + data$sobi
data$Compabund<-data$pavi + data$sobi

##################################
#make datasets
Empty<-data[data$type=='Empty',]
Mivi<-data[data$type=='Mivi',]
Pavi<-data[data$type=='Pavi',]
Sobi<-data[data$type=='Sobi',]
CompEmpty<-data[data$type=='CompEmpty',]
CompPavi<-data[data$type=='CompPavi',]
CompSobi<-data[data$type=='CompSobi',]

dataa<-rbind(CompEmpty, CompPavi, CompSobi)
dataa$bk<-as.factor(dataa$bk)
dataa$Mvtrt<-as.factor(dataa$Mvtrt)

#########
#further cleaning...

#########
##################################

##################################
#Model template
# response ~ Mvtrt * comptrt * Mvtrtxcomptrt
#where...
# Mvtrt is a factor with 4 levels: 1/6 Mv, 2/6 Mv, 4/6 Mv, 5/6 Mv
# comptrt is a factor with 3 levels: N, P, S
# responses include: 1) Mvabund (PS), 2) Compabund (PS), 3) soilmoi (PS), 4) nodi (PS), 5) no.nh (PS), 6) nitrifd (N), 7) minzd (N)
# PN=positive normal?, N=normal?
# bk is a random effect
##################################

##################################
#Determine normality for response vars
temp1<-which(dataa$bk=='2')
#temp<-dataa[temp1,]
temp<-dataa

# 1) Mvabund
hist(temp$Mvabund)
shapiro.test(temp$Mvabund) 
# normal: bk1,bk2,bk3,bk4,bk6,bk7,bk8,bk9,bk10
# not normal: bk5

# 2) Compabund
hist(temp$Compabund)
shapiro.test(temp$Compabund) 
# normal: bk4, bk5
# not normal: bk1, bk2, bk3, bk6, bk7, bk8, bk9, bk10

# 3) soilmoi
hist(dataa$soilmoi)
shapiro.test(dataa$soilmoi) # not normal
# normal: ...
# not normal: bk1...

# 4) nodi
hist(dataa$nodi)
shapiro.test(dataa$nodi) # not normal

# 5) no.nh
hist(dataa$nh.no)
shapiro.test(dataa$nh.no) # not normal

# 6) nitrifd
hist(dataa$nitrifd)
shapiro.test(dataa$nitrifd) #not normal

# 7) minzd
hist(dataa$minzd)
shapiro.test(dataa$minzd) #not normal

#################################
# Set-up data
View(dataa)
mvtrt<-dataa$Mvtrt #x1
comptrt<-dataa$comptrt #x2
bk<-dataa$bk #block
#ys
response<-dataa$Mvabund 
#response<-dataa$MVrelabund 
#response<-dataa$soilmoi 
#response<-dataa$nodi
#response<-dataa$nh.no
#response<-dataa$nitrifd
#response<-dataa$minzd
data1 <- data.frame(response, mvtrt, comptrt,bk)

#################################
#Model type 1 - Bayesian
#in a Bayesian framework, I want to know this posterior distribution....
# p(b0,b1,b2,b3|Mvtrt,comptrt,response data) #posterior
# p(b0,b1,b2,b3) #prior
# p(response data|Mvtrt,comptrt,b0,b1,b2,b3) #likelihood
#Use an MCMC sampler to estimate the posterior distribution of betas based on the likelihood of the response data
#where response data = Mv biomass
model <- MCMCregress(response ~ mvtrt + comptrt + mvtrt*comptrt, data = data1, burnin = 3000, mcmc = 10000,
                       thin = 1, verbose = 0, seed = NA, beta.start = NA)
summary(model)
plot(model)

#posteriors
posterior.Mvabund<-model
#posterior.MVrelabund<-model
#posterior.soilmoi<-model
#posterior.nodi<-model
#posterior.nh.no<-model
#posterior.nitrifd<-model
#posterior.minzd<-model

##########################################
#Model type 2 - Frequentist version and incorporating block effects
model<-lme(fixed=response~mvtrt+comptrt+mvtrt*comptrt, random=~1|bk, data=data1, method="ML")

#fitted model results
lme.Mvabund<-model
anova(lme.Mvabund)
#lme.MVrelabund<-model
#lme.soilmoi<-model
#lme.nodi<-model
#lme.nh.no<-model
#lme.nitrifd<-model
#lme.minzd<-model

##########################################
#Model type 3 - Frequentist version and no block effects
model<-glm(response~mvtrt+comptrt+mvtrt*comptrt, data=data1, family=gaussian)

#fitted model results
lm.Mvabund<-model
#lm.MVrelabund<-model
#lm.soilmoi<-model
#lm.nodi<-model
#lm.nh.no<-model
#lm.nitrifd<-model
#lm.minzd<-model

##########################################
##########################################
#Model comparison
summary(posterior.Mvabund)
summary(lme.Mvabund)
summary(lm.Mvabund)

##########################################
#Post-hoc tests on the lm model
TukeyHSD(aov(lm.Mvabund))

##########################################
#Post-hoc tests on the lme model
intertrts<-interaction(mvtrt,comptrt)
length(intertrts)

data1$intertrts <- interaction(mvtrt, comptrt)
lme.post.model <- lme(response~ intertrts, random = ~1|bk, data=data1) 
lme.full.posthocs <- glht(lme.post.model, linfct=mcp(intertrts="Tukey")) #need to modify the degrees of freedom so that it fits the original model and not the lme.post.model

summary(lme.full.posthocs)
