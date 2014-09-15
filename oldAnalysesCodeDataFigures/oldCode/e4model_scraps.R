#e4model.R scraps

#Look for correlations between predictors
plot(logtotal~mvtrt)
lm1<-lm(logtotal~mvtrt); anova(lm1) #not correlated

plot(logtotal~comptrt)
lm2<-lm(logtotal~comptrt); anova(lm2) #correlated

plot(dataa$Mvabund~mvtrt)
lm3<-lm(dataa$Mvabund~mvtrt); anova(lm3) #correlated
summary(lm3) #linear

plot(dataa$MVrelabund~mvtrt)
lm4<-lm(dataa$MVrelabund~mvtrt); anova(lm4) #correlated
summary(lm4) #linear

#Models for monoculture pots
#totdi
mod0<-lme(fixed=transtotdi~type, random=~1|bk, data=Monos, method="ML")
summary(mod0) #update
#nitrate
mod0<-lme(fixed=transnodi~type, random=~1|bk, data=Monos, method="ML")
summary(mod0) #update
#minzd
mod0<-lme(fixed=minzd~type, random=~1|bk, data=Monos, method="ML")
summary(mod0)
#nitrifd
mod0<-lme(fixed=nitrifd~type, random=~1|bk, data=Monos, method="ML")
summary(mod0)
#soilmoi
mod0<-lme(fixed=transsoilmoi~type, random=~1|bk, data=Monos, method="ML")
summary(mod0) #update

###########################################################

#Determine normality
plot(density(CompEmpty$mivi, bw = "sj"))
shapiro.test(CompEmpty$mivi) #fits normal distb

#Model for treatment effectiveness
# percmiviind vs mivi
mod0<-lme(fixed=mivi~percmiviind, random=~1|bk, data=CompEmpty, method="ML")
summary(mod0)

###########################################################
#Determine normality
plot(density(log(CompE$nodi)^2, bw = "sj"))
shapiro.test(log(CompE$nodi)^2) #fits normal distb
CompE$transnodi <- log(CompE$nodi)^2 # use CompE$transnodi

#Models for mixed pots
#nitrate
#percmiviind+comptrt+interactions
mod0<-lme(fixed=transnodi~percmiviind+comptrt+percmiviind*comptrt, random=~1|bk, data=CompE, method="ML")
mod1<-lme(fixed=transnodi~percmiviind+comptrt, random=~1|bk, data=CompE, method="ML")
mod2<-lme(fixed=transnodi~percmiviind, random=~1|bk, data=CompE, method="ML")
mod3<-lme(fixed=transnodi~comptrt, random=~1|bk, data=CompE, method="ML")
AIC(mod0, mod1, mod2, mod3)# mod1 is the best, followed by mod3
summary(mod1)
#nitrate
#mivi+comptrt+interactions
mod0<-lme(fixed=transnodi~mivi+comptrt+mivi*comptrt, random=~1|bk, data=CompE, method="ML")
mod1<-lme(fixed=transnodi~mivi+comptrt, random=~1|bk, data=CompE, method="ML")
mod2<-lme(fixed=transnodi~mivi, random=~1|bk, data=CompE, method="ML")
mod3<-lme(fixed=transnodi~comptrt, random=~1|bk, data=CompE, method="ML")
AIC(mod0, mod1, mod2, mod3) # mod3 is the best
summary(mod3)

#total+comptrt+interactions
mod0<-lme(fixed=transnodi~total+comptrt+total*comptrt, random=~1|bk, data=CompE, method="ML")
mod1<-lme(fixed=transnodi~total+comptrt, random=~1|bk, data=CompE, method="ML")
mod2<-lme(fixed=transnodi~total, random=~1|bk, data=CompE, method="ML")
mod3<-lme(fixed=transnodi~comptrt, random=~1|bk, data=CompE, method="ML")
AIC(mod0, mod1, mod2, mod3) # mod2 is the best
summary(mod2) #

#########################################
#Determine normality
plot(density(CompE$totdi, bw = "sj"))
shapiro.test(CompE$totdi) #fits normal distb

#totdi
#percmiviind+comptrt+interactions
mod0<-lme(fixed=totdi~percmiviind+comptrt+percmiviind*comptrt, random=~1|bk, data=CompE, method="ML")
mod1<-lme(fixed=totdi~percmiviind+comptrt, random=~1|bk, data=CompE, method="ML")
mod2<-lme(fixed=totdi~percmiviind, random=~1|bk, data=CompE, method="ML")
mod3<-lme(fixed=totdi~comptrt, random=~1|bk, data=CompE, method="ML")
AIC(mod0, mod1, mod2, mod3)# mod1 is the best, followed by mod3
summary(mod1)
#totdi
#mivi+comptrt+interactions
mod0<-lme(fixed=totdi~mivi+comptrt+mivi*comptrt, random=~1|bk, data=CompE, method="ML")
mod1<-lme(fixed=totdi~mivi+comptrt, random=~1|bk, data=CompE, method="ML")
mod2<-lme(fixed=totdi~mivi, random=~1|bk, data=CompE, method="ML")
mod3<-lme(fixed=totdi~comptrt, random=~1|bk, data=CompE, method="ML")
AIC(mod0, mod1, mod2, mod3) # mod3 is the best, followed by mod1
summary(mod3)

#mivi+comptrt+interactions
mod0<-lme(fixed=totdi~total+comptrt+total*comptrt, random=~1|bk, data=CompE, method="ML")
mod1<-lme(fixed=totdi~total+comptrt, random=~1|bk, data=CompE, method="ML")
mod2<-lme(fixed=totdi~total, random=~1|bk, data=CompE, method="ML")
mod3<-lme(fixed=totdi~comptrt, random=~1|bk, data=CompE, method="ML")
AIC(mod0, mod1, mod2, mod3) # mod3 is the best, followed by mod1
summary(mod2)

#########################################
#Determine normality
plot(density(log(CompE$nitrifd+10), bw = "sj"))
shapiro.test(log(CompE$nitrifd+10)) #fits normal distb
CompE$transnitrifd <- log(CompE$nitrifd+10) # use CompE$transnitrifd

#nitrifd
#percmiviind+comptrt+interactions
mod0<-lme(fixed=transnitrifd~percmiviind+comptrt+percmiviind*comptrt, random=~1|bk, data=CompE, method="ML")
mod1<-lme(fixed=transnitrifd~percmiviind+comptrt, random=~1|bk, data=CompE, method="ML")
mod2<-lme(fixed=transnitrifd~percmiviind, random=~1|bk, data=CompE, method="ML")
mod3<-lme(fixed=transnitrifd~comptrt, random=~1|bk, data=CompE, method="ML")
AIC(mod0, mod1, mod2, mod3)# mod3 is the best, followed by mod1
summary(mod3)
#nitrifd
#mivi+comptrt+interactions
mod0<-lme(fixed=transnitrifd~mivi+comptrt+mivi*comptrt, random=~1|bk, data=CompE, method="ML")
mod1<-lme(fixed=transnitrifd~mivi+comptrt, random=~1|bk, data=CompE, method="ML")
mod2<-lme(fixed=transnitrifd~mivi, random=~1|bk, data=CompE, method="ML")
mod3<-lme(fixed=transnitrifd~comptrt, random=~1|bk, data=CompE, method="ML")
AIC(mod0, mod1, mod2, mod3) # mod3 is the best, followed by mod1
summary(mod3)

#total+comptrt+interactions
mod0<-lme(fixed=transnitrifd~total+comptrt+total*comptrt, random=~1|bk, data=CompE, method="ML")
mod1<-lme(fixed=transnitrifd~total+comptrt, random=~1|bk, data=CompE, method="ML")
mod2<-lme(fixed=transnitrifd~total, random=~1|bk, data=CompE, method="ML")
mod3<-lme(fixed=transnitrifd~comptrt, random=~1|bk, data=CompE, method="ML")
AIC(mod0, mod1, mod2, mod3) # mod3 is the best, followed by mod1
summary(mod1)


#########################################
#Determine normality
plot(density(log(CompE$minzd+10), bw = "sj"))
shapiro.test(log(CompE$minzd+10)) #fits normal distb
CompE$transminzd <- log(CompE$minzd+10) # use CompE$transnitrifd

#minzd
#percmiviind+comptrt+interactions
mod0<-lme(fixed=transminzd~percmiviind+comptrt+percmiviind*comptrt, random=~1|bk, data=CompE, method="ML")
mod1<-lme(fixed=transminzd~percmiviind+comptrt, random=~1|bk, data=CompE, method="ML")
mod2<-lme(fixed=transminzd~percmiviind, random=~1|bk, data=CompE, method="ML")
mod3<-lme(fixed=transminzd~comptrt, random=~1|bk, data=CompE, method="ML")
AIC(mod0, mod1, mod2, mod3)# mod3 is the best, followed by mod1
summary(mod3)
#minzd
#mivi+comptrt+interactions
mod0<-lme(fixed=transminzd~mivi+comptrt+mivi*comptrt, random=~1|bk, data=CompE, method="ML")
mod1<-lme(fixed=transminzd~mivi+comptrt, random=~1|bk, data=CompE, method="ML")
mod2<-lme(fixed=transminzd~mivi, random=~1|bk, data=CompE, method="ML")
mod3<-lme(fixed=transminzd~comptrt, random=~1|bk, data=CompE, method="ML")
AIC(mod0, mod1, mod2, mod3) # mod3 is the best, followed by mod1
summary(mod3)

#########################################
#Determine normality
plot(density(CompE$soilmoi, bw = "sj"))
shapiro.test((CompE$soilmoi)^12) #this is as close as it will get (p=0.03)
CompE$transsoilmoi <- (CompE$soilmoi)^12 # use CompE$transsoilmoi

#soilmoi
#percmiviind+comptrt+interactions
mod0<-lme(fixed=transsoilmoi~percmiviind+comptrt+percmiviind*comptrt, random=~1|bk, data=CompE, method="ML")
mod1<-lme(fixed=transsoilmoi~percmiviind+comptrt, random=~1|bk, data=CompE, method="ML")
mod2<-lme(fixed=transsoilmoi~percmiviind, random=~1|bk, data=CompE, method="ML")
mod3<-lme(fixed=transsoilmoi~comptrt, random=~1|bk, data=CompE, method="ML")
AIC(mod0, mod1, mod2, mod3)# mod0 is the best, followed by mod3
summary(mod0)
#soilmoi
#mivi+comptrt+interactions
mod0<-lme(fixed=transsoilmoi~mivi+comptrt+mivi*comptrt, random=~1|bk, data=CompE, method="ML")
mod1<-lme(fixed=transsoilmoi~mivi+comptrt, random=~1|bk, data=CompE, method="ML")
mod2<-lme(fixed=transsoilmoi~mivi, random=~1|bk, data=CompE, method="ML")
mod3<-lme(fixed=transsoilmoi~comptrt, random=~1|bk, data=CompE, method="ML")
AIC(mod0, mod1, mod2, mod3) # mod0 is the best, followed by mod1
summary(mod0)


###########################################
###########################################
###########################################
## Summarizes data.
## Gives count, mean, standarddeviation, standard error of the mean, and confidence interval (default 95%).
## If there are within-subjectvariables, calculate adjusted values using method from Morey (2008).
##   measurevar: the name of a column that contains thevariable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidenceinterval (default is 95%)

summarySE <- function(data=NULL,measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) 
{
  require(doBy)
  length2 <- function (x, na.rm=FALSE) {if (na.rm)sum(!is.na(x)) else length(x)}
  formula <- as.formula(paste(measurevar,paste(groupvars, collapse=" + "), sep=" ~ ")) # Collapse the data
  datac <- summaryBy(formula, data=data,FUN=c(length2,mean,sd), na.rm=na.rm)
  names(datac)[ names(datac) ==paste(measurevar, ".mean",    sep="") ] <- measurevar # Rename columns
  names(datac)[ names(datac) ==paste(measurevar, ".sd",      sep="") ] <-"sd"
  names(datac)[ names(datac) ==paste(measurevar, ".length2", sep="") ] <- "N"
  datac$se <- datac$sd /sqrt(datac$N)  # Calculate standarderror of the mean
  ciMult <- qt(conf.interval/2 + .5,datac$N-1) # Confidence interval multiplier forstandard error # Calculate t-statistic for confidenceinterval: # e.g., if conf.interval is .95, use .975(above/below), and use df=N-1
  datac$ci <- datac$se * ciMult
  return(datac)
}
