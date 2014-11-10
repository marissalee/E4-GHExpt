#e4_Fig4stats.R
#Stats for Figure 4: Variation in soil measures among mixed pots


### Read-in all the custom functions for doing stats ###
source('e4Code/statFxns.R')


### Prep dataframe ###
source('e4Code/e4_prepdfFig5.R')
#str(data4)


##################################
### Fit LME models
library(lme4)
library(lmerTest)

########################################
###for Mivi ###
# resp <- mivi + I(mivi^2) + comptrt + mivi:comptrt + (1|bk)
sub.mivi<-subset(data4, biommeas == "mivi") #subset so that only have data with biommeas == mivi
lme.fig5.mivi<-FitLME.fig5.mivi(sub.mivi) #fit lme
#lme.fig5.mivi

########################################
###for relMivi ###
# resp <- relmivi + I(relmivi^2) + comptrt + relmivi:comptrt + (1|bk)
sub.relmivi<-subset(data4, biommeas == "relmivi" & comptrt != 'N') #subset so that only have data with biommeas == relmivi
lme.fig5.relmivi<-FitLME.fig5.mivi(sub.relmivi) #fit lme
#lme.fig5.relmivi

########################################
###for Total ###
# resp <- total + I(total^2) + (1|bk)
sub.total<-subset(data4, biommeas == "total") #subset so that only have data with biommeas == total 
lme.fig5.total<-FitLME.fig5.total(sub.total) #fit lme
#lme.fig5.total

########################################
### Fit LM model ###
# resp <- mivi + comptrt + mivi:comptrt
lm.fig5.mivi <- PullLM.Fig5(sub.mivi)
lm.fig5.total <- PullLM.Fig5(sub.total)



########################################
### Summarize soil meas vals by mvtrt ###
#Make a column for trt (mvtrt x comptrt) ### -- this is important for looking at cross-category means
data4t <- data4
data4t$trt <- paste(data4[,'mvtrt'], data4[,'comptrt'], sep='')
#Summarize
sum.fig5.mivi <- PullSum.Fig5(sub.mivi)
sum.fig5.relmivi <- PullSum.Fig5(sub.mivi)
sum.fig5.total <- PullSum.Fig5(sub.total)






