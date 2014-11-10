#e4_Fig2stats.R
#Stats for Figure 2: 


### Read-in all the custom functions for doing stats ###
source('e4Code/statFxns.R') 


### Prep dataframe ###
source('e4Code/e4_prepdfFig2.R')
#str(data2)


### PANEL A ###################################################

### Subset so that only have data from biommeas == mivi ###

## MEANS
sub1<-subset(data2, biommeas == 'mivi') #subset
sum.fig2a.mivi <- PullSum.Fig2(sub1) # summarize
#sum.fig2a.mivi

## LME
sub1<-subset(data2, biommeas == 'mivi' & mvtrt != 0 & mvtrt !=6) #subset
lme.fig2a.mivi <- FitLME.fig2(sub1) #model <- lmer(biomval ~ mvtrt + comptrt + mvtrt:comptrt + (1|bk), data = sub) 
#lme.fig2a.mivi

## LM
lm.fig2a.mivi <- PullLM.Fig2(sub1) # lm
#lm.fig2a.mivi



### Subset so that only have data from biommeas == compabund ###

## MEANS
sub1<-subset(data2, biommeas == 'compabund' & comptrt != 'N' & mvtrt !=6) #subset
sum.fig2a.comp <- PullSum.Fig2(sub1) #summarize
#sum.fig2a.comp

## LME
lme.fig2a.comp <- FitLME.fig2(sub1) #model <- lmer(biomval ~ mvtrt + comptrt + mvtrt:comptrt + (1|bk), data = sub) 
#lme.fig2a.comp

## LM
lm.fig2a.comp <- PullLM.Fig2(sub1) # lm
#lm.fig2a.comp




### PANEL B ###################################################

### Subset so that only have data from biommeas == relmivi ###

## MEANS
sub1<-subset(data2, biommeas == 'relmivi' & comptrt != 'N' & mvtrt !=6) #subset
sum.fig2b <- PullSum.Fig2(sub1) #summarize
#sum.fig2b

## LME
lme.fig2b <- FitLME.fig2(sub1) #model <- lmer(biomval ~ mvtrt + comptrt + mvtrt:comptrt + (1|bk), data = sub) 
#lme.fig2b

##LM
lm.fig2b <- PullLM.Fig2(sub1) #lm
#lm.fig2b


### PANEL C ###################################################

### Subset so that only have data from biommeas == mivi ###

## MEANS
sub1<-subset(data2, biommeas == 'total' & mvtrt !=6) #subset
sum.fig2c <- PullSum.Fig2(sub1) #summarize
#sum.fig2c

# LME
lme.fig2c <- FitLME.fig2(sub1) #model <- lmer(biomval ~ mvtrt + comptrt + mvtrt:comptrt + (1|bk), data = sub) 
#lme.fig2c

# LM
lm.fig2c <- PullLM.Fig2(sub1) #lm
#lm.fig2c



