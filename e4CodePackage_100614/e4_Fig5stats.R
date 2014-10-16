#e4_Fig4stats.R
#Stats for Figure 4: Variation in soil measures among mixed pots


### Read-in all the custom functions for doing stats ###
source('e4CodePackage_100614/statFxns.R')


### Prep dataframe ###
source('e4CodePackage_100614/e4_prepdfFig5.R')
#str(data4)


### Make a column for trt (mvtrt x comptrt) ### -- this is important for looking at cross-category means
data4t <- data4
data4t$trt <- paste(data4[,'mvtrt'], data4[,'comptrt'], sep='')


### Subset so that only have data with biommeas == mivi OR biommeas == total ###
sub.mivi<-subset(data4t, biommeas == "mivi")
sub.total<-subset(data4t, biommeas == "total")


#for mivi
sum.fig5.mivi <- PullSum.Fig5(sub.mivi)
lme.fig5.mivi <- PullLME4.Fig5(sub.mivi)
lm.fig5.mivi <- PullLM.Fig5(sub.mivi)

#for total
sum.fig5.total <- PullSum.Fig5(sub.total)
lme.fig5.total <- PullLME4.Fig5(sub.total)
lm.fig5.total <- PullLM.Fig5(sub.total)





