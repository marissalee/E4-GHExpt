#e4_Fig3stats.R
#Stats for Figure 3: Variation in soil measures among monocultures


### Read-in all the custom functions for doing stats ###
source('e4CodePackage_100614/statFxns.R')


### Prep dataframe ###
source('e4CodePackage_100614/e4_prepdfFig3n4.R')
str(data3)


### Subset so that only have data from monocultures ###
sub1<-subset(data3, type == 'Mivi' | type == 'Empty' | type == 'Pavi' | type == 'Sobi')

sum.fig3 <- PullSum.Fig3(sub1)
lme.fig3 <- PullLME4.Fig3(sub1)
lm.fig3 <- PullLM.Fig3(sub1)

