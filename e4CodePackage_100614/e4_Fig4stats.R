#e4_Fig4stats.R
#Stats for Figure 4: Mivi biomass vs soil measures without neighbors


### Read-in all the custom functions for doing stats ###
source('e4CodePackage_100614/statFxns.R')


### Prep dataframe ###
source('e4CodePackage_100614/e4_prepdfFig3n4.R') #use the same starting data as fig3
#str(data3)


### Subset so that only have data from comptrt == N pots ###
sub1<-subset(data3, comptrt == 'N')

sum.fig4 <- PullSum.Fig4(sub1)
lme.fig4 <- PullLME4.Fig4(sub1)
lm.fig4 <- PullLM.Fig4(sub1)

