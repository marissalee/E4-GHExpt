#e4_Fig3stats.R
#Stats for Figure 3: Variation in soil measures among monocultures


### Read-in all the custom functions for doing stats ###
source('e4CodePackage_100614/statFxns.R')


### Prep dataframe ###
source('e4CodePackage_100614/e4_prepdfFig3n4.R')
#str(data3)


### Subset so that only have data from monocultures ###
sub1<-subset(data3, type == 'Mivi' | type == 'Empty' | type == 'Pavi' | type == 'Sobi')

### Panel A - total biomass as the response var
sum.fig3a <- PullSum.Fig3a(sub1)
lme.fig3a <- PullLME4.Fig3a(sub1)
lm.fig3a <- PullLM.Fig3a(sub1)

### Panel B - soil measures as the response var
sum.fig3b <- PullSum.Fig3b(sub1)
lme.fig3b <- PullLME4.Fig3b(sub1)
lm.fig3b <- PullLM.Fig3b(sub1)

