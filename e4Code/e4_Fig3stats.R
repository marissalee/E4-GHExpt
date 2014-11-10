#e4_Fig3stats.R
#Stats for Figure 3: Variation in soil measures among monocultures


### Read-in all the custom functions for doing stats ###
source('e4Code/statFxns.R')


### Prep dataframe ###
source('e4Code/e4_prepdfFig3n4.R')
#str(data3)


### Subset so that only have data from monocultures ###
sub1<-subset(data3, type == 'Mivi' | type == 'Empty' | type == 'Pavi' | type == 'Sobi')


### Panel A - soil measures as the response var

## MEANS
sum.fig3.soil <- PullSum.Fig3.soil(sub1) #summarize
#sum.fig3.soil

## LME
lme.fig3.soil <- FitLME.Fig3.soil(sub1) #model: soilresp ~ type + (1|bk)
lme.fig3.soil

## LM
lm.fig3.soil <- PullLM.Fig3.soil(sub1)
#lm.fig3.soil



### Panel B - total biomass as the response var

## MEANS
sum.fig3.total <- PullSum.Fig3.total(sub1)
#sum.fig3.total

## LME
lme.fig3.total <- FitLME.Fig3.total(sub1) #model: total ~ type + (1|bk)
#lme.fig3.total

## LM
lm.fig3.total <- PullLM.Fig3.total(sub1)
#lm.fig3.total


