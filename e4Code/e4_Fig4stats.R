#e4_Fig4stats.R
#Stats for Figure 4: Mivi biomass vs soil measures without neighbors


### Read-in all the custom functions for doing stats ###
source('e4Code/statFxns.R')


### Prep dataframe ###
source('e4Code/e4_prepdfFig3n4.R') #use the same starting data as fig3
#str(data3)



#subset so that only have data from comptrt == N pots ###
sub1<-subset(data3, comptrt == 'N')

#subset so that only have data from comptrt == N pots ###
sub2<-subset(data3, comptrt == 'N' & type != 'Empty')



### Fit LME model ###
# resp <- mivi + I(mivi^2) + (1|bk)
lme.fig4<-FitLME.Fig4a(sub1)
lme.fig4.noempty<-FitLME.Fig4a(sub2)

########################################

# ### Fit LM model ###
# # resp <- mivi + I(mivi^2) 
# lm.fig4 <- PullLM.Fig4(sub1)
# #lm.fig4

########################################

### Summarize soil meas vals by mvtrt ###
sum.fig4 <- PullSum.Fig4(sub1)
#sum.fig4

