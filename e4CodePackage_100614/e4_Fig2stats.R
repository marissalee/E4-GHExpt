#e4_Fig2stats.R
#Stats for Figure 2: 


### Read-in all the custom functions for doing stats ###
source('e4CodePackage_100614/statFxns.R')


### Prep dataframe ###
source('e4CodePackage_100614/e4_prepdfFig2.R')
#str(data2)




### PANEL A ###################################################

### Subset so that only have data from biommeas == relmivi ###
sub1<-subset(data2, biommeas == 'relmivi' & comptrt != 'N')
sum.fig2a <- PullSum.Fig2(sub1)
lme.fig2a <- PullLME4.Fig2(sub1)
lm.fig2a <- PullLM.Fig2(sub1)




### PANEL B ###################################################

### Subset so that only have data from biommeas == mivi ###
sub1<-subset(data2, biommeas == 'mivi')
sum.fig2b.mivi <- PullSum.Fig2(sub1)
lme.fig2b.mivi <- PullLME4.Fig2(sub1)
lm.fig2b.mivi <- PullLM.Fig2(sub1)

### Subset so that only have data from biommeas == compabund ###
sub1<-subset(data2, biommeas == 'compabund' & comptrt != 'N')
sum.fig2b.comp <- PullSum.Fig2(sub1)
lme.fig2b.comp <- PullLME4.Fig2(sub1)
lm.fig2b.comp <- PullLM.Fig2(sub1)




### PANEL C ###################################################

### Subset so that only have data from biommeas == mivi ###
sub1<-subset(data2, biommeas == 'total')
sum.fig2c <- PullSum.Fig2(sub1)
lme.fig2c <- PullLME4.Fig2(sub1)
lm.fig2c <- PullLM.Fig2(sub1)




