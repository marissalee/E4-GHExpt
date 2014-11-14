#e4_Fig2stats.R
#Stats for Figure 2: 


##################################
### Prep dataframe ###
source('e4Code/e4_prepdfFig2.R')
#str(data2)

##################################
### Load Libraries ###
library(lme4)
library(lmerTest)

##################################
### Functions ###
source('e4Code/statFxns.R') 

#FXN: PULL OUT SUMMARIES, FIG 2 to summarize biomval by mvtrt and comptrt
PullSum.Fig2 <- function(sub){
  COMPTRT <- unique(sub$comptrt)
  i<-0
  summaries <- list()
  for (i in 1:length(COMPTRT)){
    df <- subset(sub, comptrt == COMPTRT[i])
    summaries[[as.character(COMPTRT[i])]] <- SummarizeThis(data=df, resp.var='biomval', group.var='mvtrt')
  }
  return(summaries)
}

#FXN: FIT LME: biomval ~ mvtrt + comptrt + mvtrt:comptrt + (1|bk), FIG 2
FitLME.fig2 <- function(sub){
  #run the full model
  model <- lmer(biomval ~ mvtrt + comptrt + mvtrt:comptrt + (1|bk), data = sub) #run the full model
  
  #r2 est
  r2<-r2.corr.mer(model)
  
  #save the anova table
  anova<-round(data.frame(anova(model)), digits=2)
  
  #fixed effects
  fe<- round(summary(model)$coefficients, digits=2)
  
  #random effects
  re1<-as.data.frame(VarCorr(model))
  vals<-round(re1[,c('vcov','sdcor')], digits=2)
  re<-data.frame(grp=re1[,'grp'],vals)
  
  #   #run the reduced models
  #   modela <- lmer(biomval ~ mvtrt + comptrt + (1|bk), data = sub) #run the reduced model
  #   modelb <- lmer(biomval ~ mvtrt + (1|bk), data = sub)
  #   modelc <- lmer(biomval ~ 1 + (1|bk), data = sub)
  #   
  #   #evaluate change in AIC by reducing models
  #   termeff.a<-anova(modela,model) 
  #   termeff.b<-anova(modelb,modela) 
  #   termeff.c<-anova(modelc,modelb) 
  #   
  #   #extract lmer anova parameters from termeffs
  #   row.a<- ExtractLMER.anova(termeff.a)
  #   row.b<- ExtractLMER.anova(termeff.b)
  #   row.c<- ExtractLMER.anova(termeff.c)
  #   tab <- rbind(row.c, row.b, row.a)
  #   wo<-c('wo.mtrt','wo.comp','wo.int')
  #   anova.tab<-data.frame(wo,tab)
  
  #  results<-list(model, anova.tab)
  results<-list(fe=fe, re=re, anova=anova, r2=r2)
  
  return(results)
}



### FIG 2A ###################################################

#1. Biommeas == mivi
## MEANS
sub1<-subset(data2, biommeas == 'mivi') #subset
sum.fig2a.mivi <- PullSum.Fig2(sub1) # summarize
## LME
sub1<-subset(data2, biommeas == 'mivi' & mvtrt != 0 & mvtrt !=6) #subset
lme.fig2a.mivi <- FitLME.fig2(sub1) #model <- lmer(biomval ~ mvtrt + comptrt + mvtrt:comptrt + (1|bk), data = sub) 


#2. Biommeas == compabund
## MEANS
sub1<-subset(data2, biommeas == 'compabund' & comptrt != 'N' & mvtrt !=6) #subset
sum.fig2a.comp <- PullSum.Fig2(sub1) #summarize
## LME
lme.fig2a.comp <- FitLME.fig2(sub1) #model <- lmer(biomval ~ mvtrt + comptrt + mvtrt:comptrt + (1|bk), data = sub) 




### FIG 2B ###################################################

#1. Biommeas == total
## MEANS
sub1<-subset(data2, biommeas == 'total' & mvtrt !=6) #subset
sum.fig2b <- PullSum.Fig2(sub1) #summarize
# LME
lme.fig2b <- FitLME.fig2(sub1) #model <- lmer(biomval ~ mvtrt + comptrt + mvtrt:comptrt + (1|bk), data = sub) 




### Extra ###################################################

#1Biommeas == relmivi
## MEANS
# sub1<-subset(data2, biommeas == 'relmivi' & comptrt != 'N' & mvtrt !=6) #subset
# sum.fig2b <- PullSum.Fig2(sub1) #summarize
# ## LME
# lme.fig2b <- FitLME.fig2(sub1) #model <- lmer(biomval ~ mvtrt + comptrt + mvtrt:comptrt + (1|bk), data = sub) 
# 
# 


