#e4_Fig5stats.R
#Stats for Figure 4: Variation in soil measures among mixed pots

##################################
### Prep dataframe ###
source('e4Code/e4_prepdfFig5.R')
#str(data4)

##################################
### Load Libraries ###
library(lme4)
library(lmerTest)

##################################
### Functions ###
#FXN: FIT LME: sF ~ biomval + I(biomval^2) + comptrt + biomval:comptrt + (1|bk)
FitLME.fig5 <- function(sub){
  
  #loop through each soil measure
  SOILMEAS<-unique(sub$soilmeas)
  s <- 0
  
  #storage containers
  models <- list()
  #   anova.tabs <-list()
  
  for (s in 1:length(SOILMEAS)){
    
    #subset data by soil measure
    df <- subset(sub, soilmeas == SOILMEAS[s])
    
    #run the full model with lmerTest active
    model <- lmer(sF ~ biomval + I(biomval^2) + comptrt + biomval:comptrt + (1|bk), data = df) 
    
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
    
    #calculate predicted vals using the fe only and attach it to the df
    pred<-predict(model, re.form=NA) #this sets the random effects to 0
    preddf<-data.frame(df, pred=pred)
    
    models[[as.character(SOILMEAS[s])]]<- list(fe=fe,re=re, anova=anova, r2=r2, preddf=preddf)
    
    #     #save the model output
    #     models[[as.character(SOILMEAS[s])]] <- model
    #     
    #     #run the reduced models
    #     modela<- lmer(sF ~ biomval + I(biomval^2) + comptrt  + (1|bk), data = df)
    #     modelb<- lmer(sF ~ biomval + I(biomval^2) + (1|bk), data = df)
    #     modelc<- lmer(sF ~ biomval + (1|bk), data = df)
    #     modeld<- lmer(sF ~ 1 + (1|bk), data = df)
    #     
    #     #evaluate change in AIC by reducing models
    #     termeff.a<-anova(modela,model) 
    #     termeff.b<-anova(modelb,modela) 
    #     termeff.c<-anova(modelc,modelb) 
    #     termeff.d<-anova(modeld,modelc) 
    #     
    #     #extract lmer anova parameters from termeffs
    #     row.a<- ExtractLMER.anova(termeff.a)
    #     row.b<- ExtractLMER.anova(termeff.b)
    #     row.c<- ExtractLMER.anova(termeff.c)
    #     row.d<- ExtractLMER.anova(termeff.d)
    #     tab <- rbind(row.d, row.c, row.b, row.a)
    #     wo<-c('wo.m','wo.m2','wo.comp','wo.int')
    #     anova.tab<-data.frame(wo,tab)
    #     anova.tabs[[as.character(SOILMEAS[s])]] <- anova.tab
  }
  #   result <- list(models,anova.tabs)
  result <- models
  return(result)
}

#FXN: FIT LME: sF ~ biomval + I(biomval^2) + (1|bk)
FitLME.fig6 <- function(sub){
  
  #loop through each soil measure
  SOILMEAS<-unique(sub$soilmeas)
  s <- 0
  
  #storage containers
  models <- list()
#   anova.tabs <-list()
  
  for (s in 1:length(SOILMEAS)){
    
    #subset data by soil measure
    df <- subset(sub, soilmeas == SOILMEAS[s])
    
    #run the full model with lmerTest active
    model <- lmer(sF ~ biomval + I(biomval^2) + (1|bk), data = df) 
    
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
    
    #calculate predicted vals using the fe only and attach it to the df
    pred<-predict(model, re.form=NA) #this sets the random effects to 0
    preddf<-data.frame(df, pred=pred)
    
    models[[as.character(SOILMEAS[s])]]<- list(fe=fe,re=re, anova=anova, r2=r2, preddf=preddf)
    
    #     #save the model output
    #     models[[as.character(SOILMEAS[s])]] <- model
    #     
    #     #run the reduced models
    #     modela<- lmer(sF ~ biomval +  (1|bk), data = df)
    #     modelb<- lmer(sF ~ 1 + (1|bk), data = df)
    #     
    #     #evaluate change in AIC by reducing models
    #     termeff.a<-anova(modela,model) 
    #     termeff.b<-anova(modelb,modela) 
    #     
    #     #extract lmer anova parameters from termeffs
    #     row.a<- ExtractLMER.anova(termeff.a)
    #     row.b<- ExtractLMER.anova(termeff.b)
    #     tab <- rbind(row.b, row.a)
    #     wo<-c('wo.t','wo.t2')
    #     anova.tab<-data.frame(wo,tab)
    #     anova.tabs[[as.character(SOILMEAS[s])]] <- anova.tab
    #     
  }
  #   result <- list(models, anova.tabs)
  result <- models
  return(result)
  
}

#FXN: PULL OUT SUMMARIES, resp.var = sF and group.var = 'comptrt'
PullSum.Fig5n6 <- function(sub){
  SOILMEAS<-unique(sub$soilmeas) #this is the same for sub.mivi and sub.total
  s <- 0
  summaries.mv <- list()
  summaries.comp <- list()
  summaries.trt <- list()
  for (s in 1:length(SOILMEAS)){
    df <- subset(sub, soilmeas == SOILMEAS[s])
    summaries.comp[[as.character(SOILMEAS[s])]] <- SummarizeThis(data=df, resp.var='sF', group.var='comptrt') #by comptrt
  }
  
  return(summaries.comp)
}


########################################
###for Mivi ###
# resp <- mivi + I(mivi^2) + comptrt + mivi:comptrt + (1|bk)
sub.mivi<-subset(data4, biommeas == "mivi") #subset so that only have data with biommeas == mivi
lme.fig5.mivi<-FitLME.fig5(sub.mivi) #fit lme

########################################
###for Fig6, Total ###
# resp <- total + I(total^2) + (1|bk)
sub.total<-subset(data4, biommeas == "total") #subset so that only have data with biommeas == total 
lme.fig6<-FitLME.fig6(sub.total) #fit lme
warnings()

########################################
### Summarize soil meas vals by mvtrt ###
#Make a column for trt (mvtrt x comptrt) ### -- this is important for looking at cross-category means
data4t <- data4
data4t$trt <- paste(data4[,'mvtrt'], data4[,'comptrt'], sep='')
#Summarize
sum.fig5.mivi <- PullSum.Fig5n6(sub.mivi) #for fig5
sum.fig5.total <- PullSum.Fig5n6(sub.total) #for fig6





### Extra ###################################################

###for Fig5b, relMivi ###
# # resp <- relmivi + I(relmivi^2) + comptrt + relmivi:comptrt + (1|bk)
# sub.relmivi<-subset(data4, biommeas == "relmivi" & comptrt != 'N') #subset so that only have data with biommeas == relmivi
# lme.fig5.relmivi<-FitLME.fig5(sub.relmivi) #fit lme
# warnings()
# 
# data4t <- data4
# data4t$trt <- paste(data4[,'mvtrt'], data4[,'comptrt'], sep='')
# #Summarize
# #sum.fig5.relmivi <- PullSum.Fig5n6(sub.mivi) #for fig5b





