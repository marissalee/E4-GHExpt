#e4_Fig4stats.R
#Stats for Figure 4: Mivi biomass vs soil measures without neighbors

##################################
### Prep dataframe ###
source('e4Code/e4_prepdfFig3n4.R') #use the same starting data as fig3
#str(data3)

##################################
### Load Libraries ###
library(lme4)
library(lmerTest)

##################################
### Functions ###
#FXN: FIT LME: soilmeas ~ mivi + I(mivi^2) + (1|bk)
FitLME.Fig4a <- function(sub){
  
  #loop through each soil measure
  SOILMEAS<-unique(sub$soilmeas)
  s <- 0
  
  #storage containers
  models <- list()
  store<-numeric(0)
  # anova.tabs <-list()
  
  for (s in 1:length(SOILMEAS)){ 
    #subset data by soil measure
    df <- subset(sub, soilmeas == SOILMEAS[s])
    
    #run the full model with lmerTest active
    model <- lmer(soilval ~ mivi + I(mivi^2) + (1|bk), data = df) 
    
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
    
    #save the model output
    models[[as.character(SOILMEAS[s])]] <- list(fe=fe, re=re, anova=anova, r2=r2, preddf=preddf)
    #   #run the reduced models
    #   modela<- lmer(soilval ~ mivi + (1|bk), data = df)
    #   modelb<- lmer(soilval ~ 1 + (1|bk), data = df)
    #   
    #   #evaluate change in AIC by reducing models
    #   termeff.a<-anova(modela,model) 
    #   termeff.b<-anova(modelb,modela) 
    #   
    #   #extract lmer anova parameters from termeffs
    #   row.a<- ExtractLMER.anova(termeff.a)
    #   row.b<- ExtractLMER.anova(termeff.b)
    #   tab <- rbind(row.b, row.a)
    #   wo<-c('wo.m','wo.m2')
    #   anova.tab<-data.frame(wo,tab)
    #   anova.tabs[[as.character(SOILMEAS[s])]] <- anova.tab
  }
  results<-models
  return(results)
}

#FXN: PULL OUT SUMMARIES, resp.var = soilval and group.var = 'mvtrt'
PullSum.Fig4 <- function(sub){
  SOILMEAS<-unique(sub$soilmeas)
  s <- 0
  summaries <- list()
  for (s in 1:length(SOILMEAS)){
    df <- subset(sub, soilmeas == SOILMEAS[s])
    summaries[[as.character(SOILMEAS[s])]] <- SummarizeThis(data=df, resp.var='soilval', group.var='mvtrt')
  }
  return(summaries)
}




########################################
#INCLUDES EMPTY POTS
sub1<-subset(data3, comptrt == 'N') #subset data

### Fit LME model ###
# resp <- mivi + I(mivi^2) + (1|bk)
lme.fig4<-FitLME.Fig4a(sub1)




########################################
#EXCLUDES EMPTY POTS
sub2<-subset(data3, comptrt == 'N' & type != 'Empty') #subset data

### Fit LME model ###
# resp <- mivi + I(mivi^2) + (1|bk)
lme.fig4.noempty<-FitLME.Fig4a(sub2)




########################################
### Summarize soil meas vals by mvtrt ###
sum.fig4 <- PullSum.Fig4(sub1)





