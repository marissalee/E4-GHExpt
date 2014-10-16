#statFxns.R


#FUNCTION TO SUMMARIZE A DATASET (MEAN, SE, N)
#needs these packages:
library(plyr)
SummarizeThis <- function(data, resp.var, group.var){
  respcol <- data[,resp.var]
  groupcol <- data[,group.var]
  dt <- data.frame(respcol,groupcol)
  summ <- ddply(dt,~groupcol,summarise,
           mean=mean(respcol),
           se=sd(respcol)/sqrt(length(respcol)),
           n=length(respcol))
  return(summ)
}




#FUNCTION TO PULL OUT LMER MODEL PARAMETERS
ExtractLMER <- function(model){
  #random effects
  #Var
  REvar<-as.data.frame(VarCorr(model))[,'vcov']
  #SD
  REsd<-as.data.frame(VarCorr(model))[,'sdcor']
  #names of the random effect levels
  RElevels<-as.data.frame(VarCorr(model))[,1]
  
  #fixed effects
  #Est
  FEest<-fixef(model)
  #SE
  FEsderr <- sqrt(diag(vcov(model))) #std err
  #t value
  t <- FEest/FEsderr 
  #names of the fixed effect levels
  FElevels<-names(FEest)
  
  #pull everything together
  REdf <- data.frame(RElevels, REvar, REsd)
  FEdf <- data.frame(FElevels, FEest, FEsderr, t)
  result <- list(REdf, FEdf)
  names(result) <- c('RE','FE')
  
  return(result)
}




#FUNCTION TO PULL OUT LMER ANOVA PARAMETERS... for e4 fig3 monocultures
ExtractLMER.anova <- function(termeff){
  #delAIC
  aic.model <- termeff[2,2] 
  aic.modela <- termeff[1,2]
  delAIC <- aic.model - aic.modela
  #chisq
  Chisq <- termeff[2,6]
  #pval
  anova.pval <- termeff[2,8]
  #chisq df
  ChisqDF <- termeff[2,7]
  #model dfs
  df.model <- termeff[1,1]
  df.modelRed <- termeff[2,1]
  
  #pull everything together
  result <- data.frame(delAIC, Chisq, anova.pval, ChisqDF, df.model, df.modelRed)
  
  return(result)
}




#FUNCTION TO PULL OUT SUMMARIES
#ONLY USE THIS FOR FIG 2, where model = mvtrt * comptrt
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




#FUNCTION TO PULL OUT LME4 PARAMS AND ANOVA,
#ONLY USE THIS FOR FIG 2, where model = mvtrt * comptrt
library(lme4)
PullLME4.Fig2 <- function(sub){
  
  #run the full model
  model <- lmer(biomval ~ mvtrt + comptrt + mvtrt:comptrt + (1|bk), data = sub) #run the full model
  model.param <- ExtractLMER(model) #extract lmer model parameters
  
  #run the reduced models
  modela <- lmer(biomval ~ mvtrt + comptrt + (1|bk), data = sub) #run the reduced model
  modelb <- lmer(biomval ~ mvtrt + (1|bk), data = sub)
  modelc <- lmer(biomval ~ 1 + (1|bk), data = sub)
  
  #extract lmer anova parameters from termeffs
  MODELS<-list(model, modela, modelb, modelc)
  DIFFS<-c('wo.int','wo.comptrt','wo.mvtrt')
  d<-0
  anova.params<-list()
  for (m in 1:length(DIFFS)){
    cur.model <- MODELS[[m]]
    red.model <- MODELS[[m+1]]
    termeff <- anova(red.model, cur.model) #compare the full and reduced model with ANOVA
    anova.params[[as.character(DIFFS[m])]] <- ExtractLMER.anova(termeff)
  }
  
  #store important stuff in a list
  results <- list(model.param, anova.params)
  names(results) <- c('model.param','anova.params')
  
  return(results)
}




#FUNCTION TO PULL OUT LM PARAMS AND TUKEYS
#ONLY USE THIS FOR FIG 2, where model = mvtrt * comptrt
PullLM.Fig2 <- function(sub){
  #run the full model
  aov.fit <- aov(biomval ~ mvtrt + comptrt + mvtrt:comptrt, data = sub)
  betas <- c('mvtrt','comptrt','mvtrt:comptrt','Residuals')
  
  #save the anova pval 
  aov.fit.tab<-matrix(unlist(summary(aov.fit)[1]), nrow=length(betas))
  pval<- aov.fit.tab[,5]
  lm.pvals <- data.frame(betas, pval)
  
  #save the tuky pvals
  tukys <- TukeyHSD(aov.fit)
  
  #store important stuff in a list
  results <- list(lm.pvals, tukys)
  names(results) <- c('lm.pvals','tukys')
  
  return(results)
}




#FUNCTION TO PULL OUT SUMMARIES
#ONLY USE THIS FOR FIG 3, where model = type
PullSum.Fig3 <- function(sub){
  SOILMEAS<-unique(sub$soilmeas)
  s <- 0
  summaries <- list()
  for (s in 1:length(SOILMEAS)){
    df <- subset(sub, soilmeas == SOILMEAS[s])
    summaries[[as.character(SOILMEAS[s])]] <- SummarizeThis(data=df, resp.var='soilval', group.var='type')
  }
  return(summaries)
}




#FUNCTION TO PULL OUT LME4 PARAMS AND ANOVA,
#ONLY USE THIS FOR FIG 3, where model = type
library(lme4)
PullLME4.Fig3 <- function(sub){
  SOILMEAS<-unique(sub$soilmeas)
  s <- 0
  model.params <- list()
  anova.params <- list()
  for (s in 1:length(SOILMEAS)){
    df <- subset(sub, soilmeas == SOILMEAS[s])
    model <- lmer(soilval ~ type + (1|bk), data = df) #run the full model
    #extract lmer model parameters
    model.params[[as.character(SOILMEAS[s])]] <- ExtractLMER(model)
    
    modela<- lmer(soilval ~ 1 + (1|bk), data = df) #run the reduced model
    termeff<-anova(modela,model) #compare the full and reduced model with ANOVA
    #extract lmer anova parameters from termeff
    anova.params[[as.character(SOILMEAS[s])]] <- ExtractLMER.anova(termeff)
  }
  
  #store important stuff in a list
  results <- list(model.params, anova.params)
  names(results) <- c('model.params','anova.params')
  
  return(results)
}




#FUNCTION TO PULL OUT LM PARAMS AND TUKEYS
#ONLY USE THIS FOR FIG 3, where model = type
PullLM.Fig3 <- function(sub){
  SOILMEAS<-unique(sub$soilmeas)
  s <- 0
  lm.pvals <- list()
  tukys <- list()
  for (s in 1:length(SOILMEAS)){
    df <- subset(sub, soilmeas == SOILMEAS[s])
    aov.fit <- aov(soilval ~ type, data = df)
    betas <- c('type','Residuals')
    
    #save the anova pval 
    aov.fit.tab<-matrix(unlist(summary(aov.fit)[1]), nrow=length(betas))
    pval<- aov.fit.tab[,5]
    lm.pvals[[as.character(SOILMEAS[s])]] <- data.frame(betas, pval)
    
    #save the tuky pvals
    tuk<-TukeyHSD(aov.fit, 'type')[1]
    tukys[[as.character(SOILMEAS[s])]] <- data.frame(tuk)
  }
  
  #store important stuff in a list
  results <- list(lm.pvals, tukys)
  names(results) <- c('lm.pvals','tukys')
  
  return(results)
}




#FUNCTION TO PULL OUT SUMMARIES
#ONLY USE THIS FOR FIG 4, where resp.var = soilval and group.var = 'mvtrt'
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




#FUNCTION TO PULL OUT LME4 PARAMS AND ANOVA,
#ONLY USE THIS FOR FIG 4, where the model is soilval ~ mivi
library(lme4)
PullLME4.Fig4 <- function(sub){
  SOILMEAS<-unique(sub$soilmeas)
  s <- 0
  model.params <- list()
  anova.params <- list()
  for (s in 1:length(SOILMEAS)){
    df <- subset(sub, soilmeas == SOILMEAS[s])
    model <- lmer(soilval ~ mivi + (1|bk), data = df) #run the full model
    #extract lmer model parameters
    model.params[[as.character(SOILMEAS[s])]] <- ExtractLMER(model)
    
    modela<- lmer(soilval ~ 1 + (1|bk), data = df) #run the reduced model
    termeff<-anova(modela,model) #compare the full and reduced model with ANOVA
    #extract lmer anova parameters from termeff
    anova.params[[as.character(SOILMEAS[s])]] <- ExtractLMER.anova(termeff)
  }
  
  #store important stuff in a list
  results <- list(model.params, anova.params)
  names(results) <- c('model.params','anova.params')
  
  return(results)
}




#FUNCTION TO PULL OUT LM PARAMS AND TUKEYS
#ONLY USE THIS FOR FIG 4, where the model is soilval ~ mivi
PullLM.Fig4 <- function(sub){
  SOILMEAS<-unique(sub$soilmeas)
  s <- 0
  lm.pvals <- list()
  tukys <- list()
  for (s in 1:length(SOILMEAS)){
    df <- subset(sub, soilmeas == SOILMEAS[s])
    aov.fit <- aov(soilval ~ mivi, data = df)
    betas <- c('type','Residuals')
    
    #save the anova pval 
    aov.fit.tab<-matrix(unlist(summary(aov.fit)[1]), nrow=length(betas))
    pval<- aov.fit.tab[,5]
    lm.pvals[[as.character(SOILMEAS[s])]] <- data.frame(betas, pval)
    
  }
  
  #store important stuff in a list
  results <- list(lm.pvals)
  names(results) <- c('lm.pvals')
  
  return(results)
}





#FUNCTION TO PULL OUT SUMMARIES
#ONLY USE THIS FOR FIG 5, where resp.var = sF and group.var = 'mvtrt', 'comptrt','trt'
PullSum.Fig5 <- function(sub){
  SOILMEAS<-unique(sub$soilmeas) #this is the same for sub.mivi and sub.total
  s <- 0
  summaries.mv <- list()
  summaries.comp <- list()
  summaries.trt <- list()
  for (s in 1:length(SOILMEAS)){
    df <- subset(sub, soilmeas == SOILMEAS[s])
    summaries.mv[[as.character(SOILMEAS[s])]] <- SummarizeThis(data=df, resp.var='sF', group.var='mvtrt') #by mvtrt
    summaries.comp[[as.character(SOILMEAS[s])]] <- SummarizeThis(data=df, resp.var='sF', group.var='comptrt') #by comptrt
    summaries.trt[[as.character(SOILMEAS[s])]] <- SummarizeThis(data=df, resp.var='sF', group.var='trt') #by mvtrt x comptrt (trt)
  }
  summaries<-list(summaries.mv, summaries.comp, summaries.trt)
  
  return(summaries)
}




#FUNCTION TO PULL OUT LME4 PARAMS AND ANOVA,
#ONLY USE THIS FOR FIG 5, where the model is sF ~ biomval * comptrt
library(lme4)
PullLME4.Fig5 <- function(sub){
  SOILMEAS<-unique(sub$soilmeas) #this is the same for sub.mivi and sub.total
  s <- 0
  model.params <- list()
  anova.paramsA <- list()
  anova.paramsB <- list()
  anova.paramsC <- list()
  for (s in 1:length(SOILMEAS)){
    df <- subset(sub, soilmeas == SOILMEAS[s])
    model <- lmer(sF ~ biomval + comptrt + biomval:comptrt + (1|bk), data = df) #run the full model
    #extract lmer model parameters
    model.params[[as.character(SOILMEAS[s])]] <- ExtractLMER(model)
    
    modela<- lmer(sF ~ biomval + comptrt + (1|bk), data = df) #run the reduced model
    termeff<-anova(modela,model) #compare the full and reduced model with ANOVA
    #extract lmer anova parameters from termeff
    anova.paramsA[[as.character(SOILMEAS[s])]] <- ExtractLMER.anova(termeff)
    
    modelb<- lmer(sF ~ biomval + (1|bk), data = df) #run the reduced model
    termeff<-anova(modelb,modela) #compare the fuller and reduced model with ANOVA
    #extract lmer anova parameters from termeff
    anova.paramsB[[as.character(SOILMEAS[s])]] <- ExtractLMER.anova(termeff)
    
    modelc<- lmer(sF ~ 1 + (1|bk), data = df) #run the reduced model
    termeff<-anova(modelc,modelb) #compare the fuller and reduced model with ANOVA
    #extract lmer anova parameters from termeff
    anova.paramsC[[as.character(SOILMEAS[s])]] <- ExtractLMER.anova(termeff)
  }
  
  #store important stuff in a list
  results <- list(model.params, anova.paramsA, anova.paramsB, anova.paramsC)
  names(results) <- c('model.params','anova.params.wo.int','anova.params.wo.comp','anova.params.wo.biom')
  
  return(results)
}




#FUNCTION TO PULL OUT LM PARAMS AND TUKEYS
#ONLY USE THIS FOR FIG 5, where the model is sF ~ biomval * comptrt
PullLM.Fig5 <- function(sub){
  SOILMEAS<-unique(sub$soilmeas)
  s <- 0
  lm.pvals <- list()
  tukys <- list()
  for (s in 1:length(SOILMEAS)){
    df <- subset(sub, soilmeas == SOILMEAS[s])
    aov.fit <- aov(sF ~ biomval + comptrt + biomval:comptrt, data = df) #run the model
    betas <- c('biomval','comptrt','biomval:comptrt', 'Residuals')
    
    #save the anova pvals
    aov.fit.tab<-matrix(unlist(summary(aov.fit)[1]), nrow=length(betas))
    pval<- aov.fit.tab[,5]
    lm.pvals[[as.character(SOILMEAS[s])]] <- data.frame(betas, pval)
    
    #save the tuky pvals
    tuk<-TukeyHSD(aov.fit, 'comptrt')[1]
    tukys[[as.character(SOILMEAS[s])]] <- data.frame(tuk)
    
  }
  
  #store important stuff in a list
  results <- list(lm.pvals, tukys)
  names(results) <- c('lm.pvals','tukys')
  
  return(results)
}





