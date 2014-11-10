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




#FUNCTION TO PULL OUT Tukey tables
PullTuks <- function(tuktab){
  tukpval<-data.frame(tuktab)
  col<-grep('adj',names(tukpval)) #get the pval column
  p<-round(tukpval[,col], digits=2) #store the pval column and round it off
  nam<-row.names(tukpval) #save the category names
  tuks<-data.frame(nam,p)
  
  return(tuks)
}



#FUNCTION TO PUT ROW NAMES ON EXPORTED TABLES
MakeTable<-function(thing){
  df <- ldply(thing, data.frame)
  rownames<-unlist(llply(thing, row.names))
  df<-data.frame(rownames, df)
  return(df)
}




#FUNCTION TO REFACTOR SOIL LISTS OF fixed and random effects
SoilUnlist <- function(soillist){
  soilnames<-names(soillist)
  i<-0
  fetabs<-list()
  retabs<-list()
  anovatabs<-list()
  r2tabs<-list()
  
  for (i in 1:length(soillist)){
    fetabs[[as.character(soilnames[i])]]<-soillist[[i]]$fe
    retabs[[as.character(soilnames[i])]]<-soillist[[i]]$re
    anovatabs[[as.character(soilnames[i])]]<-soillist[[i]]$anova
    r2tabs[[as.character(soilnames[i])]]<-soillist[[i]]$r2
  }
  
  result<-list(fetab=fetabs, retab=retabs, anovatab=anovatabs, r2tab=r2tabs)
  return(result)
}


#FUNCTION TO ESTIMATE r2 VALUE FOR MIXED EFFECTS MODELS
#FROM: http://glmm.wikidot.com/faq
r2.corr.mer <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  summary(lmfit)$r.squared
}


####################################################################################################
# FIG 2
####################################################################################################

#FUNCTION TO PULL OUT SUMMARIES
#ONLY USE THIS FOR FIG 2 to summarize biomval by mvtrt and comptrt
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




#FUNCTION TO FIT LME: biomval ~ mvtrt + comptrt + mvtrt:comptrt + (1|bk)
library(lme4)
library(lmerTest)
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






#FUNCTION TO FIT LM AND TUKEYS: biomval ~ mvtrt + comptrt + mvtrt:comptrt 
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



#FUNCTION TO CALC how many times greater a grouped mean is than another
#figure2
GreaterThan.fig2ab <- function(meantab){
  INNERID<-c(1,2,4,5)
  i<-0
  store<-list()
  for (i in 1:length(INNERID)){
    df<-subset(meantab, inner.id == INNERID[[i]])
    mean.P <- df[df$outer.id=='P', 'mean']
    mean.S <- df[df$outer.id=='S', 'mean'] 
    if(sum(df$outer.id=='N') == 1){
      mean.N <- df[df$outer.id=='N', 'mean'] 
      XgreaterthanNP <- mean.N / mean.P 
    } else{
      XgreaterthanNP <- NA
    }
    XgreaterthanPS <- mean.P / mean.S
    store[[i]]<-data.frame(XgreaterthanNP, XgreaterthanPS)
  }
  store1<-matrix(unlist(store), nrow=length(store), byrow=T)
  colnames(store1)<-names(store[[1]])
  tab<-data.frame(INNERID, store1)
  
  return(tab)
}




#FUNCTION TO CALC how many times greater a grouped mean is than another
#figure2c
GreaterThan.fig2c <- function(meantab){
  INNERID<-c(1,2,4,5)
  i<-0
  store<-list()
  for (i in 1:length(INNERID)){
    df<-subset(meantab, inner.id == INNERID[[i]])
    mean.P <- df[df$outer.id=='P', 'mean']
    mean.S <- df[df$outer.id=='S', 'mean'] 
    mean.N <- df[df$outer.id=='N', 'mean'] 
    
    XgreaterthanPN<- mean.P / mean.N 
    XgreaterthanSP <- mean.S / mean.P
    
    store[[i]]<-data.frame(XgreaterthanPN, XgreaterthanSP)
  }
  store1<-matrix(unlist(store), nrow=length(store), byrow=T)
  colnames(store1)<-names(store[[1]])
  tab<-data.frame(INNERID, store1)
  
  return(tab)
}




#FUNCTION TO Make means look pretty from a list with 2 levels (use for Fig 2, Fig 3)
PrettyMeans.2levs<-function(listmeans){
  outerlevs<-length(listmeans)
  store<-numeric(0)
  i<-0
  for (i in 1:outerlevs){
    curr.df<- data.frame(listmeans[[i]])
    curr.outerid<-rep(names(listmeans)[i], dim(curr.df)[1])
    curr.df$outer<-curr.outerid
    
    store<-rbind(store, curr.df)
  }
  
  colnames(store)[1]<-'inner'
  result<-data.frame('outer.id'=store$outer, 
                     'inner.id'=store$inner, 
                     'mean'=store$mean, 
                     'se'=store$se, 
                     'n'=store$n)
  return(result)
}




####################################################################################################
# FIG 3
####################################################################################################

#FUNCTION TO PULL OUT SUMMARIES
#ONLY USE THIS FOR FIG 3a, where the model is total ~ type
PullSum.Fig3.total <- function(sub){
    summaries <- SummarizeThis(data=sub, resp.var='total', group.var='type')
  return(summaries)
}




#FUNCTION TO FIT LME: total ~ type + (1|bk)
library(lme4)
library(lmerTest)
FitLME.Fig3.total <- function(sub){
  model <- lmer(total ~ type + (1|bk), data = sub) #run the full model
  
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
#   modela <- lmer(total ~ 1 + (1|bk), data = sub)
#   
#   #evaluate change in AIC by reducing models
#   termeff.a<-anova(modela,model) 
#   
#   #extract lmer anova parameters from termeffs
#   row.a<- ExtractLMER.anova(termeff.a)
#   tab <- rbind(row.a)
#   wo<-c('wo.type')
#   anova.tab<-data.frame(wo,tab)
  
#  results<-list(model, anova.tab)
  results<-list(fe=fe,re=re, anova=anova, r2=r2)
  
  return(results)
}




#FUNCTION TO PULL OUT LM PARAMS AND TUKEYS
#ONLY USE THIS FOR FIG 3a, where the model is total ~ type
PullLM.Fig3.total <- function(sub){
    aov.fit <- aov(total ~ type, data = sub)
    betas <- c('type','Residuals')
    
    #save the anova pval 
    aov.fit.tab<-matrix(unlist(summary(aov.fit)[1]), nrow=length(betas))
    pval <- aov.fit.tab[,5]
    lm.pvals <- data.frame(betas, pval)
    
    #save the tuky pvals
    tuk<-TukeyHSD(aov.fit, 'type')[1]
    tukys <- data.frame(tuk)
  
  #store important stuff in a list
  results <- list(lm.pvals, tukys)
  names(results) <- c('lm.pvals','tukys')
  
  return(results)
}




#FUNCTION TO PULL OUT SUMMARIES
#ONLY USE THIS FOR FIG 3b, where the model is soilval ~ type
PullSum.Fig3.soil <- function(sub){
  SOILMEAS<-unique(sub$soilmeas)
  s <- 0
  summaries <- list()
  for (s in 1:length(SOILMEAS)){
    df <- subset(sub, soilmeas == SOILMEAS[s])
    summaries[[as.character(SOILMEAS[s])]] <- SummarizeThis(data=df, resp.var='soilval', group.var='type')
  }
  return(summaries)
}




#FUNCTION TO FIT LME: soilval ~ type + (1|bk)
library(lme4)
library(lmerTest)
FitLME.Fig3.soil <- function(sub){
  SOILMEAS<-unique(sub$soilmeas)
  s <- 0
  models <- list()
#   anova.tabs <- list()
  for (s in 1:length(SOILMEAS)){
    df <- subset(sub, soilmeas == SOILMEAS[s])
    
    model <- lmer(soilval ~ type + (1|bk), data = df) #run the full model
    
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
    
    models[[as.character(SOILMEAS[s])]]<- list(fe=fe,re=re, anova=anova, r2=r2)
    
#     modela <- lmer(soilval ~ 1 + (1|bk), data = df) #run the reduced model
#     
#     #evaluate change in AIC by reducing models
#     termeff.a<-anova(modela,model) 
#     
#     #extract lmer anova parameters from termeffs
#     row.a<- ExtractLMER.anova(termeff.a)
#     tab <- rbind(row.a)
#     wo <- c('wo.type')
#     anova.tab <- data.frame(wo,tab)
#     anova.tabs[[as.character(SOILMEAS[s])]] <- anova.tab
    
#     results<-list(model, anova.tab)
  }
  
  #store important stuff in a list
#   results <- list(models, anova.tabs)
#   names(results) <- c('models','anova.tabs')
  results <- models
  
  return(results)
}




#FUNCTION TO PULL OUT LM PARAMS AND TUKEYS
#ONLY USE THIS FOR FIG 3b, where the model is soilval ~ type
PullLM.Fig3.soil <- function(sub){
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





####################################################################################################
# FIG 4
####################################################################################################

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




#FUNCTION TO FIT LME: soilmeas ~ mivi + I(mivi^2) + (1|bk)
library(lme4)
library(lmerTest)
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
    keep<-predict(model, re.form=NA) #this sets the random effects to 0
    store<-c(store,keep) #make a big vector 
    
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
    
    #save the model output
    models[[as.character(SOILMEAS[s])]] <- list(fe=fe, re=re, anova=anova, r2=r2)
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






####################################################################################################
# FIG 5
####################################################################################################

#FUNCTION TO PULL OUT SUMMARIES
#ONLY USE THIS FOR FIG 5, where resp.var = sF and group.var = 'comptrt'
PullSum.Fig5 <- function(sub){
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




#FUNCTION TO FIT LME: sF ~ biomval + I(biomval^2) + comptrt + biomval:comptrt + (1|bk)
FitLME.fig5.mivi <- function(sub){
  
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
    
    models[[as.character(SOILMEAS[s])]]<- list(fe=fe,re=re, anova=anova, r2=r2)
    
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





#FUNCTION TO FIT LME: sF ~ biomval + I(biomval^2) + (1|bk)
FitLME.fig5.total <- function(sub){
  
  #loop through each soil measure
  SOILMEAS<-unique(sub$soilmeas)
  s <- 0
  
  #storage containers
  models <- list()
  anova.tabs <-list()
  
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
    
    models[[as.character(SOILMEAS[s])]]<- list(fe=fe,re=re, anova=anova, r2=r2)
    
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




#FUNCTION TO Predict LME: sF ~ biomval + I(biomval^2) + (1|bk)
PredLME.fig5.total <- function(sub){
  
  #loop through each soil measure
  SOILMEAS<-unique(sub$soilmeas)
  s <- 0
  
  #storage containers
  store<-numeric(0)
  
  for (s in 1:length(SOILMEAS)){
    
    #subset data by soil measure
    df <- subset(sub, soilmeas == SOILMEAS[s])
    
    #run the full model with lmerTest active
    model <- lmer(sF ~ biomval + I(biomval^2) + (1|bk), data = df) 
    
    keep<-predict(model, re.form=NA) #this sets the random effects to 0
    store<-c(store,keep) #make a big vector   
  }
  #   result <- list(models, anova.tabs)
  result <- store
  return(result)
  
}





