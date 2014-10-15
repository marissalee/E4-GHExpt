#e4_Fig4stats.R
#Stats for Figure 4: Variation in soil measures among mixed pots


### Read-in all the custom functions for doing stats ###
source('e4CodePackage_100614/statFxns.R')


### Prep dataframe ###
source('e4CodePackage_100614/e4_prepdfFig4.R')
#str(data4)


### Make a column for trt (mvtrt x comptrt) ### -- this is important for looking at cross-category means
data4t <- data4
data4t$trt <- paste(data4[,'mvtrt'], data4[,'comptrt'], sep='')


### Subset so that only have data with biommeas == mivi OR biommeas == total ###
sub.mivi<-subset(data4t, biommeas == "mivi")
sub.total<-subset(data4t, biommeas == "total")


### Make a new column for transformed data ###
sub.total$logtotal<-log(sub.total$biomval)


### I'm going to be looping through each soil measure for all of these, so... ###
SOILMEAS<-unique(sub.mivi$soilmeas) #this is the same for sub.mivi and sub.total




### Summarize variation in soil measures by mvtrt, comptrt, trt ###

s <- 0
summaries.mv <- list()
summaries.comp <- list()
summaries.trt <- list()
for (s in 1:length(SOILMEAS)){
  df <- subset(sub.mivi, soilmeas == SOILMEAS[s])
  summaries.mv[[as.character(SOILMEAS[s])]] <- SummarizeThis(data=df, resp.var='sF', group.var='mvtrt') #by mvtrt
  summaries.comp[[as.character(SOILMEAS[s])]] <- SummarizeThis(data=df, resp.var='sF', group.var='comptrt') #by comptrt
  summaries.trt[[as.character(SOILMEAS[s])]] <- SummarizeThis(data=df, resp.var='sF', group.var='trt') #by mvtrt x comptrt (trt)
}
summaries.mv
summaries.comp
summaries.trt




### Mixed effects model to predict soil measure using mivi biomass and comptrt ###

library(lme4)
s <- 0
model.params <- list()
anova.paramsA <- list()
anova.paramsB <- list()
anova.paramsC <- list()
for (s in 1:length(SOILMEAS)){
  df <- subset(sub.mivi, soilmeas == SOILMEAS[s])
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
model.params
anova.paramsA # without biomval:comptrt
anova.paramsB # without comptrt
anova.paramsC # without biomval




### Linear regression to predict soil measure using mivi biomass and comptrt ###

s <- 0
lm.pvals <- list()
#tukys <- list()
for (s in 1:length(SOILMEAS)){
  s<-1
  df <- subset(sub.mivi, soilmeas == SOILMEAS[s])
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
lm.pvals
#tukys





