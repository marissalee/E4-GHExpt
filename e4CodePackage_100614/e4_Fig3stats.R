#e4_Fig3stats.R
#Stats for Figure 3: Variation in soil measures among monocultures


### Read-in all the custom functions for doing stats ###
source('e4CodePackage_100614/statFxns.R')


### Prep dataframe ###
source('e4CodePackage_100614/prepdfFig3.R')
#str(data3)


### Subset so that only have data from monocultures ###
sub1<-subset(data3.4, type == 'Mivi' | type == 'Empty' | type == 'Pavi' | type == 'Sobi')


### I'm going to be looping through each soil measure for all of these, so... ###
SOILMEAS<-unique(sub1$soilmeas)




### Summarize variation in soil measures by monoculture type ###

s <- 0
summaries <- list()
for (s in 1:length(SOILMEAS)){
  df <- subset(sub1, soilmeas == SOILMEAS[s])
  summary <- SummarizeThis(data=df, resp.var='soilval', group.var='type')
  summaries[[as.character(SOILMEAS[s])]] <- summary
}
summaries




### Mixed effects model to predict soil measure ###

library(lme4)
s <- 0
model.params <- list()
anova.params <- list()
for (s in 1:length(SOILMEAS)){
  df <- subset(sub1, soilmeas == SOILMEAS[s])
  model <- lmer(soilval ~ type + (1|bk), data = df) #run the full model
  #extract lmer model parameters
  model.params[[as.character(SOILMEAS[s])]] <- ExtractLMER(model)
  
  modela<- lmer(soilval ~ 1 + (1|bk), data = df) #run the reduced model
  termeff<-anova(modela,model) #compare the full and reduced model with ANOVA
  #extract lmer anova parameters from termeff
  anova.params[[as.character(SOILMEAS[s])]] <- ExtractLMER.anova(termeff)
}
model.params
anova.params




### Linear regression model and post-hoc tests ###

s <- 0
lm.pvals <- list()
tukys <- list()
for (s in 1:length(SOILMEAS)){
 s<-1
  df <- subset(sub1, soilmeas == SOILMEAS[s])
  lm <- lm(soilval ~ type, data = df) #run the model
  lmfit<-anova(lm) #test the significance of the term type
  
  #save the anova pval and tuky pvals
  lm.pvals[[as.character(SOILMEAS[s])]]<-anova(lmfit)$'Pr(>F)'[1]
  tukys[[as.character(SOILMEAS[s])]]<-TukeyHSD(lmfit)$type
}
lm.pvals
tukys




