#'checkLME.R'
#DOUBLE CHECKING LME RESULTS
source('e4Code/e4_prepdfFig3n4.R') #use the same starting data as fig3
sub1<-subset(data3, comptrt == 'N') #subset data
sub2<-subset(data3, comptrt == 'N' & type != 'Empty') #subset data

dim(sub1)
dim(sub2)

#CHOOSE THE DATASET
sub<-sub1

#loop through each soil measure
SOILMEAS<-unique(sub$soilmeas)
SOILMEAS

s <- 7 #nodi

#storage containers
models <- list()
store<-numeric(0)
# anova.tabs <-list()

for (s in 1:length(SOILMEAS)){ 
  #subset data by soil measure
  df <- subset(sub, soilmeas == SOILMEAS[s])
  head(df)
  
  #run the full model with lmerTest active
  model <- lmer(soilval ~ mivi + I(mivi^2) + (1|bk), data = df) 
  model
  
  #r2 est
  r2<-r2.corr.mer(model)
  r2
  
  #save the anova table
  anova<-round(data.frame(anova(model)), digits=2)
  anova
  
  #fixed effects
  fe<- round(summary(model)$coefficients, digits=2)
  fe
  
  #random effects
  re1<-as.data.frame(VarCorr(model))
  vals<-round(re1[,c('vcov','sdcor')], digits=2)
  re<-data.frame(grp=re1[,'grp'],vals)
  re
  
  #calculate predicted vals using the fe only and attach it to the df
  pred<-predict(model, re.form=NA) #this sets the random effects to 0
  preddf<-data.frame(df, pred=pred)
  
  #save the model output
  models[[as.character(SOILMEAS[s])]] <- list(fe=fe, re=re, anova=anova, r2=r2, preddf=preddf)