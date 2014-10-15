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
  


