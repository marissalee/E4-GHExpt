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
#ONLY USE THIS FOR FIG 3a, where the model is total ~ type
PullSum.Fig3a <- function(sub){
    summaries <- SummarizeThis(data=sub, resp.var='total', group.var='type')
  return(summaries)
}




#FUNCTION TO PULL OUT LME4 PARAMS AND ANOVA,
#ONLY USE THIS FOR FIG 3a, where the model is total ~ type
library(lme4)
PullLME4.Fig3a <- function(sub){
    model <- lmer(total ~ type + (1|bk), data = sub) #run the full model
    #extract lmer model parameters
    model.params <- ExtractLMER(model)
    
    modela<- lmer(total ~ 1 + (1|bk), data = sub) #run the reduced model
    termeff<-anova(modela,model) #compare the full and reduced model with ANOVA
    #extract lmer anova parameters from termeff
    anova.params <- ExtractLMER.anova(termeff)
  
  #store important stuff in a list
  results <- list(model.params, anova.params)
  names(results) <- c('model.params','anova.params')
  
  return(results)
}




#FUNCTION TO PULL OUT LM PARAMS AND TUKEYS
#ONLY USE THIS FOR FIG 3a, where the model is total ~ type
PullLM.Fig3a <- function(sub){
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
PullSum.Fig3b <- function(sub){
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
#ONLY USE THIS FOR FIG 3b, where the model is soilval ~ type
library(lme4)
PullLME4.Fig3b <- function(sub){
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
#ONLY USE THIS FOR FIG 3b, where the model is soilval ~ type
PullLM.Fig3b <- function(sub){
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




#FUNCTION TO PULL OUT Tukey tables
PullTuks <- function(tuktab){
  tukpval<-data.frame(tuktab)
  col<-grep('adj',names(tukpval)) #get the pval column
  p<-round(tukpval[,col], digits=2) #store the pval column and round it off
  nam<-row.names(tukpval) #save the category names
  tuks<-data.frame(nam,p)
  
  return(tuks)
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




#FUNCTION TO Make Fig2 lme tables look pretty
library(doBy)
PrettyLME4 <- function(thing){
  terms<-names(thing[[2]])
  store<-list()
  i<-0
  for (i in 1:length(terms)){
    tmp<-thing[[2]][i]
    store[[i]]<-tmp[[1]]$anova.pval
  }
  pvals<-round(unlist(store), digits=4)
  order<-c(3,2,1)
  result<-data.frame(order,terms, pvals)
  result1<-orderBy(~order,result)
  
  return(result1)
}




#FUNCTION TO Make Fig3b lme tables look pretty
PrettyLME.fig3b <- function(thing){
  ys<-names(thing[[2]])
  store<-list()
  i<-0
  for(i in 1:length(ys)){
    tmp<-thing[[2]][i]
    store[[i]]<-tmp[[1]]$anova.pval
  }
  result<-data.frame('soilmeas'=ys,'pval'=round(unlist(store), digits=2))
  
  return(result)
}




#FUNCTION TO Make Fig4 lme tables look pretty (anova pval and fixed effect coefs)
PrettyLME.fig4 <- function(thing){
  #pvals
  mat.vals<-round(matrix(unlist(thing$anova.params), nrow=length(thing$anova.params), byrow=T), digits=2)
  colnames(mat.vals)<-names(thing$anova.params$nhdi)
  soilmeas<-names(thing$anova.params)
  anovaparams<-data.frame(soilmeas, mat.vals)
  regpval<-anovaparams[,c('soilmeas','anova.pval')] # pvals are here
  
  #lme4 model coefs
  tmp1<-round(matrix(unlist(thing$model.params), nrow=length(thing$model.params), byrow=T), digits=2)
  #make sensible column names
  renam<-names(thing$model.params$nhdi$RE)
  fenam<-names(thing$model.params$nhdi$FE)
  LIST<-list(renam, fenam)
  colnlist<-list()
  i<-0
  for (i in 1:length(LIST)){
    seq1<-rep(LIST[[i]], each=2)
    seq<-rep(c(1,2), times=length(LIST[[i]]))
    coln<-paste(seq1,seq, sep="")
    colnlist[[i]]<-coln
  }
  colnames(tmp1)<-unlist(colnlist)
  modelparams<-data.frame(soilmeas, tmp1)
  regcoefs<-modelparams[,c('FEest1','FEest2')] #intercepts and slopes
  
  
  #put everything together
  regdata<-data.frame(regpval,regcoefs)
  
  return(regdata)
}




#FUNCTION TO Make Fig5 lme tables look pretty (anova pval and fixed effect coefs)
library(stringr)
PrettyLME.fig5 <- function(thing){
  
  ## Pull out pvals for each of the terms in the model
  terms<-names(thing)[-1]
  temp<-str_split(terms, 'anova.params.')
  terms.nice<-unlist(temp)[grep('wo',unlist(temp))]
  
  store<-list()
  i<-0
  for(i in 1:length(terms)){
    #pull out the term
    termthing<-thing[[i+1]]
    
    #unlist the anova table for that term
    mat.vals<-round(matrix(unlist(termthing), nrow=length(termthing), byrow=T), digits=2)
    colnames(mat.vals)<-names(termthing[[1]])
    soilmeas<-names(termthing)
    termparams<-data.frame(soilmeas,mat.vals)
    
    #store the pval for all soil meas
    store[[i]]<-termparams[,c('soilmeas','anova.pval')]
  }
  
  tmp<-matrix(unlist(store), nrow=length(store), byrow=T)
  tmp1<-tmp[,8:14] #just the pvals
  colnames(tmp1)<-soilmeas
  order<-c(3,2,1)
  result<-data.frame(order,terms,tmp1)
  result1<-orderBy(~order,result)
  
  #make the soil measures into rows instead
  result2<-data.frame(t(result1)[c(-1,-2),])
  colnames(result2)<-terms.nice
  pvaltab<-result2
  
  
  ## Pull out the fixed effect coefficients for each soil measure full model
  #unlist the model.params 
  tmp2<-round(matrix(unlist(thing$model.params), nrow=length(thing$model.params), byrow=T), digits=2)
  
  #make sensible column names
  renam.cols<-names(thing$model.params$nhdi$RE)
  renam.rows<-row.names(thing$model.params$nhdi$RE)
  part1<-rep(renam.cols, each=length(renam.rows))
  part2<-rep(seq(1,length(renam.rows)), length(renam.cols))
  newREcols<-paste(part1,part2,sep='')
  
  fenam.cols<-names(thing$model.params$nhdi$FE)
  fenam.rows<-row.names(thing$model.params$nhdi$FE)
  part1<-rep(fenam.cols, each=length(fenam.rows))
  part2<-rep(seq(1,length(fenam.rows)), length(fenam.cols))
  newFEcols<-paste(part1,part2,sep='')
  
  nam.cols<-c(newREcols,newFEcols)
  nam.rows<-names(thing$model.params)
  
  #attach the column names and row identifiers
  colnames(tmp2)<-nam.cols
  tmp3<-data.frame(nam.rows,tmp2)
  
  #subset FEest coef cols
  FEcols<-grep("FEest",colnames(tmp3))
  tmp3[,FEcols]
  
  
  
  ### Pull everything together into a dataframe
  resulttab<-data.frame(pvaltab,tmp3[,FEcols])
  
  
  return(resulttab)
}


