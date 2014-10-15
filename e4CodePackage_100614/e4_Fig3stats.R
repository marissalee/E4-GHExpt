#e4_Fig3stats.R
#Stats for Figure 3: Variation in soil measures among monocultures

# use this dataset
#str(data)
removecols<-c('julydate',
              'pavi','sobi','weed','relmivi','compabund',
              'notes')
indx<-colnames(data) %in% removecols
data.r<-data[,!indx]
ru.data<- data.frame(potid=data.r$potid,
                     bk=data.r$bk,
                     type=data.r$type,
                     comptrt=data.r$comptrt,
                     mvtrt=data.r$mvtrt,
                     mivi=data.r$mivi,
                     total=data.r$total,
                     nhdi=data.r$nhdi,
                     nodi=data.r$nodi,
                     totdi=data.r$totdi,
                     ammonifd=data.r$ammonifd,
                     nitrifd=data.r$nitrifd,
                     minzd=data.r$minzd,
                     soilmoi=data.r$soilmoi)
data3.3 <- ru.data
## Reshape so that plant biomass values are all in one column (biomval), with an identifier column to identify what type of biomass that value represents (biommeas) and add a column that weights the soilvals by total plant biomass
data3.4 <- melt(data3.3, measure.vars=c('nhdi','nodi','totdi','ammonifd','nitrifd','minzd','soilmoi'), id.vars=c('potid','bk','type','comptrt','mvtrt','mivi','total'))
whichvar<-which(colnames(data3.4)=='variable')
whichval<-which(colnames(data3.4)=='value')
colnames(data3.4)[whichvar]<-'soilmeas'
colnames(data3.4)[whichval]<-'soilval'


# subset so that only have data from monocultures
sub1<-subset(data3.4, type == 'Mivi' | type == 'Empty' | type == 'Pavi' | type == 'Sobi')

# read-in all the custom functions for doing stats
source('e4CodePackage_100614/statFxns.R')

#I'm going to be looping through each soil measure for all of these, so...
SOILMEAS<-unique(sub1$soilmeas)


##1. summarize variation in soil measures by monoculture type
s <- 0
summaries <- list()
for (s in 1:length(SOILMEAS)){
  df <- subset(sub1, soilmeas == SOILMEAS[s])
  summary <- SummarizeThis(data=df, resp.var='soilval', group.var='type')
  summaries[[as.character(SOILMEAS[s])]] <- summary
}
summaries



##2. run a mixed effects model to predict soil measure
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



##3. run a linear regression model and post-hoc tests
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

