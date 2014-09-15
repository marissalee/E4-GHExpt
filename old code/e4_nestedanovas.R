#E4 nested ANOVAs for Model1 and Model2

setwd("~/Desktop/E4_R")
library(lme4)

##################################
#import data
data<-read.table('e4_trimmedData.txt',header=T)
data$total<-data$Mvabund + data$pavi + data$sobi
data$Compabund<-data$pavi + data$sobi
Empty<-data[data$type=='Empty',]
Mivi<-data[data$type=='Mivi',]
Pavi<-data[data$type=='Pavi',]
Sobi<-data[data$type=='Sobi',]
CompEmpty<-data[data$type=='CompEmpty',]
CompPavi<-data[data$type=='CompPavi',]
CompSobi<-data[data$type=='CompSobi',]

# Set-up data
dataa<-rbind(CompEmpty, CompPavi, CompSobi)
dataa$bk<-as.factor(dataa$bk)
dataa$logtotal<-log(dataa$total)
#xs
logtotal<-dataa$logtotal #x1
mvtrt<-dataa$Mvtrt #x2
comptrt<-dataa$comptrt #x3
bk<-dataa$bk #block
#ys
soilmoi<-dataa$soilmoi 
nodi<-dataa$nodi
nhno<-dataa$nh.no
nitrifd<-dataa$nitrifd
minzd<-dataa$minzd
response<-cbind(soilmoi,nodi,nhno,nitrifd,minzd)


##################################
##################################
#Test of fixed effects, Model1

responsecols<-c(1,2,4)
responsenames<-c('Soil moisture (%)','Nitrate (ugN/G)','Nitrification (ugN/G*d)')
results<-list()
term.names1<-c('mvtrt','mvtrt^2','comptrt','mvtrt x comptrt')

for (r in 1:length(responsecols)){
  data1<-data.frame(response[,responsecols[r]], logtotal, mvtrt, comptrt, bk)
  
  #test of fixed effects - Model1
  model1<-lmer(data1[,1] ~ mvtrt + I(mvtrt^2) + comptrt + mvtrt:comptrt+(1|bk), data=data1)
  model1a<-lmer(data1[,1] ~ mvtrt + I(mvtrt^2) + comptrt + (1|bk), data=data1)
  model1b<-lmer(data1[,1] ~ mvtrt + I(mvtrt^2) + (1|bk), data=data1)
  model1c<-lmer(data1[,1] ~ mvtrt +  (1|bk), data=data1)
  model1d<-lmer(data1[,1] ~ 1 + (1|bk), data=data1)
  term4<-anova(model1, model1a)
  term3<-anova(model1a, model1b)
  term2<-anova(model1b, model1c)
  term1<-anova(model1c, model1d)
  terms<-list(term1=term1,term2=term2,term3=term3,term4=term4)
  
  store<-numeric(0)
        for (i in 1:length(terms)){
          term<-data.frame(terms[i])
          delAIC<-round(term[2,2]-term[1,2],digits=2)
          ChiSq<-round(term[2,5],digits=2)
          ChiDf<-term[2,6]
          Pval<-round(term[2,7],digits=4)
          termrow<-c(delAIC,ChiDf,ChiSq,Pval)
          store<-rbind(store,termrow)
          
        }
  row.names(store)<-term.names1
  colnames(store)<-c('deltaAIC','ChiDf','ChiSq','Pval')
  results[[r]]<-store
  
}
names(results)<-responsenames
results

model1results<-data.frame(results)
colnames(model1results)
rownames(model1results)

write.table(model1results, file='model1results.txt', sep='\t', row.names=T, col.names=T)


##################################
##################################
#Test of fixed effects, Model2

responsecols<-c(1,2,4)
responsenames<-c('Soil moisture (%)','Nitrate (ugN/G)','Nitrification (ugN/G*d)')
term.names2<-c('mvtrt','mvtrt^2','logtotal','logtotal^2','mvtrt x comptrt')
store<-numeric(0)
results2<-list()

for (r in 1:length(responsecols)){
  data1<-data.frame(response[,responsecols[r]], logtotal, mvtrt, comptrt, bk)
  
  #test of fixed effects - Model2
  model2<-lmer(data1[,1] ~ mvtrt + I(mvtrt^2) + logtotal + I(logtotal^2) + mvtrt:logtotal +(1|bk), data=data1)
  model2a<-lmer(data1[,1] ~ mvtrt + I(mvtrt^2) + logtotal + I(logtotal^2) +(1|bk), data=data1)
  model2b<-lmer(data1[,1] ~ mvtrt + I(mvtrt^2) + logtotal +(1|bk), data=data1)
  model2c<-lmer(data1[,1] ~ mvtrt + I(mvtrt^2) +(1|bk), data=data1)
  model2d<-lmer(data1[,1] ~ mvtrt +(1|bk), data=data1)
  model2e<-lmer(data1[,1] ~ 1 +(1|bk), data=data1)
  term5<-anova(model2, model2a)
  term4<-anova(model2a, model2b)
  term3<-anova(model2b, model2c)
  term2<-anova(model2c, model2d)
  term1<-anova(model2d, model2e)
  terms<-list(term1=term1,term2=term2,term3=term3,term4=term4,term5=term5)
  
  store<-numeric(0)
  for (i in 1:length(terms)){
    term<-data.frame(terms[i])
    delAIC<-round(term[2,2]-term[1,2],digits=2)
    ChiSq<-round(term[2,5],digits=2)
    ChiDf<-term[2,6]
    Pval<-round(term[2,7],digits=4)
    termrow<-c(delAIC,ChiDf,ChiSq,Pval)
    store<-rbind(store,termrow)
    
  }
  row.names(store)<-term.names2
  colnames(store)<-c('deltaAIC','ChiDf','ChiSq','Pval')
  results2[[r]]<-store
  
}
names(results2)<-responsenames
results2

model2results<-data.frame(results2)
colnames(model2results)
rownames(model2results)

write.table(model2results, file='model2results.txt', sep='\t', row.names=T, col.names=T)

###############







