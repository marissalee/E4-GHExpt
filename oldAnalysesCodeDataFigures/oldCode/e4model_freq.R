#e4model_freq.R
# 3/25/13
setwd("~/Desktop/E4_R")

library(nlme) #for mixed effect model, lme()
library(multcomp) #for setting contrasts

##################################
#import data
data<-read.table('e4_trimmedData.txt',header=T)
data$total<-data$Mvabund + data$pavi + data$sobi
data$Compabund<-data$pavi + data$sobi

#make datasets
Empty<-data[data$type=='Empty',]
Mivi<-data[data$type=='Mivi',]
Pavi<-data[data$type=='Pavi',]
Sobi<-data[data$type=='Sobi',]
CompEmpty<-data[data$type=='CompEmpty',]
CompPavi<-data[data$type=='CompPavi',]
CompSobi<-data[data$type=='CompSobi',]

##################################
#Model structure
# response ~ Mvtrt * comptrt * Mvtrtxcomptrt
#where...
# Mvtrt is a factor with 4 levels: 1/6 Mv, 2/6 Mv, 4/6 Mv, 5/6 Mv
# comptrt is a factor with 3 levels: N, P, S
# responses include: 1) Mvabund (PS), 2) Compabund (PS), 3) soilmoi (PS), 4) nodi (PS), 5) no.nh (PS), 6) nitrifd (N), 7) minzd (N)

# Set-up data
dataa<-rbind(CompEmpty, CompPavi, CompSobi)
dataa$bk<-as.factor(dataa$bk)
dataa$Mvtrt<-as.factor(dataa$Mvtrt)

mvtrt<-dataa$Mvtrt #x1
comptrt<-dataa$comptrt #x2
bk<-dataa$bk #block
#ys
mvabund<-dataa$Mvabund 
mvrelabund<-dataa$MVrelabund 
soilmoi<-dataa$soilmoi 
nodi<-dataa$nodi
nhno<-dataa$nh.no
nitrifd<-dataa$nitrifd
minzd<-dataa$minzd
response<-cbind(mvabund,mvrelabund,soilmoi,nodi,nhno,nitrifd,minzd)
dim(response)[2]

anova.models.lme<-list()

for (i in 1:dim(response)[2]){ #loop through each response
  data1 <- data.frame(response[,i], mvtrt, comptrt, bk) #define the dataframe
  responsenow<-response[,i]
  model.lme<-lme(fixed=responsenow ~ mvtrt + comptrt + mvtrt*comptrt, random=~1|bk, data=data1, method="ML") #define the model, Model type 2 - Frequentist version with block effects
  anova.models.lme[[i]]<-anova(model.lme) #store the anova results  
}

anova.models.lme
names(anova.models.lme)<-colnames(response)  
table.anovas.lme<-do.call('rbind',anova.models.lme)
table.anovas.lme

write.table(table.anovas.lme, file='model1_mv1to5.txt', sep='\t', row.names=T, col.names=T)

#######################################
#Nested models
#Nodi
data1 <- data.frame(nodi, mvtrt, comptrt, bk) #define the dataframe
lme.model<-lme(nodi~ mvtrt+comptrt, random = ~1|bk, data=data1)
anova(lme.model)
lme.model2<-lme(nodi~ comptrt+mvtrt, random = ~1|bk, data=data1)
anova(lme.model2)
lme.model3<-lme(nodi~ comptrt, random = ~1|bk, data=data1)
anova(lme.model3)
lme.posthocs <- glht(lme.model3, linfct=mcp(comptrt="Tukey")) 
summary(lme.posthocs)

#Nitrifd
data1 <- data.frame(nitrifd, mvtrt, comptrt, bk) #define the dataframe
lme.model<-lme(nitrifd~ mvtrt+comptrt, random = ~1|bk, data=data1)
anova(lme.model)
lme.model2<-lme(nitrifd~ comptrt+mvtrt, random = ~1|bk, data=data1)
anova(lme.model2)
lme.model3<-lme(nitrifd~ comptrt, random = ~1|bk, data=data1)
anova(lme.model3)
lme.posthocs <- glht(lme.model3, linfct=mcp(comptrt="Tukey")) 
summary(lme.posthocs)

#Minzd
data1 <- data.frame(minzd, mvtrt, comptrt, bk) #define the dataframe
lme.model<-lme(minzd~ mvtrt+comptrt, random = ~1|bk, data=data1)
anova(lme.model)
lme.model2<-lme(minzd~ comptrt+mvtrt, random = ~1|bk, data=data1)
anova(lme.model2)
lme.model3<-lme(minzd~ comptrt, random = ~1|bk, data=data1)
anova(lme.model3)
lme.posthocs <- glht(lme.model3, linfct=mcp(comptrt="Tukey")) 
summary(lme.posthocs)

##############################################
##############################################
##############################################
#Includes Mvtrt 0/6 through 5/6 (not Mvtrt 6/6)

# Set-up data
datam<-rbind(Empty,Pavi, Sobi,CompEmpty, CompPavi, CompSobi)
datam$bk<-as.factor(datam$bk)
datam$Mvtrt<-as.factor(datam$Mvtrt)
summary(datam)

mvtrt<-datam$Mvtrt #x1
comptrt<-datam$comptrt #x2
bk<-datam$bk #block
#ys
mvabund<-datam$Mvabund 
mvrelabund<-datam$MVrelabund 
soilmoi<-datam$soilmoi 
nodi<-datam$nodi
nhno<-datam$nh.no
nitrifd<-datam$nitrifd
minzd<-datam$minzd
response<-cbind(mvabund,mvrelabund,soilmoi,nodi,nhno,nitrifd,minzd)
dim(response)[2]

anova.models.lme<-list()

for (i in 1:dim(response)[2]){ #loop through each response
  data1 <- data.frame(response[,i], mvtrt, comptrt, bk) #define the dataframe
  responsenow<-response[,i]
  model.lme<-lme(fixed=responsenow ~ mvtrt + comptrt + mvtrt*comptrt, random=~1|bk, data=data1, method="ML") #define the model, Model type 2 - Frequentist version with block effects
  anova.models.lme[[i]]<-anova(model.lme) #store the anova results  
}

anova.models.lme
names(anova.models.lme)<-colnames(response)  
table.anovas.lme<-do.call('rbind',anova.models.lme)
table.anovas.lme

write.table(table.anovas.lme, file='model1_mv0to5.txt', sep='\t', row.names=T, col.names=T)

#######################################
#Nested models
#Soilmoi
data1 <- data.frame(soilmoi, mvtrt, comptrt, bk) #define the dataframe
lme.model<-lme(soilmoi~ mvtrt+comptrt+mvtrt*comptrt, random = ~1|bk, data=data1)
anova(lme.model)
lme.model2<-lme(soilmoi~ comptrt+mvtrt, random = ~1|bk, data=data1)
anova(lme.model2)
lme.model3<-lme(soilmoi~ comptrt, random = ~1|bk, data=data1)
anova(lme.model3)
lme.posthocs <- glht(lme.model3, linfct=mcp(comptrt="Tukey")) 
summary(lme.posthocs)

#Nodi
data1 <- data.frame(nodi, mvtrt, comptrt, bk) #define the dataframe
lme.model<-lme(nodi~ mvtrt+comptrt+mvtrt*comptrt, random = ~1|bk, data=data1)
anova(lme.model)
lme.model2<-lme(nodi~ comptrt+mvtrt+comptrt*mvtrt, random = ~1|bk, data=data1)
anova(lme.model2)
lme.full.posthocss$nodi

#Nhno
data1 <- data.frame(nhno, mvtrt, comptrt, bk) #define the dataframe
lme.model<-lme(nhno~ mvtrt+comptrt, random = ~1|bk, data=data1)
anova(lme.model)
lme.model2<-lme(nhno~ comptrt+mvtrt, random = ~1|bk, data=data1)
anova(lme.model2)
lme.model3<-lme(nhno~ comptrt, random = ~1|bk, data=data1)
anova(lme.model3)
lme.posthocs <- glht(lme.model3, linfct=mcp(comptrt="Tukey")) 
summary(lme.posthocs)

