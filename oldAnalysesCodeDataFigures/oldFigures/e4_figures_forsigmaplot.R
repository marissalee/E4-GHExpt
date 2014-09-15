#e4 figures - barcharts
# 2/18/13
setwd("~/Desktop/E4_R")
data<-read.table('e4_trimmedData.txt',header=T)
data$bk<-as.factor(data$bk)
data$Mvtrt<-as.factor(data$Mvtrt)
data$total<-data$Mvabund + data$pavi + data$sobi

##################################
summarySE <- function(data=NULL,measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) 
{
  require(doBy)
  length2 <- function (x, na.rm=FALSE) {if (na.rm)sum(!is.na(x)) else length(x)}
  formula <- as.formula(paste(measurevar,paste(groupvars, collapse=" + "), sep=" ~ ")) # Collapse the data
  datac <- summaryBy(formula, data=data,FUN=c(length2,mean,sd), na.rm=na.rm)
  names(datac)[ names(datac) ==paste(measurevar, ".mean",    sep="") ] <- measurevar # Rename columns
  names(datac)[ names(datac) ==paste(measurevar, ".sd",      sep="") ] <-"sd"
  names(datac)[ names(datac) ==paste(measurevar, ".length2", sep="") ] <- "N"
  datac$se <- datac$sd /sqrt(datac$N)  # Calculate standarderror of the mean
  ciMult <- qt(conf.interval/2 + .5,datac$N-1) # Confidence interval multiplier forstandard error # Calculate t-statistic for confidenceinterval: # e.g., if conf.interval is .95, use .975(above/below), and use df=N-1
  datac$ci <- datac$se * ciMult
  return(datac)
}
#for sigmaplot summaries
respvars<-c('Mvabund','total','MVrelabund','soilmoi','nhdi','nodi','nh.no','nitrifd','minzd')
respvarsP<-c('Mvabund','total','MVrelabund','pavi','soilmoi','nhdi','nodi','nh.no','nitrifd','minzd')
respvarsS<-c('Mvabund','total','MVrelabund','sobi','soilmoi','nhdi','nodi','nh.no','nitrifd','minzd')
respvarsMonos<-c('Mvabund','total','MVrelabund','pavi','sobi','soilmoi','nhdi','nodi','nh.no','nitrifd','minzd')

##################################


##################################
#make datasets
# dataset1: No competition treatment, all Mvtrt levels
Empty<-data[data$type=='Empty',]
Mivi<-data[data$type=='Mivi',]
CompEmpty<-data[data$type=='CompEmpty',]
dataset1<-rbind(Empty, CompEmpty, Mivi)
colnames(dataset1)
write.table(dataset1,file='dataset1.txt', sep='\t', row.names=F)
#summarize for sigmaplot  = 'fullsummary1.txt'
fullsummary<-numeric(0)
for (i in (1:length(respvars))){
  mat<-summarySE(dataset1, measurevar=respvars[i],groupvars='Mvtrt')  
  means<-mat[,3]
  ses<-mat[,5]
  respvar.cols<-cbind(means,ses)
  colnames(respvar.cols)<-paste(respvars[i],c('means','ses'), sep='_')
  fullsummary<-cbind(fullsummary,respvar.cols) 
}     
fullsummary<-cbind(mat$Mvtrt,fullsummary)
colnames(fullsummary)[1]<-'Mvtrt'
write.table(fullsummary, file='fullsummary1.txt', sep='\t', row.names=F)

############################################################
############################################################
# dataset2: Competition treatment, all Mvtrt levels
CompPavi<-data[data$type=='CompPavi',]
Pavi<-data[data$type=='Pavi',]
CompSobi<-data[data$type=='CompSobi',]
Sobi<-data[data$type=='Sobi',]
dataset2<-rbind(Mivi, CompPavi, Pavi, CompSobi, Sobi)
P2<-rbind(Mivi, CompPavi, Pavi)
S2<-rbind(Mivi, CompSobi, Sobi)
write.table(dataset2,file='dataset2.txt', sep='\t', row.names=F)
#summarize for sigmaplot - P = 'fullsummaryP2.txt'
fullsummary<-numeric(0)
for (i in (1:length(respvarsP))){
  mat<-summarySE(P2, measurevar=respvarsP[i],groupvars='Mvtrt')  
  means<-mat[,3]
  ses<-mat[,5]
  respvar.cols<-cbind(means,ses)
  colnames(respvar.cols)<-paste(respvarsP[i],c('means','ses'), sep='_')
  fullsummary<-cbind(fullsummary,respvar.cols) 
}     
fullsummary<-cbind(mat$Mvtrt,fullsummary)
colnames(fullsummary)[1]<-'Mvtrt'
write.table(fullsummary, file='fullsummaryP2.txt', sep='\t', row.names=F)
#summarize for sigmaplot - S = 'fullsummaryS2.txt'
fullsummary<-numeric(0)
for (i in (1:length(respvarsS))){
  mat<-summarySE(S2, measurevar=respvarsS[i],groupvars='Mvtrt')  
  means<-mat[,3]
  ses<-mat[,5]
  respvar.cols<-cbind(means,ses)
  colnames(respvar.cols)<-paste(respvarsS[i],c('means','ses'), sep='_')
  fullsummary<-cbind(fullsummary,respvar.cols) 
}     
fullsummary<-cbind(mat$Mvtrt,fullsummary)
colnames(fullsummary)[1]<-'Mvtrt'
write.table(fullsummary, file='fullsummaryS2.txt', sep='\t', row.names=F)

############################################################
############################################################
# dataset1b: No competition treatment, middle Mvtrt levels
dataset1b<-rbind(CompEmpty)
write.table(dataset1b,file='dataset1b.txt', sep='\t', row.names=F)
#summarize for sigmaplot = 'fullsummary1b.txt'
fullsummary<-numeric(0)
for (i in (1:length(respvars))){
  mat<-summarySE(dataset1b, measurevar=respvars[i],groupvars='Mvtrt')  
  means<-mat[,3]
  ses<-mat[,5]
  respvar.cols<-cbind(means,ses)
  colnames(respvar.cols)<-paste(respvars[i],c('means','ses'), sep='_')
  fullsummary<-cbind(fullsummary,respvar.cols) 
}     
fullsummary<-cbind(mat$Mvtrt,fullsummary)
colnames(fullsummary)[1]<-'Mvtrt'
write.table(fullsummary, file='fullsummary1b.txt', sep='\t', row.names=F)

############################################################
############################################################
# dataset2b: Competition treatment, middle Mvtrt levels
dataset2b<-rbind(CompPavi, CompSobi)
P2b<-CompPavi
S2b<-CompSobi
write.table(dataset2b,file='dataset2b.txt', sep='\t', row.names=F)
#summarize for sigmaplot - P = 'fullsummaryP2b.txt'
fullsummary<-numeric(0)
for (i in (1:length(respvarsP))){
  mat<-summarySE(P2b, measurevar=respvarsP[i],groupvars='Mvtrt')  
  means<-mat[,3]
  ses<-mat[,5]
  respvar.cols<-cbind(means,ses)
  colnames(respvar.cols)<-paste(respvarsP[i],c('means','ses'), sep='_')
  fullsummary<-cbind(fullsummary,respvar.cols) 
}     
fullsummary<-cbind(mat$Mvtrt,fullsummary)
colnames(fullsummary)[1]<-'Mvtrt'
write.table(fullsummary, file='fullsummaryP2b.txt', sep='\t', row.names=F)
#summarize for sigmaplot - S = 'fullsummaryS2b.txt'
fullsummary<-numeric(0)
for (i in (1:length(respvarsS))){
  mat<-summarySE(S2b, measurevar=respvarsS[i],groupvars='Mvtrt')  
  means<-mat[,3]
  ses<-mat[,5]
  respvar.cols<-cbind(means,ses)
  colnames(respvar.cols)<-paste(respvarsS[i],c('means','ses'), sep='_')
  fullsummary<-cbind(fullsummary,respvar.cols) 
}     
fullsummary<-cbind(mat$Mvtrt,fullsummary)
colnames(fullsummary)[1]<-'Mvtrt'
write.table(fullsummary, file='fullsummaryS2b.txt', sep='\t', row.names=F)

############################################################
############################################################
# dataset3: Monocultures
dataset3<-rbind(Empty,Sobi,Pavi,Mivi)
write.table(dataset3,file='dataset3.txt', sep='\t', row.names=F)
#summarize for sigmaplot = 'fullsummary3.txt'
fullsummary<-numeric(0)
for (i in (1:length(respvarsMonos))){
  mat<-summarySE(dataset3, measurevar=respvarsMonos[i],groupvars='type')  
  means<-mat[,3]
  ses<-mat[,5]
  respvar.cols<-cbind(means,ses)
  colnames(respvar.cols)<-paste(respvarsMonos[i],c('means','ses'), sep='_')
  fullsummary<-cbind(fullsummary,respvar.cols) 
}     
fullsummary<-cbind(as.character(mat$type),fullsummary)
colnames(fullsummary)[1]<-'type'
write.table(fullsummary, file='fullsummary3.txt', sep='\t', row.names=F)

