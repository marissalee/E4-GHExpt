#e4.R
# 5/9/13

#OVERALL SETUP
##############
#wd
setwd("~/Desktop/E4_R")

#inputs
#1. e4_trimmedData.txt

#products
#Analysis of unplanted and monoculture pots
#done 1. summary table (aggregated by plant): mean, se, n = 'summary_monos.txt'
#done 2. figure 2: resps v plant = 'fig2_monos.png'
#done 3. lme4 model: resp<-plant+(1|bk) and test of fixed effects = 'termeffs_monos.txt'
#Analysis of mixed community pots
#done 1. summary tables (aggregated by (1) mvtrt, (2) comptrt, and (3) factorial trts): mean, se, n = 'summary_mixedcomm.txt'
#done 2. plot parameters to determine if correlated: logtotal/mvtrt, logtotal/comptrt = 'param_correl.png'
#done 3. figure 3: resps v mvtrt, resps v logtotal = 'fig3_mixedcomm.png'
#done 4a. lme4 model: resp<-mvtrt+comptrt+mvtrt*comptrt+(1|bk) and test of fixed effects = 'termeffs1_mixedcomm.txt'
#done 4b. lme4 model: resp<-mvtrt+logtotal+mvtrt*logtotal+(1|bk) and test of fixed effects ='termeffs2_mixedcomm.txt'
#Analysis of Mvtrt on measured Mv abundance
#done 1. plot of mvabund v mvtrt, mvrelabund v mvtrt
#done 2. lme4 models: mvabund/relmvabund<-mvtrt+(1|bk) and test of fixed effects = 'termeffs_abund_mixedcomm.txt'


#libraries
library(doBy)
library(lme4)
#library(coefplot2)# this no longer exists for R version 3.1.1
library(coefplot)# hopefully this has the same functionality...
require(data.table)

#import data
data<-read.table('e4_trimmedData.txt',header=T)
#modify...
data$total<-data$Mvabund + data$pavi + data$sobi
data$bk<-as.factor(data$bk)
data$compabund<-data$pavi + data$sobi
#subset...
Empty<-data[data$type=='Empty',]
Mivi<-data[data$type=='Mivi',]
Pavi<-data[data$type=='Pavi',]
Sobi<-data[data$type=='Sobi',]
CompEmpty<-data[data$type=='CompEmpty',]
CompPavi<-data[data$type=='CompPavi',]
CompSobi<-data[data$type=='CompSobi',]
###########################################
###########################################

###########################################
#UNPLANTED AND MONOCULTURE POTS
#SETUP
###########################################
datam<-rbind(Empty, Pavi, Sobi, Mivi)
#make a column for monoculture/unplanted categories
datam$plant<-rep(NA,length(datam$type)) #key: 1= Unplanted, 2=M.v., 3= P.v., 4=S.b.
datam[which(datam$type=='Empty'),19]<-1 
datam[which(datam$type=='Mivi'),19]<-2
datam[which(datam$type=='Pavi'),19]<-3
datam[which(datam$type=='Sobi'),19]<-4
#xs
plant<-datam$plant; plantlabels<-c('Unplanted','M.v.','P.v.','S.b.')
bk<-datam$bk
#ys
soilmoi<-datam$soilmoi
nodi<-datam$nodi
nitrifd<-datam$nitrifd
#other
total<-datam$total
tmp<-total; hist(tmp); shapiro.test(tmp) #not normal, but better than log-transformed
compabund<-data$compabund

datamf<-data.frame(total, soilmoi, nodi, nitrifd, plant, bk)
#write.table(datamf, file='datamf.txt', sep='\t', row.names=F)

#done 1. summary table (aggregated by plant): mean, se, n = 'summary_monos.txt'
#done 2. figure 2: resps v plant = 'fig2_monos.png'
#done 3. lme4 model: resp<-plant+(1|bk) and test of fixed effects = 'termeffs_monos.txt'
######
#Parts 1 and 2 - Summarize and Plot
#FIG.1
######
par(mfrow = c(2, 2)) #number of panels
par(cex = 0.8) #font size
par(mar = c(0, 4, 0, 0), oma = c(8, 5, 0.5, 0.5)) #size of inner, outer margins 
par(tcl = -0.25) #length of tick marks
par(mgp = c(2, 0.6, 0))

xlims <- c(0.5,4.5)
resps<-datamf[,c(1:4)]
#View(resps)
respnames<-c('Total dry aboveground biomass (g)','Soil moisture (%)','Nitrate (ugN/G)','Net Nitrification (ugN/G*d)')
shortnames<-c('total','soilmoi','nodi','nitrifd')
resplabels<-c('Total dry aboveground biomass (g)','Soil moisture (%)',
              expression(paste("Nitrate (",mu,"gN/G)")),
              expression(paste("Nitrification (",mu,"gN/G"%*%"d)"))) #this needs to go directly into the plotting fxn?

yl1<-c(0,100) #total
yl2<-c(55,90) #soilmoi
yl3<-c(0,110) #nodi
yl4<-c(-4,4) #nitrifd
ylims<-cbind(yl1,yl2,yl3,yl4)
yat1<-seq.int(0,100, by=10) #total
yat2<-seq.int(55,90, by=5) #soilmoi
yat3<-seq.int(0,110, by=10) #nodi
yat4<-seq.int(-4,4, by=2) #nitrifd
yat<-list(yat1,yat2,yat3,yat4)
tuk1<-c('a','b','b','c') #total
tuk2<-c('a','a','a','b') #soilmoi
tuk3<-c('a','b','bc','c') #nodi
tuk4<-c('','','','') #nitrifd
tukletters<-cbind(tuk1,tuk2,tuk3,tuk4)
tukletters<-rbind(tukletters[1,],tukletters[3,],tukletters[4,],tukletters[2,]) #reorganize so that this matches 'plantvec'

sums<-list()
models<-list()
modelfits<-list()
termeffs<-numeric(0)
lm.pvals<-list()
tukys<-list()

#change the order of the panels
panelseq<-c(1,3,2,4)
for (i in panelseq){
  
	resp<-resps[,i]
  data<-data.frame(resp,plant,bk)
  #Part 1 - summarize
  dat<-data.table(data)
  sum<-dat[,list(mean=mean(resp), se=sd(resp)/sqrt(length(resp)), n=length(resp)),by=plant]
  plantvec<-sum$plant
  mean<-sum$mean
  se<-sum$se
	sum1<-orderBy(~plant,sum)
	sum1$plant<-plantlabels
	sums[[i]]<-sum1
  #Part 2 - lme model
	trt<-as.factor(plant)
  data1<-data.frame(resp, trt,bk)
	model <- lmer(resp ~ trt+(1|bk), data=data1)
  modela<- lmer(resp ~ 1+(1|bk), data=data1)
	
  #NEED TO CHECK THIS OUT... some changes have been made to lmer, so this fxn doesn't work now
	#logLik<-summary(model)@logLik
  #logLikrow<-c(shortnames[i],NA,NA,logLik)
	#REmat<-summary(model)@REmat
	#coefs<-summary(model)@coefs
  #coefmat<-cbind(rownames(coefs),coefs)
  #modelfit<-rbind(logLikrow,REmat,coefmat)
  
  #modelfits[[i]]<-modelfit
  #models[[i]]<-model

  termeff<-anova(modela,model)
	delAIC<-termeff[2,2]-termeff[1,2]
	Chisq<-termeff[2,5]
	ChisqDF<-termeff[2,6]
	pval<-termeff[2,7]
	termeffrow<-c(delAIC,Chisq,ChisqDF,pval)
  termeffs<-rbind(termeffs,termeffrow)
  
	#Part 2b - lm model and post-hoc tests
	lm <- lm(resp ~ trt, data=data1)
	lmfit<-aov(lm)
	lm.pval<-anova(lmfit)$'Pr(>F)'[1]
	lm.pvals[[i]]<-lm.pval
	tuky<-TukeyHSD(lmfit)$trt
	respname<-rep(respnames[i],dim(tuky)[1])
	mat<-cbind(respname,tuky)
	tukys[[i]]<-mat
  
  #Part 3 - Plot
  plot(resp~plant, data, axes = FALSE, type = "n", 
       xlab='', ylab='',
         xlim=xlims,
         ylim=ylims[,i])
  #add points
	#points(y=resp,x=jitter(data$plant, .5), pch=16)   	#data
	points(y=mean,x=plantvec+.1, pch=16)				#plant means
	text(y=mean,x=plantvec-.1, labels=tukletters[,i]) #tukys
  arrows(plantvec+.1,mean+se, 						#error bars
	       plantvec+.1,mean-se,
	       angle=90, code=3, length=0, col=1) 
  #add panel letters and boxes
  mtext(letters[i], side = 3, line = -1.5, adj = 0.05, cex = 1, font=2) 
	box()
  #x-axes
  if (i %in% c(2, 4)) 
  { axis(1, at = 1:4, labels=F)
    labels <- c('Unplanted',expression(italic("Microstegium")),expression(italic("Panicum")),expression(italic("Sorghum"))) 
    #will need to paste these labels in later bc italics don't work...
    text(1:4, par("usr")[3] - 0.25, srt = 45, adj = c(1,1),
         labels = labels, xpd = TRUE)
    mtext("Plant species", side = 1, outer = F, cex = 1, adj= 0.55, line=5)
  }
  #y-axes
  axis(2, at = yat[[i]])
	mtext(resplabels[i], side = 2, outer = F, cex = 1, line=2.2)
  
}
#export 4-panel figure = 'fig2_monos.png'
#export sums table
names(sums)<-shortnames
sumsdf<-data.frame(sums)
#write.table(sumsdf, file='summary_monos.txt', sep='\t', row.names=F)

#export lme model summary, nested anova results
#THIS DOESN'T WORK BECAUSE OF LMER FXN UPDATE, SEE ABOVE
modelfits<-data.frame(modelfits)
#write.table(modelfits, file='modelfits_monos.txt', sep='\t', row.names=T, col.names=T)
colnames(termeffs)<-c('delAIC','Chisq','ChisqDF','pval')
rownames(termeffs)<-shortnames
#write.table(termeffs, file='termeffs_monos.txt', sep='\t', row.names=T, col.names=T)

#export lm model summary, tukey results
names(lm.pvals)<-shortnames
lm.pvalsdf<-data.frame(lm.pvals)
row.names(lm.pvalsdf)<-'lm.pval'
names(tukys)<-shortnames
tukysdf<-data.frame(tukys)
#write.table(tukysdf, file='tukys_monos.txt', sep='\t', row.names=T, col.names=T)
#######
#FOR AMMONIUM / TOTAL INORGANIC N / AMMONIFICATION / MINERALIZATION
#ESM figure
#######
par(mfrow = c(2, 2)) #number of panels
par(cex = 0.8) #font size
par(mar = c(0, 4, 0, 0), oma = c(8, 5, 0.5, 0.5)) #size of inner, outer margins 
par(tcl = -0.25) #length of tick marks
par(mgp = c(2, 0.6, 0))

xlims <- c(0.5,4.5)
colnames(datam)[c(9,12,13,15)]
resps<-datam[,c(9,12,13,15)]
#View(resps)
respnames<-c('Ammonium (ugN/G)',
             'Total Inorg. N (ugN/G)',
             'Ammonification (ugN/G*d)',
             'Mineralization (ugN/G*d)')
shortnames<-c('nhdi','totdi','ammonifd','minzd')

yname1<-expression(paste("Ammonium (",mu,"gN/G)"))
yname2<-expression(paste("Total Inorg. N (",mu,"gN/G)"))
yname3<-expression(paste("Ammonification (",mu,"gN/G"%*%"d)"))
yname4<-expression(paste("Mineralization (",mu,"gN/G"%*%"d)"))  
resplabels<-c(yname1,yname2,yname3,yname4)

yl1<-c(0,110) #nhdi
yl2<-c(0,110) #totdi
yl3<-c(-4,4) #ammonifd
yl4<-c(-4,4) #minzd
ylims<-cbind(yl1,yl2,yl3,yl4)
yat1<-seq.int(0,120, by=20) #nhdi
yat2<-seq.int(0,120, by=20) #totdi
yat3<-seq.int(-5,5, by=2) #ammonifd
yat4<-seq.int(-5,5, by=2) #minzd
yat<-list(yat1,yat2,yat3,yat4)
tuk1<-c('a','ab','ab','b') #nhdi
tuk2<-c('a','b','bc','c') #totdi
tuk3<-c('','','','') #ammonifd
tuk4<-c('','','','') #minzd
tukletters<-cbind(tuk1,tuk2,tuk3,tuk4)
tukletters<-rbind(tukletters[1,],tukletters[3,],tukletters[4,],tukletters[2,]) #reorganize so that this matches 'plantvec'

sums<-list()
models<-list()
modelfits<-list()
termeffs<-numeric(0)
lm.pvals<-list()
tukys<-list()

#change the order of the panels
panelseq<-c(1,2,3,4)
for (i in panelseq){
  resp<-resps[,i]
  data<-data.frame(resp,plant,bk)
  #Part 1 - summarize
  dat<-data.table(data)
  sum<-dat[,list(mean=mean(resp), se=sd(resp)/sqrt(length(resp)), n=length(resp)),by=plant]
  plantvec<-sum$plant
  mean<-sum$mean
  se<-sum$se
  sum1<-orderBy(~plant,sum)
  sum1$plant<-plantlabels
  sums[[i]]<-sum1
  #Part 2 - lme model
  trt<-as.factor(plant)
  data1<-data.frame(resp, trt,bk)
  model <- lmer(resp ~ trt+(1|bk), data=data1)
  modela<- lmer(resp ~ 1+(1|bk), data=data1)
  
  #NEED TO CHECK THIS OUT... some changes have been made to lmer, so this fxn doesn't work now
  #logLik<-summary(model)@logLik
  #logLikrow<-c(shortnames[i],NA,NA,logLik)
  #REmat<-summary(model)@REmat
  #coefs<-summary(model)@coefs
  #coefmat<-cbind(rownames(coefs),coefs)
  #modelfit<-rbind(logLikrow,REmat,coefmat)
  
  #modelfits[[i]]<-modelfit
  #models[[i]]<-model
  
  termeff<-anova(modela,model)
  delAIC<-termeff[2,2]-termeff[1,2]
  Chisq<-termeff[2,5]
  ChisqDF<-termeff[2,6]
  pval<-termeff[2,7]
  termeffrow<-c(delAIC,Chisq,ChisqDF,pval)
  termeffs<-rbind(termeffs,termeffrow)
  
  #Part 2b - lm model and post-hoc tests
  lm <- lm(resp ~ trt, data=data1)
  lmfit<-aov(lm)
  lm.pval<-anova(lmfit)$'Pr(>F)'[1]
  lm.pvals[[i]]<-lm.pval
  tuky<-TukeyHSD(lmfit)$trt
  respname<-rep(respnames[i],dim(tuky)[1])
  mat<-cbind(respname,tuky)
  tukys[[i]]<-mat
  
  #Part 3 - Plot
  plot(resp~plant, data, axes = FALSE, type = "n", 
       xlab='', ylab='',
       xlim=xlims,
       ylim=ylims[,i])
  #add points
  #points(y=resp,x=jitter(data$plant, .5), pch=16)   	#data
  points(y=mean,x=plantvec+.1, pch=16)				#plant means
  text(y=mean,x=plantvec-.1, labels=tukletters[,i]) #tukys
  arrows(plantvec+.1,mean+se, 						#error bars
         plantvec+.1,mean-se,
         angle=90, code=3, length=0, col=1) 
  #add panel letters and boxes
  mtext(letters[i], side = 3, line = -1.5, adj = 0.05, cex = 1, font=2) 
  box()
  #x-axes
  if (i %in% c(3, 4)) 
  { axis(1, at = 1:4, labels=FALSE)
    labels <- c('Unplanted',expression(italic(Microstegium)),expression(italic(Panicum)),expression(italic(Sorghum)))
    text(1:4, par("usr")[3] - 0.25, srt = 45, adj = c(1,1),
         labels = labels, xpd = TRUE)
    mtext("Plant species", side = 1, outer = F, cex = 1, adj= 0.55, line=5)
  }
  #y-axes
  axis(2, at = yat[[i]])
  mtext(resplabels[i], side = 2, outer = F, cex = 1, line=2.2)
  
}
#export 4-panel figure = 'fig2_monos.png'
#export sums table
names(sums)<-shortnames
sumsdf<-data.frame(sums)
#write.table(sumsdf, file='summary_monos.txt', sep='\t', row.names=F)

#export lme model summary, nested anova results
#THIS DOESN'T WORK BECAUSE OF LMER FXN UPDATE, SEE ABOVE
#modelfits<-data.frame(modelfits)
#write.table(modelfits, file='modelfits_monos.txt', sep='\t', row.names=T, col.names=T)
#colnames(termeffs)<-c('delAIC','Chisq','ChisqDF','pval')
#rownames(termeffs)<-shortnames
#write.table(termeffs, file='termeffs_monos.txt', sep='\t', row.names=T, col.names=T)

#export lm model summary, tukey results
names(lm.pvals)<-shortnames
lm.pvalsdf<-data.frame(lm.pvals)
row.names(lm.pvalsdf)<-'lm.pval'
names(tukys)<-shortnames
tukysdf<-data.frame(tukys)
#write.table(tukysdf, file='tukys_monos.txt', sep='\t', row.names=T, col.names=T)
#######


###########################################
###########################################


###########################################
#MIXED COMMUNITY POTS
#SETUP
#######
dataa<-rbind(CompEmpty, CompPavi, CompSobi)
dataa$logtotal<-log(dataa$total)
#make a column for trt (mvtrt x comptrt) 
dataa$trt<-rep(NA,length(dataa$comptrt))
for (k in 1:dim(dataa)[1]){ #loop through each row in dataa
  mvname<-dataa[k,4] #make a character string out of mvtrt and comptrt
  compname<-dataa[k,3]
  trtname<-paste(mvname,compname, sep='_')
  dataa[k,20]<-trtname #put that character string into the trt col
}
#xs
logtotal<-dataa$logtotal
mvtrt<-dataa$Mvtrt 
comptrt<-dataa$comptrt
trt<-dataa$trt
bk<-dataa$bk
#make a mvtrt x comptrt column
#ys
soilmoi<-dataa$soilmoi 
nodi<-dataa$nodi
nhno<-dataa$nh.no
nitrifd<-dataa$nitrifd
minzd<-dataa$minzd
#other vars
abund<-dataa$Mvabund
relabund<-dataa$MVrelabund
compabund<-dataa$compabund
nhdi<-dataa$nhdi
totdi<-dataa$totdi
ammonifd<-dataa$ammonifd
total<-dataa$total

dataaf<-data.frame(soilmoi, nodi, nitrifd, mvtrt,comptrt,trt,logtotal,bk,abund,relabund, compabund, total)
databf<-data.frame(nhdi, nodi, totdi,mvtrt,comptrt,trt,logtotal,bk,abund,relabund, compabund)
datacf<-data.frame(ammonifd, nitrifd, minzd,mvtrt,comptrt,trt,logtotal,bk,abund,relabund, compabund)

#1. summary tables (aggregated by (1) mvtrt, (2) comptrt, and (3) factorial trts): mean, se, n
#2. plot parameters to determine if correlated: logtotal/mvtrt, logtotal/comptrt
#3. figure 3: resps v mvtrt, resps v logtotal
#4a. lme4 model: resp<-mvtrt+comptrt+mvtrt*comptrt+(1|bk) and test of fixed effects
#4b. lme4 model: resp<-mvtrt+logtotal+mvtrt*logtotal+(1|bk) and test of fixed effects
#######
#Part 1 - Summarize data
#######
resps<-dataaf[,c(12,1:3)]
respnames<-c('Total dry aboveground biomass (g)','Soil moisture (%)','Nitrate (ugN/G)','Net Nitrification (ugN/G*d)')
shortnames<-c('total','soilmoi','nodi','nitrifd')

sums.mvtrt<-list()
sums.comptrt<-list()
sums.trt<-list()

lmfits<-list()
tukys<-list()

for (i in 1:dim(resps)[2]){
  resp<-resps[,i]
  data<-data.frame(resp,mvtrt,comptrt,trt,logtotal,bk)
  #Part 1 - Summarize data
  dat<-data.table(data)
  sum.mvtrt<-dat[,list(mean=mean(resp), se=sd(resp)/sqrt(length(resp)), n=length(resp)),by=mvtrt]
  sum.comptrt<-dat[,list(mean=mean(resp), se=sd(resp)/sqrt(length(resp)), n=length(resp)),by=comptrt]
  sum.trt<-dat[,list(mean=mean(resp), se=sd(resp)/sqrt(length(resp)), n=length(resp)),by=trt]
  sum.mvtrt1<-orderBy(~mvtrt,sum.mvtrt)
  sum.comptrt1<-orderBy(~comptrt,sum.comptrt)
  sum.trt1<-orderBy(~trt,sum.trt)
  sums.mvtrt[[i]]<-sum.mvtrt1
  sums.comptrt[[i]]<-sum.comptrt1
  sums.trt[[i]]<-sum.trt1
  
  #Part2 - lm model and tukey posthoc tests
  lmfit <- lm(resp ~ mvtrt + comptrt + mvtrt:comptrt, data=data)
  lmfits[[i]]<-anova(lmfit)

  lm.mvtrt <- lm(resp ~ as.factor(mvtrt), data=data)
  lmfit.mvtrt<-aov(lm.mvtrt)
  tuky.mvtrt<-TukeyHSD(lmfit.mvtrt)$`as.factor(mvtrt)`
  
  lm.comptrt <- lm(resp ~ comptrt, data=data)
  lmfit.comptrt<-aov(lm.comptrt)
  tuky.comptrt<-TukeyHSD(lmfit.comptrt)$comptrt
  
  lm.trt <- lm(resp ~ trt, data=data)
  lmfit.trt<-aov(lm.trt)
  tuky.trt<-TukeyHSD(lmfit.trt)$trt
  
  respname<-rep(respnames[i],(dim(tuky.mvtrt)[1]+dim(tuky.comptrt)[1]+dim(tuky.trt)[1]))
  contrast<-c(rownames(tuky.mvtrt),rownames(tuky.comptrt),rownames(tuky.trt))
  tuky<-rbind(tuky.mvtrt,tuky.comptrt,tuky.trt)
  mat<-cbind(respname,contrast,tuky)
  tukys[[i]]<-mat
  
}
#process sums tables
sumstables<-list(sums.mvtrt,sums.comptrt,sums.trt)
sumstables2<-list()
comparison<-c('mvtrt','comptrt','trt')
for (i in 1:length(sumstables)){
  names(sumstables[[i]])<-shortnames
  df<-data.frame(sumstables[[i]])
  df1<-df[,-c(5,9,13)]
  colnames(df1)[1]<-'level'
  trt<-rep(comparison[i],dim(df1)[1])
  df2<-cbind(trt,df1)
  sumstables2[[i]]<-df2
}
#export sums table
sumsdf1<-data.frame(sumstables2[[1]])
sumsdf2<-data.frame(sumstables2[[2]])
sumsdf3<-data.frame(sumstables2[[3]])
sumsdf<-rbind(sumsdf1,sumsdf2,sumsdf3)

#process tukys table
lmfits
tukystab<-numeric(0)
for (i in 1:length(tukys)){
  df<-data.frame(tukys[[i]])
  tukystab<-rbind(tukystab,df)
}

#write.table(sumsdf, file='summary_mixedcomm.txt', sep='\t', row.names=F)
#write.table(tukystab, file='tukys_mixedcomm.txt', sep='\t', row.names=F)
#######
#Part 2 - plot parameters to determine if correlated: logtotal/mvtrt, logtotal/comptrt
#ESM Figure
#######
data<-dataaf
logtotal<-dataaf$logtotal
comptrt<-dataaf$comptrt
mvtrt<-dataaf$mvtrt
dat<-data.table(data)

par(mfrow = c(1, 2)) #number of panels
par(cex = 0.8) #font size
par(mar = c(4, 0, 0, 0), oma = c(5, 5, 0.5, 0.5)) #size of inner, outer margins 
par(tcl = -0.25) #length of tick marks
par(mgp = c(2, 0.6, 0))
#logtotal v mvtrt
var(logtotal, comptrt) 
sum<-dat[,list(mean=mean(logtotal), se=sd(logtotal)/sqrt(length(logtotal))),by=mvtrt]
vec<-sum$mvtrt
mean<-sum$mean
se<-sum$se
ylimm<-c(2.8,4.5)
tuka<-c('a','a','a','a') #mvtrt
tukb<-c('a','b','c') #comtrt

plot(logtotal~mvtrt, axes = FALSE, type = "n", ylim=ylimm, xlim=c(.5,5.5), xlab='')
#points(y=logtotal,x=jitter(mvtrt,.5), pch=16) #data
points(y=mean,x=vec+.1, pch=16) # mvtrt means
#text(y=mean,x=vec-.1, labels=tuka) #tukys
arrows(vec+.1,mean+se,   					#error bars
       vec+.1,mean-se,
       angle=90, code=3, length=0, col=1) 
axis(1, at = 1:5)
axis(2)
mtext(letters[1], side = 3, line = -1.5, adj = 0.05, cex = 1, font=2) 
box()
mtext(expression(paste(italic(Microstegium)," density")), side = 1, outer = F, cex = 1, line=3.2)
mtext("treatment (# individuals)", side = 1, outer = F, cex = 1, line=4.2)



#logtotal v comptrt
var(logtotal, comptrt) #consider using 'mutual information' to quantify this
sum<-dat[,list(mean=mean(logtotal), se=sd(logtotal)/sqrt(length(logtotal))),by=comptrt]
vec<-sum$comptrt
mean<-sum$mean
se<-sum$se
plot(logtotal~mvtrt,axes = FALSE, type = "n", ylim=ylimm, xlim=c(.5,3.5), xlab='')
#points(y=logtotal,x=jitter(as.numeric(comptrt), .5), pch=16) #data
points(y=mean,x=as.numeric(vec)+.1, pch=16) #plant means
text(y=mean,x=as.numeric(vec)-.1, labels=tukb) #tukys
arrows(as.numeric(vec)+.1,mean+se,     				#error bars
       as.numeric(vec)+.1,mean-se,
       angle=90, code=3, length=0, col=1) 
axis(1, at = 1:3, label=c('No neighbor',
                          expression(italic(Panicum)),
                          expression(italic(Sorghum))
                          )
     )
mtext(letters[2], side = 3, line = -1.5, adj = 0.05, cex = 1, font=2) 
box()
mtext('Log-transformed \n total dry aboveground biomass', side = 2, outer = T, cex = 1, line=2.2, adj=0.7)
mtext("Neighbor treatment", side = 1, outer = F, cex = 1, line=3.2)
#export 2-panel figure - param_correl.png
#######
#Parts 3 and 4 - Make figure 3 and eval model
#FIG.2
#######
par(mfrow = c(3, 2)) #number of panels
par(cex = 0.8) #font size
par(mar = c(0, 0, 0, 0), oma = c(5, 5, 0.5, 8)) #size of inner, outer margins 
par(tcl = -0.25) #length of tick marks
par(mgp = c(2, 0.6, 0))

xlim1 <- c(0.5,5.5)
xlim2 <- c(1.9,5.1)
resps<-dataaf[,c(1:3)]
respnames<-c('Soil moisture (%)','Nitrate (ugN/G)','Nitrification\n(ugN/G*d)')
shortnames<-c('soilmoi','nodi','nitrifd')
yname1<-'Soil moisture (%)'
yname2<-expression(paste("Nitrate (",mu,"gN/G)"))
yname3<-expression(paste("Nitrification (",mu,"gN/G"%*%"d)"))  
resplabels<-c(yname1,yname2,yname3)
yl2<-c(50,105) #soilmoi
yl3<-c(0,115) #nodi
yl4<-c(-5,17) #nitrifd
ylims<-cbind(yl2,yl3,yl4)
cols<-c(1,'darkgray','darkgray')
pchs<-c(16,17,15)
pchs2<-c(1,2,0)
linecols<-c(1,1,'darkgray')
ltys<-c(1,2,2)
termeffs1<-numeric(0)
termeffs2<-numeric(0)
models<-list()
models2<-list()
pl1<-c('a','d')
pl2<-c('b','e')
pl3<-c('c','f')
pl<-cbind(pl1,pl2,pl3)
panelad<-c('mvtrt  ns\nneigtrt ***\ninteraction  **',
           'mvtrt  ns\nlogtotal ***\ninteraction  ns\nlogtotal^2 ***')
panelbe<-c('mvtrt  ns\ncomptrt ***\ninteraction  ns',
           'mvtrt  ns\nlogtotal ***\ninteraction  ns\nlogtotal^2  ns')
panelcf<-c('mvtrt  ns\nneigtrt   *\ninteraction  ns',
           'mvtrt  ns\nlogtotal  ns\ninteraction  ns\nlogtotal^2  ns')
panellabs<-cbind(panelad,panelbe,panelcf)

for (i in 1:dim(resps)[2]){
  resp<-resps[,i]
  data<-data.frame(resp,mvtrt,comptrt,logtotal,bk)
  data1N<-subset(data, comptrt=='N')
  data1P<-subset(data, comptrt=='P')
  data1S<-subset(data, comptrt=='S')
  
  #Model1
  model1<-lmer(resp ~ mvtrt + comptrt + mvtrt:comptrt+(1|bk), data=data)
  models[[i]]<-model1
  model1a<-lmer(resp ~ mvtrt + comptrt +(1|bk), data=data)
  model1b<-lmer(resp ~ mvtrt +(1|bk), data=data)
  model1c<-lmer(resp ~ 1 +(1|bk), data=data)
  term3<-anova(model1a,model1)
  term2<-anova(model1b,model1a)
  term1<-anova(model1c,model1b)
  terms<-list(term1,term2,term3)
  for(k in 1:length(terms)){
    termeff<-terms[[k]]
    delAIC<-termeff[2,2]-termeff[1,2]
    Chisq<-termeff[2,5]
    ChisqDF<-termeff[2,6]
    pval<-termeff[2,7]
    respname<-paste(respnames[i])
    termname<-paste('term',k)
    termeffrow<-c(respname,termname,delAIC,Chisq,ChisqDF,pval)
    termeffs1<-rbind(termeffs1,termeffrow)
  }
  
  #Plot Model1
  cs1 <- fixef(model1) 
  re1 <- ranef(model1)
  plot(resp ~ mvtrt, data, type='n',axes = FALSE,
       ylim=ylims[,i], 
       ylab=respnames[i], 
       xlim=xlim1,
       xlab='M.v. density treatment \n (# M.v. inds)')
  points(y=data1N[,1], x=jitter(data1N$mvtrt), col=cols[1], pch=pchs[1], cex=.8)
  points(y=data1P[,1], x=jitter(data1P$mvtrt), col=cols[2], pch=pchs[2], cex=.8)
  points(y=data1S[,1], x=jitter(data1S$mvtrt), col=cols[3], pch=pchs[3], cex=.8)
  curve(cs1[1] + cs1[2]*x, min(mvtrt), max(mvtrt), add=T, col=linecols[1], lty=ltys[1]) #group mean
  curve(cs1[1] + (cs1[2]+cs1[5])*x + cs1[3], min(mvtrt), max(mvtrt), add=T, col=linecols[2],lty=ltys[2]) #group mean
  curve(cs1[1] + (cs1[2]+cs1[6])*x + cs1[4], min(mvtrt), max(mvtrt), add=T, col=linecols[3],lty=ltys[3]) #group mean
  #legend('topright', legend=c('Model1: None','Model1: P.v.','Model1: S.b.'), lty=ltys, col=cols, , pch=pchs, cex=.7)
  mtext(pl[1,i], side = 3, line = -1.5, adj = 0.05, cex = 1, font=2) 
  mtext(panellabs[1,i], side = 3, line = -3,adj = .95, cex = .7, font=1) 
  box()
  if (i %in% 3) {
    axis(1)
    mtext(expression(paste(italic(Microstegium)," density")), side = 1, outer = F, cex = 1, line=2.2)
    mtext("treatment (# individuals)", side = 1, outer = F, cex = 1, line=3.2)
    } #x-axes
  axis(2) #y-axes
  mtext(resplabels[i], side = 2, outer = F, cex = 1, line=2.2)
  
  #Model2
  model2<-lmer(resp ~ mvtrt + logtotal + mvtrt:logtotal + I(logtotal^2) +(1|bk), data=data)
  models2[[i]]<-model2
  model2a<-lmer(resp ~ mvtrt + logtotal + mvtrt:logtotal +(1|bk), data=data)
  model2b<-lmer(resp ~ mvtrt + logtotal +(1|bk), data=data)
  model2c<-lmer(resp ~ mvtrt +(1|bk), data=data)
  model2d<-lmer(resp ~ 1 +(1|bk), data=data)
  term4<-anova(model2a,model2)
  term3<-anova(model2b,model2a)
  term2<-anova(model2c,model2b)
  term1<-anova(model2d,model2c)
  terms<-list(term1,term2,term3,term4)
  for(k in 1:length(terms)){
    termeff<-terms[[k]]
    delAIC<-termeff[2,2]-termeff[1,2]
    Chisq<-termeff[2,5]
    ChisqDF<-termeff[2,6]
    pval<-termeff[2,7]
    respname<-paste(respnames[i])
    termname<-paste('term',k)
    termeffrow<-c(respname, termname,delAIC,Chisq,ChisqDF,pval)
    termeffs2<-rbind(termeffs2,termeffrow)
  }
  
  
  #Plot Model2
  cs2 <- fixef(model2) 
  re2 <- ranef(model2)
  plot(resp ~ logtotal, data, type='n', axes = FALSE,
       ylim=ylims[,i],
       ylab=respnames[i],
       xlim=xlim2,
       xlab='Log-transformed \n total dry aboveground biomass')
  points(y=data1N[,1], x=data1N$logtotal, col=cols[1], pch=pchs[1], cex=.8)
  points(y=data1P[,1], x=data1P$logtotal, col=cols[2], pch=pchs[2], cex=.8)
  points(y=data1S[,1], x=data1S$logtotal, col=cols[3], pch=pchs[3], cex=.8)
  if (i %in% c(1,2)) {curve(cs2[1] + cs2[2] + (cs2[3]+cs2[5])*x + cs2[4]*x^2,  min(logtotal), max(logtotal), add=T, col=cols[2])}
  #legend('topright', legend=c('Model2: Plant biomass'), lty=1, col=c('gray'), cex=.7)
  mtext(pl[2,i], side = 3, line = -1.5, adj = 0.05, cex = 1, font=2) 
  mtext(panellabs[2,i], side = 3, line = -4, adj = .95, cex = .7, font=1) 
  box()
  if (i %in% 3) {
    axis(1)
    mtext('Log-transformed \n total dry aboveground biomass', side = 1, outer = F, cex = 1, line=3.2)}
  
  if (i %in% 1) {
    legend(x=5.25,y=107.25, 
           legend=c("No neighbor", expression(italic(Panicum)),expression(italic(Sorghum)), NA,
                    "No neighbor fit",expression(paste(italic(Panicum)," fit")),expression(paste(italic(Sorghum)," fit")), NA,
                    "Total \nbiomass fit", NA), 
           cex=.8,
           pch = c(pchs,NA,rep(NA,3),NA,NA,NA), 
           lty = c(rep(NA,3),NA,ltys,NA,1,NA), 
           col=c(cols,NA,linecols,NA,cols[2],NA),  
           xpd=NA)
  }

}

#export 6-panel figure - fig3_mixedcomm.png
#export model summary, nested anova results
colnames(termeffs1)<-c('resp','term','delAIC','Chisq','ChisqDF','pval')
colnames(termeffs1)<-c('resp','term','delAIC','Chisq','ChisqDF','pval')
#write.table(termeffs1, file='termeffs1_mixedcomm.txt', sep='\t', row.names=F, col.names=T)
#write.table(termeffs2, file='termeffs2_mixedcomm.txt', sep='\t', row.names=F, col.names=T)

modelfits<-list()
#THIS DOESN'T WORK BECAUSE OF LMER FXN UPDATE, SEE ABOVE
for (i in 1:length(models)){
  logLik<-summary(models[[i]])@logLik
  logLikrow<-c(respnames[i],NA,NA,logLik)
  REmat<-summary(models[[i]])@REmat
  coefs<-summary(models[[i]])@coefs
  coefmat<-cbind(rownames(coefs),coefs)
  modelfit<-rbind(logLikrow,REmat,coefmat)
  modelfits[[i]]<-modelfit
}
modelfits1<-data.frame(modelfits)
#write.table(modelfits1, file='modelfits1_mixedcomm.txt', sep='\t', row.names=F, col.names=T)

#need to add community biomass
resp<-dataaf$total
data<-data.frame(resp,mvtrt,comptrt,logtotal,bk)
model1<-lmer(resp ~ mvtrt + comptrt + mvtrt:comptrt+(1|bk), data=data)
#THIS DOESN'T WORK BECAUSE OF LMER FXN UPDATE, SEE ABOVE
logLik<-summary(model1)@logLik
logLikrow<-c('total',NA,NA,logLik)
REmat<-summary(model1)@REmat
coefs<-summary(model1)@coefs
coefmat<-cbind(rownames(coefs),coefs)
modelfit.total<-rbind(logLikrow,REmat,coefmat) ## use this
#write.table(modelfit.total, file='modelfit1total_mixedcomm.txt', sep='\t', row.names=F, col.names=T)
model1a<-lmer(resp ~ mvtrt + comptrt +(1|bk), data=data)
model1b<-lmer(resp ~ mvtrt +(1|bk), data=data)
model1c<-lmer(resp ~ 1 +(1|bk), data=data)
term3<-anova(model1a,model1)
term2<-anova(model1b,model1a)
term1<-anova(model1c,model1b)
terms<-list(term1,term2,term3)
termeffs1<-numeric(0)
for(k in 1:length(terms)){
  termeff<-terms[[k]]
  delAIC<-termeff[2,2]-termeff[1,2]
  Chisq<-termeff[2,5]
  ChisqDF<-termeff[2,6]
  pval<-termeff[2,7]
  respname<-'total'
  termname<-paste('term',k)
  termeffrow<-c(respname,termname,delAIC,Chisq,ChisqDF,pval)
  termeffs1<-rbind(termeffs1,termeffrow)
}
colnames(termeffs1)<-c('resp','term','delAIC','Chisq','ChisqDF','pval')
termeffs1.total<-termeffs1 ## use this
#write.table(termeffs1.total, file='termeffs1total_mixedcomm.txt', sep='\t', row.names=F, col.names=T)

modelfits<-list()
#THIS DOESN'T WORK BECAUSE OF LMER FXN UPDATE, SEE ABOVE
for (i in 1:length(models2)){
  logLik<-summary(models2[[i]])@logLik
  logLikrow<-c(respnames[i],NA,NA,logLik)
  REmat<-summary(models2[[i]])@REmat
  coefs<-summary(models2[[i]])@coefs
  coefmat<-cbind(rownames(coefs),coefs)
  modelfit<-rbind(logLikrow,REmat,coefmat)
  modelfits[[i]]<-modelfit
}
modelfits2<-data.frame(modelfits)
#write.table(modelfits2, file='modelfits2_mixedcomm.txt', sep='\t', row.names=F, col.names=T)


#Find r2 for Fig 2d and 2e
resp<-resps[,2] #soil moi
data<-data.frame(resp,mvtrt,comptrt,logtotal,bk)
mod<-lm(resp ~ logtotal + I(logtotal^2), data=data)
#######


###########################################
###########################################


###########################################
#MIXED COMMUNITY POTS - SUPPLEMENTAL

#1# Covariance among mvtrt, mvabund, mvrelabund
#######
#1.1# PLOT of mvabund v mvtrt, mvrelabund v mvtrt
#ESM Figure
#######
par(mfrow = c(2, 1)) #number of panels
par(cex = 0.8) #font size
par(mar = c(0, 4, 0, 0), oma = c(5, 5, 0.5, 15)) #size of inner, outer margins 
par(tcl = -0.25) #length of tick marks
par(mgp = c(2, 0.6, 0))

data<-dataaf
COMPS<-c('N','P','S')
vec<-c(5,4,2,1)
#PANEL A
plot(compabund ~ mvtrt, data,type='n',axes = FALSE,
     xlim=c(.5,5.5),
     ylim=c(0,105),
     xlab='', ylab='')
box()
for (g in 1:length(COMPS)){
  datag<-subset(data, comptrt==COMPS[g])
  dat<-data.table(datag)
  sum<-dat[,list(mean=mean(abund), se=sd(abund)/sqrt(length(abund))),by=mvtrt]
  mean<-sum$mean
  se<-sum$se
  #points(y=datag$abund, x=jitter(datag$mvtrt), col=cols[g], pch=pchs[g], cex=.8)
  lines(x=vec+.1,y=mean, type="b", pch=pchs[g],col=cols[g], lty=ltys[g])
  arrows(vec+.1,mean+se,vec+.1,mean-se, angle=90, code=3, length=0, col=cols[g])
}
for (g in 2:3){
  datag<-subset(data, comptrt==COMPS[g])
  dat<-data.table(datag)
  sum<-dat[,list(mean=mean(compabund), se=sd(compabund)/sqrt(length(compabund))),by=mvtrt]
  mean<-sum$mean
  se<-sum$se
  #points(y=datag$compabund, x=jitter(datag$mvtrt), col=cols[g], pch=pchs[g], cex=.8)
  lines(x=vec+.1,y=mean, type="b", pch=pchs2[g],col=cols[g], lty=ltys[g])
  arrows(vec+.1,mean+se,vec+.1,mean-se, angle=90, code=3, length=0, col=cols[g])
}
legend(x=5.75,y=109, 
       legend=c(expression(paste(italic(Microstegium),", No neighbor")),
                expression(paste(italic(Microstegium),", with ", italic(Panicum))),
                expression(paste(italic(Microstegium),", with ", italic(Sorghum))),
                NA,
                expression(italic(Panicum)),
                expression(italic(Sorghum))
                ), 
       pch=c(pchs,NA,pchs2[2],pchs2[3]), 
       col=c(cols,NA,cols[2],cols[3]),
       lty=c(ltys,NA,ltys[2],ltys[3]), 
       cex=.8,y.intersp=1.5,  
       xpd=NA)
mtext('a', side = 3, line = -1.5, adj = 0.05, cex = 1, font=2) 
axis(2)
mtext('Species abundance (g)', side = 2, outer = F, cex = 1, line=2.2)
# #PANEL B
# plot(compabund ~ mvtrt, data, type='n',axes = FALSE,
#      xlim=c(.5,5.5),
#      ylim=NULL, 
#      xlab='',ylab='')
# box()
# for (g in 2:3){
#   datag<-subset(data, comptrt==COMPS[g])
#   dat<-data.table(datag)
#   sum<-dat[,list(mean=mean(compabund), se=sd(compabund)/sqrt(length(compabund))),by=mvtrt]
#   mean<-sum$mean
#   se<-sum$se
#   #points(y=datag$compabund, x=jitter(datag$mvtrt), col=cols[g], pch=pchs[g], cex=.8)
#   lines(x=vec+.1,y=mean, type="b", pch=pchs[g],col=cols[g], lty=ltys[g])
#   arrows(vec+.1,mean+se,vec+.1,mean-se, angle=90, code=3, length=0, col=cols[g])
# }
# #legend('topright', legend=c('No comp','P.v.','S.b.'), pch=pchs, col=cols, cex=.7)
# mtext('b', side = 3, line = -1.5, adj = 0.05, cex = 1, font=2) 
# axis(2)
# title(ylab='Comp. abundance (g)')
#PANEL C
plot(relabund ~ mvtrt, data, type='n',axes = FALSE,
     xlim=c(.5,5.5),
     ylim=c(0,110), 
     xlab='',ylab='')
box()
for (g in 1:length(COMPS)){
  datag<-subset(dataaf, comptrt==COMPS[g])
  dat<-data.table(datag)
  sum<-dat[,list(mean=mean(relabund), se=sd(relabund)/sqrt(length(relabund))),by=mvtrt]
  mean<-sum$mean
  se<-sum$se
  #points(y=datag$relabund, x=jitter(datag$mvtrt), col=cols[g], pch=pchs[g], cex=.8)
  lines(x=vec+.1,y=mean, type="b", pch=pchs[g],col=cols[g], lty=ltys[g])
  arrows(vec+.1,mean+se,vec+.1,mean-se, angle=90, code=3, length=0, col=cols[g])
}
legend(x=5.75,y=114, 
       legend=c('No neighbor',
                expression(italic(Panicum)),
                expression(italic(Sorghum))), 
       pch=pchs, 
       lty=ltys, 
       col=cols, 
       cex=.8, 
       y.intersp=1.5,
       xpd=NA)
mtext('b', side = 3, line = -1.5, adj = 0.05, cex = 1, font=2) 
axis(2)
axis(1)
mtext(expression(italic(Microstegium)), side = 2, outer = F, cex = 1, line=3.2)
mtext("relative abundance (%)", side = 2, outer = F, cex = 1, line=2.2)
       
mtext(expression(paste(italic(Microstegium)," density")), side = 1, outer = F, cex = 1, line=2.2)
mtext("treatment (# individuals)", side = 1, outer = F, cex = 1, line=3.2)

#title(xlab='M.v. individuals per pot', outer=T)
#export 3-panel figure 'abund v mvtrt.png'
#######
#1.2.a lme4 model: mvabund<-mvtrt+(1|bk) and test of fixed effects
#1.2.b lme4 model: relmvabund<-mvtrt+(1|bk) and test of fixed effects
#######
modelA<-lmer(abund ~ mvtrt +(1|bk), data=data)
modelAa<-lmer(abund ~ 1 +(1|bk), data=data)
termeff<-anova(modelAa,modelA)
delAIC<-termeff[2,2]-termeff[1,2]
Chisq<-termeff[2,5]
ChisqDF<-termeff[2,6]
pval<-termeff[2,7]
termname<-'M.v. individuals per pot'
respname<-'M.v. abundance (g)'
termeffA<-c(respname, termname,delAIC,Chisq,ChisqDF,pval)

modelB<-lmer(relabund ~ mvtrt +(1|bk), data=data)
modelBa<-lmer(relabund ~ 1 +(1|bk), data=data)
termeff<-anova(modelBa,modelB)
delAIC<-termeff[2,2]-termeff[1,2]
Chisq<-termeff[2,5]
ChisqDF<-termeff[2,6]
pval<-termeff[2,7]
termname<-'M.v. individuals per pot'
respname<-'M.v. relative abundance (%)'
termeffB<-c(respname, termname,delAIC,Chisq,ChisqDF,pval)

termeffs<-rbind(termeffA,termeffB)
colnames(termeffs)<-c('resp','term','delAIC','Chisq','ChisqDF','pval')
#write.table(termeffs, file='termeffs_abund_mixedcomm.txt', sep='\t', row.names=F, col.names=T)
#######

#2# Correlary to Parts 3 and 4 - Make figure 3 and eval model
#######
#2.1 FOR AMMONIUM / NITRATE / TOTAL INORGANIC N
#Not included in the manuscript
#######
par(mfrow = c(3, 2)) #number of panels
par(cex = 0.8) #font size
par(mar = c(0, 0, 0, 0), oma = c(5, 4, 0.5, 8)) #size of inner, outer margins 
par(tcl = -0.25) #length of tick marks
par(mgp = c(2, 0.6, 0))

xlim1 <- c(0.5,5.5)
xlim2 <- c(1.9,5.1)

resps<-databf[,c(1:3)]
respnames<-c('Ammonium (ugN/G)','Nitrate (ugN/G)','Total Inorganic N (ugN/G)')
shortnames<-c('nhdi','nodi','totdi')
yname1<-expression(paste("Ammonium (",mu,"gN/G)"))
yname2<-expression(paste("Nitrate (",mu,"gN/G)"))
yname3<-expression(paste("Total Inorganic N (",mu,"gN/G)"))  
resplabels<-c(yname1,yname2,yname3)
yl2<-c(0,50) #nhd
yl3<-c(0,115) #nodi
yl4<-c(0,115) #totd
ylims<-cbind(yl2,yl3,yl4)
cols<-c(1,'darkgray','darkgray')
pchs<-c(16,17,15)
pchs2<-c(1,2,0)
linecols<-c(1,1,'darkgray')
ltys<-c(1,2,2)
termeffs1<-numeric(0)
termeffs2<-numeric(0)
pl1<-c('a','d')
pl2<-c('b','e')
pl3<-c('c','f')
pl<-cbind(pl1,pl2,pl3)
panelad<-c('mvtrt  ns\nneigtrt    *\ninteraction  ns',
           'mvtrt  ns\nlogtotal   *\ninteraction  ns\nlogtotal^2  **')
panelbe<-c('mvtrt  ns\nneigtrt  ***\ninteraction  ns',
           'mvtrt  ns\nlogtotal ***\ninteraction  ns\nlogtotal^2  ns')
panelcf<-c('mvtrt   .\nneigtrt  ***\ninteraction  ns',
           'mvtrt   .\nlogtotal ***\ninteraction  ns\nlogtotal^2  ns')
panellabs<-cbind(panelad,panelbe,panelcf)
termeffs1<-numeric(0)
termeffs2<-numeric(0)

for (i in 1:dim(resps)[2]){
  resp<-resps[,i]
  data<-data.frame(resp,mvtrt,comptrt,logtotal,bk)
  data1N<-subset(data, comptrt=='N')
  data1P<-subset(data, comptrt=='P')
  data1S<-subset(data, comptrt=='S')
  
  #Model1
  model1<-lmer(resp ~ mvtrt + comptrt + mvtrt:comptrt+(1|bk), data=data)
  model1a<-lmer(resp ~ mvtrt + comptrt +(1|bk), data=data)
  model1b<-lmer(resp ~ mvtrt +(1|bk), data=data)
  model1c<-lmer(resp ~ 1 +(1|bk), data=data)
  term3<-anova(model1a,model1)
  term2<-anova(model1b,model1a)
  term1<-anova(model1c,model1b)
  terms<-list(term1,term2,term3)
  for(k in 1:length(terms)){
    termeff<-terms[[k]]
    delAIC<-termeff[2,2]-termeff[1,2]
    Chisq<-termeff[2,5]
    ChisqDF<-termeff[2,6]
    pval<-termeff[2,7]
    respname<-paste(respnames[i])
    termname<-paste('term',k)
    termeffrow<-c(respname,termname,delAIC,Chisq,ChisqDF,pval)
    termeffs1<-rbind(termeffs1,termeffrow)
  }
  
  #Plot Model1
  cs1 <- fixef(model1) 
  re1 <- ranef(model1)
  plot(resp ~ mvtrt, data, type='n',axes = FALSE,
       ylim=ylims[,i], 
       ylab=respnames[i], 
       xlim=xlim1,
       xlab='M.v. density treatment \n (# M.v. inds)')
  points(y=data1N[,1], x=jitter(data1N$mvtrt), col=cols[1], pch=pchs[1], cex=.8)
  points(y=data1P[,1], x=jitter(data1P$mvtrt), col=cols[2], pch=pchs[2], cex=.8)
  points(y=data1S[,1], x=jitter(data1S$mvtrt), col=cols[3], pch=pchs[3], cex=.8)
  curve(cs1[1] + cs1[2]*x, min(mvtrt), max(mvtrt), add=T, col=linecols[1], lty=ltys[1]) #group mean
  curve(cs1[1] + (cs1[2]+cs1[5])*x + cs1[3], min(mvtrt), max(mvtrt), add=T, col=linecols[2],lty=ltys[2]) #group mean
  curve(cs1[1] + (cs1[2]+cs1[6])*x + cs1[4], min(mvtrt), max(mvtrt), add=T, col=linecols[3],lty=ltys[3]) #group mean
  #legend('topright', legend=c('Model1: None','Model1: P.v.','Model1: S.b.'), lty=ltys, col=cols, , pch=pchs, cex=.7)
  mtext(pl[1,i], side = 3, line = -1.5, adj = 0.05, cex = 1, font=2) 
  mtext(panellabs[1,i], side = 3, line = -3,adj = .95, cex = .7, font=1) 
  box()
  if (i %in% 3) {
    axis(1)
    mtext(expression(paste(italic(Microstegium)," density")), side = 1, outer = F, cex = 1, line=2.2)
    mtext("treatment (# individuals)", side = 1, outer = F, cex = 1, line=3.2)
  } #x-axes
  axis(2) #y-axes
  mtext(resplabels[i], side = 2, outer = F, cex = 1, line=2.2)
  
  #Model2
  model2<-lmer(resp ~ mvtrt + logtotal + mvtrt:logtotal + I(logtotal^2) +(1|bk), data=data)
  model2a<-lmer(resp ~ mvtrt + logtotal + mvtrt:logtotal +(1|bk), data=data)
  model2b<-lmer(resp ~ mvtrt + logtotal +(1|bk), data=data)
  model2c<-lmer(resp ~ mvtrt +(1|bk), data=data)
  model2d<-lmer(resp ~ 1 +(1|bk), data=data)
  term4<-anova(model2a,model2)
  term3<-anova(model2b,model2a)
  term2<-anova(model2c,model2b)
  term1<-anova(model2d,model2c)
  terms<-list(term1,term2,term3,term4)
  for(k in 1:length(terms)){
    termeff<-terms[[k]]
    delAIC<-termeff[2,2]-termeff[1,2]
    Chisq<-termeff[2,5]
    ChisqDF<-termeff[2,6]
    pval<-termeff[2,7]
    respname<-paste(respnames[i])
    termname<-paste('term',k)
    termeffrow<-c(respname, termname,delAIC,Chisq,ChisqDF,pval)
    termeffs2<-rbind(termeffs2,termeffrow)
  }
  
  
  #Plot Model2
  cs2 <- fixef(model2) 
  re2 <- ranef(model2)
  plot(resp ~ logtotal, data, type='n', axes = FALSE,
       ylim=ylims[,i],
       ylab=respnames[i],
       xlim=xlim2,
       xlab='Log-transformed \n total dry aboveground biomass')
  points(y=data1N[,1], x=data1N$logtotal, col=cols[1], pch=pchs[1], cex=.8)
  points(y=data1P[,1], x=data1P$logtotal, col=cols[2], pch=pchs[2], cex=.8)
  points(y=data1S[,1], x=data1S$logtotal, col=cols[3], pch=pchs[3], cex=.8)
  if (i %in% c(1,2,3)) {curve(cs2[1] + cs2[2] + (cs2[3]+cs2[5])*x + cs2[4]*x^2,  min(logtotal), max(logtotal), add=T, col=cols[2])}
  #legend('topright', legend=c('Model2: Plant biomass'), lty=1, col=c('gray'), cex=.7)
  mtext(pl[2,i], side = 3, line = -1.5, adj = 0.05, cex = 1, font=2) 
  mtext(panellabs[2,i], side = 3, line = -4, adj = .95, cex = .7, font=1) 
  box()
  if (i %in% 3) {
    axis(1)
    mtext('Log-transformed \n total dry aboveground biomass', side = 1, outer = F, cex = 1, line=3.2)
  }
  if (i %in% 1) {
    legend(x=5.25,
           y=107.25, 
           legend=c("No neighbor", 
                    expression(italic(Panicum)),
                    expression(italic(Sorghum)), 
                    NA,
                    "No neighbor fit",
                    expression(paste(italic(Panicum)," fit")),
                    expression(paste(italic(Sorghum)," fit")), 
                    NA,
                    "Total \nbiomass fit", 
                    NA), 
           cex=.8,
           pch = c(pchs,NA,rep(NA,3),NA,NA,NA), 
           lty = c(rep(NA,3),NA,ltys,NA,1,NA), 
           col=c(cols,NA,linecols,NA,cols[2],NA),  
           xpd=NA)
    #I'm not sure why this legend isn't showing up... may need to fix the xy position
  }
  
}

#export 6-panel figure - fig3_mixedcomm.png
#export model summary, nested anova results
colnames(termeffs1)<-c('resp','term','delAIC','Chisq','ChisqDF','pval')
colnames(termeffs1)<-c('resp','term','delAIC','Chisq','ChisqDF','pval')
write.table(termeffs1, file='termeffs1_mixedcomm.txt', sep='\t', row.names=F, col.names=T)
write.table(termeffs2, file='termeffs2_mixedcomm.txt', sep='\t', row.names=F, col.names=T)
#######
#2.2 FOR AMMONIFICATION / NITRIFICATION / MINERALIZATION
#Not included in the manuscript
#######
par(mfrow = c(3, 2)) #number of panels
par(cex = 0.8) #font size
par(mar = c(0, 0, 0, 0), oma = c(5, 5, 0.5, 8)) #size of inner, outer margins 
par(tcl = -0.25) #length of tick marks
par(mgp = c(2, 0.6, 0))

xlim1 <- c(0.5,5.5)
xlim2 <- c(1.9,5.1)

resps<-datacf[,c(1:3)]
respnames<-c('Net Ammonification\n(ugN/G*d)','Net Nitrification\n (ugN/G*d)','Net Mineralization\n(ugN/G*d)')
shortnames<-c('ammonif','nitrif','minzd')
yl2<-c(-3.5,4.5) #ammonif
yl3<-c(-10,18) #nitrif
yl4<-c(-10,18) #minzd
ylims<-cbind(yl2,yl3,yl4)
cols<-c(1,'darkgray','darkgray')
pchs<-c(16,17,15)
pchs2<-c(1,2,0)
linecols<-c(1,1,'darkgray')
ltys<-c(1,2,2)
termeffs1<-numeric(0)
termeffs2<-numeric(0)
pl1<-c('a','d')
pl2<-c('b','e')
pl3<-c('c','f')
pl<-cbind(pl1,pl2,pl3)
panelad<-c('mvtrt  ns\nneigtrt    *\ninteraction  ns',
           'mvtrt  ns\nlogtotal   *\ninteraction  ns\nlogtotal^2   *')
panelbe<-c('mvtrt  ns\nneigtrt    *\ninteraction  ns',
           'mvtrt  ns\nlogtotal  ns\ninteraction  ns\nlogtotal^2  ns')
panelcf<-c('mvtrt  ns\nneigtrt    .\ninteraction  ns',
           'mvtrt  ns\nlogtotal  ns\ninteraction  ns\nlogtotal^2  ns')
panellabs<-cbind(panelad,panelbe,panelcf)
termeffs1<-numeric(0)
termeffs2<-numeric(0)

for (i in 1:dim(resps)[2]){
  resp<-resps[,i]
  data<-data.frame(resp,mvtrt,comptrt,logtotal,bk)
  data1N<-subset(data, comptrt=='N')
  data1P<-subset(data, comptrt=='P')
  data1S<-subset(data, comptrt=='S')
  
  #Model1
  model1<-lmer(resp ~ mvtrt + comptrt + mvtrt:comptrt+(1|bk), data=data)
  model1a<-lmer(resp ~ mvtrt + comptrt +(1|bk), data=data)
  model1b<-lmer(resp ~ mvtrt +(1|bk), data=data)
  model1c<-lmer(resp ~ 1 +(1|bk), data=data)
  term3<-anova(model1a,model1)
  term2<-anova(model1b,model1a)
  term1<-anova(model1c,model1b)
  terms<-list(term1,term2,term3)
  for(k in 1:length(terms)){
    termeff<-terms[[k]]
    delAIC<-termeff[2,2]-termeff[1,2]
    Chisq<-termeff[2,5]
    ChisqDF<-termeff[2,6]
    pval<-termeff[2,7]
    respname<-paste(respnames[i])
    termname<-paste('term',k)
    termeffrow<-c(respname,termname,delAIC,Chisq,ChisqDF,pval)
    termeffs1<-rbind(termeffs1,termeffrow)
  }
  
  #Plot Model1
  cs1 <- fixef(model1) 
  re1 <- ranef(model1)
  plot(resp ~ mvtrt, data, type='n',axes = FALSE,
       ylim=ylims[,i], 
       ylab=respnames[i], 
       xlim=xlim1,
       xlab='M.v. density treatment \n (# M.v. inds)')
  points(y=data1N[,1], x=jitter(data1N$mvtrt), col=cols[1], pch=pchs[1], cex=.8)
  points(y=data1P[,1], x=jitter(data1P$mvtrt), col=cols[2], pch=pchs[2], cex=.8)
  points(y=data1S[,1], x=jitter(data1S$mvtrt), col=cols[3], pch=pchs[3], cex=.8)
  curve(cs1[1] + cs1[2]*x, min(mvtrt), max(mvtrt), add=T, col=linecols[1], lty=ltys[1]) #group mean
  curve(cs1[1] + (cs1[2]+cs1[5])*x + cs1[3], min(mvtrt), max(mvtrt), add=T, col=linecols[2],lty=ltys[2]) #group mean
  curve(cs1[1] + (cs1[2]+cs1[6])*x + cs1[4], min(mvtrt), max(mvtrt), add=T, col=linecols[3],lty=ltys[3]) #group mean
  #legend('topright', legend=c('Model1: None','Model1: P.v.','Model1: S.b.'), lty=ltys, col=cols, , pch=pchs, cex=.7)
  mtext(pl[1,i], side = 3, line = -1.5, adj = 0.05, cex = 1, font=2) 
  mtext(panellabs[1,i], side = 3, line = -3,adj = .95, cex = .7, font=1) 
  box()
  if (i %in% 3) {
    axis(1)
    mtext('M.v. density treatment \n (# M.v. inds)', side = 1, outer = F, cex = 1, line=3.2)
  } #x-axes
  axis(2) #y-axes
  mtext(respnames[i], side = 2, outer = F, cex = 1, line=2.2)
  
  #Model2
  model2<-lmer(resp ~ mvtrt + logtotal + mvtrt:logtotal + I(logtotal^2) +(1|bk), data=data)
  model2a<-lmer(resp ~ mvtrt + logtotal + mvtrt:logtotal +(1|bk), data=data)
  model2b<-lmer(resp ~ mvtrt + logtotal +(1|bk), data=data)
  model2c<-lmer(resp ~ mvtrt +(1|bk), data=data)
  model2d<-lmer(resp ~ 1 +(1|bk), data=data)
  term4<-anova(model2a,model2)
  term3<-anova(model2b,model2a)
  term2<-anova(model2c,model2b)
  term1<-anova(model2d,model2c)
  terms<-list(term1,term2,term3,term4)
  for(k in 1:length(terms)){
    termeff<-terms[[k]]
    delAIC<-termeff[2,2]-termeff[1,2]
    Chisq<-termeff[2,5]
    ChisqDF<-termeff[2,6]
    pval<-termeff[2,7]
    respname<-paste(respnames[i])
    termname<-paste('term',k)
    termeffrow<-c(respname, termname,delAIC,Chisq,ChisqDF,pval)
    termeffs2<-rbind(termeffs2,termeffrow)
  }
  
  
  #Plot Model2
  cs2 <- fixef(model2) 
  re2 <- ranef(model2)
  plot(resp ~ logtotal, data, type='n', axes = FALSE,
       ylim=ylims[,i],
       ylab=respnames[i],
       xlim=xlim2,
       xlab='Log-transformed \n total dry aboveground biomass')
  points(y=data1N[,1], x=data1N$logtotal, col=cols[1], pch=pchs[1], cex=.8)
  points(y=data1P[,1], x=data1P$logtotal, col=cols[2], pch=pchs[2], cex=.8)
  points(y=data1S[,1], x=data1S$logtotal, col=cols[3], pch=pchs[3], cex=.8)
  if (i %in% c(1)) {curve(cs2[1] + cs2[2] + (cs2[3]+cs2[5])*x + cs2[4]*x^2,  min(logtotal), max(logtotal), add=T, col=cols[2])}
  #legend('topright', legend=c('Model2: Plant biomass'), lty=1, col=c('gray'), cex=.7)
  mtext(pl[2,i], side = 3, line = -1.5, adj = 0.05, cex = 1, font=2) 
  mtext(panellabs[2,i], side = 3, line = -4, adj = .95, cex = .7, font=1) 
  box()
  if (i %in% 3) {
    axis(1)
    mtext('Log-transformed \n total dry aboveground biomass', side = 1, outer = F, cex = 1, line=3.2)}
  
  if (i %in% 1) {
    legend(x=5.25,y=4.5, 
           legend=c("No neighbor", "P.v.","S.b.", NA,
                    "No neighbor fit","P.v. fit","S.b. fit", NA,
                    "Total \nbiomass fit", NA), 
           cex=.8,
           pch = c(pchs,NA,rep(NA,3),NA,NA,NA), 
           lty = c(rep(NA,3),NA,ltys,NA,1,NA), 
           col=c(cols,NA,linecols,NA,cols[2],NA),  
           xpd=NA)
  }
  
}

#export 6-panel figure - fig3_mixedcomm.png
#export model summary, nested anova results
colnames(termeffs1)<-c('resp','term','delAIC','Chisq','ChisqDF','pval')
colnames(termeffs1)<-c('resp','term','delAIC','Chisq','ChisqDF','pval')
#write.table(termeffs1, file='termeffs1_mixedcomm.txt', sep='\t', row.names=F, col.names=T)
#write.table(termeffs2, file='termeffs2_mixedcomm.txt', sep='\t', row.names=F, col.names=T)
#######
#2.3 FOR AMMONIUM / TOTAL INORGANIC N / AMMONIFICATION / MINERALIZATION
#ESM figure
#######
par(mfrow = c(4, 2)) #number of panels
par(cex = 0.8) #font size
par(mar = c(0, 0, 0, 0), oma = c(5, 4, 0.5, 8)) #size of inner, outer margins 
par(tcl = -0.25) #length of tick marks
par(mgp = c(2, 0.6, 0))

xlim1 <- c(0.5,5.5)
xlim2 <- c(1.9,5.1)

resps<-cbind(databf[,c(1,3)],datacf[,c(1,3)])
respnames<-c('Ammonium (ugN/G)','Total Inorganic N (ugN/G)','Ammonification (ugN/G*d)','Mineralization (ugN/G*d)')
shortnames<-c('nhdi','totdi','ammonifd','minzd')
yname1<-"Ammonium"
yname2<-"Total Inorg. N"
yname3<-"Ammonification"
yname4<-"Mineralization"
resplabels<-c(yname1,yname2,yname3,yname4)
y2name1<-expression(paste("(",mu,"gN/G)"))
y2name2<-expression(paste("(",mu,"gN/G)"))
y2name3<-expression(paste("(",mu,"gN/G*d)"))
y2name4<-expression(paste("(",mu,"gN/G*d)"))
resplabels2<-c(y2name1,y2name2,y2name3,y2name4)
yl2<-c(0,55) #nhdi
yl3<-c(0,130) #totdi
yl4<-c(-3,5) #ammonifd
yl5<-c(-6,19) #minzd
ylims<-cbind(yl2,yl3,yl4,yl5)
cols<-c(1,'darkgray','darkgray')
pchs<-c(16,17,15)
pchs2<-c(1,2,0)
linecols<-c(1,1,'darkgray')
ltys<-c(1,2,2)
termeffs1<-numeric(0)
termeffs2<-numeric(0)
pl1<-c('a','e')
pl2<-c('b','f')
pl3<-c('c','g')
pl4<-c('d','h')
pl<-cbind(pl1,pl2,pl3,pl4)
panelae<-c('mvtrt  ns\nneigtrt    *\ninteraction  ns',
           'mvtrt  ns\nlogtotal   *\ninteraction  ns\nlogtotal^2  **')
panelbf<-c('mvtrt  ns\nneigtrt  ***\ninteraction  ns',
           'mvtrt  ns\nlogtotal ***\ninteraction  ns\nlogtotal^2  ns')
panelcg<-c('mvtrt  ns\nneigtrt    *\ninteraction  ns',
           'mvtrt  ns\nlogtotal   *\ninteraction  ns\nlogtotal^2   *')
paneldh<-c('mvtrt  ns\nneigtrt   ns\ninteraction  ns',
           'mvtrt  ns\nlogtotal  ns\ninteraction  ns\nlogtotal^2  ns')
panellabs<-cbind(panelae,panelbf,panelcg,paneldh)
termeffs1<-numeric(0)
termeffs2<-numeric(0)

for (i in 1:dim(resps)[2]){
  resp<-resps[,i]
  data<-data.frame(resp,mvtrt,comptrt,logtotal,bk)
  data1N<-subset(data, comptrt=='N')
  data1P<-subset(data, comptrt=='P')
  data1S<-subset(data, comptrt=='S')
  
  #Model1
  model1<-lmer(resp ~ mvtrt + comptrt + mvtrt:comptrt+(1|bk), data=data)
  model1a<-lmer(resp ~ mvtrt + comptrt +(1|bk), data=data)
  model1b<-lmer(resp ~ mvtrt +(1|bk), data=data)
  model1c<-lmer(resp ~ 1 +(1|bk), data=data)
  term3<-anova(model1a,model1)
  term2<-anova(model1b,model1a)
  term1<-anova(model1c,model1b)
  terms<-list(term1,term2,term3)
  for(k in 1:length(terms)){
    termeff<-terms[[k]]
    delAIC<-termeff[2,2]-termeff[1,2]
    Chisq<-termeff[2,5]
    ChisqDF<-termeff[2,6]
    pval<-termeff[2,7]
    respname<-paste(respnames[i])
    termname<-paste('term',k)
    termeffrow<-c(respname,termname,delAIC,Chisq,ChisqDF,pval)
    termeffs1<-rbind(termeffs1,termeffrow)
  }
  
  #Plot Model1
  cs1 <- fixef(model1) 
  re1 <- ranef(model1)
  plot(resp ~ mvtrt, data, type='n',axes = FALSE,
       ylim=ylims[,i], 
       ylab=respnames[i], 
       xlim=xlim1,
       xlab='M.v. density treatment \n (# M.v. inds)')
  points(y=data1N[,1], x=jitter(data1N$mvtrt), col=cols[1], pch=pchs[1], cex=.8)
  points(y=data1P[,1], x=jitter(data1P$mvtrt), col=cols[2], pch=pchs[2], cex=.8)
  points(y=data1S[,1], x=jitter(data1S$mvtrt), col=cols[3], pch=pchs[3], cex=.8)
  curve(cs1[1] + cs1[2]*x, min(mvtrt), max(mvtrt), add=T, col=linecols[1], lty=ltys[1]) #group mean
  curve(cs1[1] + (cs1[2]+cs1[5])*x + cs1[3], min(mvtrt), max(mvtrt), add=T, col=linecols[2],lty=ltys[2]) #group mean
  curve(cs1[1] + (cs1[2]+cs1[6])*x + cs1[4], min(mvtrt), max(mvtrt), add=T, col=linecols[3],lty=ltys[3]) #group mean
  #legend('topright', legend=c('Model1: None','Model1: P.v.','Model1: S.b.'), lty=ltys, col=cols, , pch=pchs, cex=.7)
  mtext(pl[1,i], side = 3, line = -1.5, adj = 0.05, cex = 1, font=2) 
  mtext(panellabs[1,i], side = 3, line = -3,adj = .95, cex = .7, font=1) 
  box()
  if (i %in% 4) {
    axis(1)
    mtext(expression(paste(italic(Microstegium)," density")), side = 1, outer = F, cex = 1, line=2.2)
    mtext("treatment (# individuals)", side = 1, outer = F, cex = 1, line=3.2)
  } #x-axes
  axis(2) #y-axes
  mtext(resplabels[i], side = 2, outer = F, cex = 1, line = 2.5)
  mtext(resplabels2[i], side = 2, outer = F, cex = 1, line = 1.5)
  
  #Model2
  model2<-lmer(resp ~ mvtrt + logtotal + mvtrt:logtotal + I(logtotal^2) +(1|bk), data=data)
  model2a<-lmer(resp ~ mvtrt + logtotal + mvtrt:logtotal +(1|bk), data=data)
  model2b<-lmer(resp ~ mvtrt + logtotal +(1|bk), data=data)
  model2c<-lmer(resp ~ mvtrt +(1|bk), data=data)
  model2d<-lmer(resp ~ 1 +(1|bk), data=data)
  term4<-anova(model2a,model2)
  term3<-anova(model2b,model2a)
  term2<-anova(model2c,model2b)
  term1<-anova(model2d,model2c)
  terms<-list(term1,term2,term3,term4)
  for(k in 1:length(terms)){
    termeff<-terms[[k]]
    delAIC<-termeff[2,2]-termeff[1,2]
    Chisq<-termeff[2,5]
    ChisqDF<-termeff[2,6]
    pval<-termeff[2,7]
    respname<-paste(respnames[i])
    termname<-paste('term',k)
    termeffrow<-c(respname, termname,delAIC,Chisq,ChisqDF,pval)
    termeffs2<-rbind(termeffs2,termeffrow)
  }
  
  
  #Plot Model2
  cs2 <- fixef(model2) 
  re2 <- ranef(model2)
  plot(resp ~ logtotal, data, type='n', axes = FALSE,
       ylim=ylims[,i],
       ylab=respnames[i],
       xlim=xlim2,
       xlab='Log-transformed \n total dry aboveground biomass')
  points(y=data1N[,1], x=data1N$logtotal, col=cols[1], pch=pchs[1], cex=.8)
  points(y=data1P[,1], x=data1P$logtotal, col=cols[2], pch=pchs[2], cex=.8)
  points(y=data1S[,1], x=data1S$logtotal, col=cols[3], pch=pchs[3], cex=.8)
  if (i %in% c(1,2,3,4)) {curve(cs2[1] + cs2[2] + (cs2[3]+cs2[5])*x + cs2[4]*x^2,  min(logtotal), max(logtotal), add=T, col=cols[2])}
  #legend('topright', legend=c('Model2: Plant biomass'), lty=1, col=c('gray'), cex=.7)
  mtext(pl[2,i], side = 3, line = -1.5, adj = 0.05, cex = 1, font=2) 
  mtext(panellabs[2,i], side = 3, line = -4, adj = .95, cex = .7, font=1) 
  box()
  if (i %in% 4) {
    axis(1)
    mtext('Log-transformed \n total dry aboveground biomass', side = 1, outer = F, cex = 1, line=3.2)
  }
  if (i %in% 1) {
    legend(x=5.25,
           y=130, 
           legend=c("No neighbor", 
                    expression(italic(Panicum)),
                    expression(italic(Sorghum)), 
                    NA,
                    "No neighbor fit",
                    expression(paste(italic(Panicum)," fit")),
                    expression(paste(italic(Sorghum)," fit")), 
                    NA,
                    "Total \nbiomass fit", 
                    NA), 
           cex=.8,
           pch = c(pchs,NA,rep(NA,3),NA,NA,NA), 
           lty = c(rep(NA,3),NA,ltys,NA,1,NA), 
           col=c(cols,NA,linecols,NA,cols[2],NA),  
           xpd=NA)
  }
  
}

#export 8-panel figure - esm8.png
#export model summary, nested anova results
colnames(termeffs1)<-c('resp','term','delAIC','Chisq','ChisqDF','pval')
colnames(termeffs2)<-c('resp','term','delAIC','Chisq','ChisqDF','pval')
#write.table(termeffs1, file='termeffs1_esm8.txt', sep='\t', row.names=F, col.names=T)
#write.table(termeffs2, file='termeffs2_esm8.txt', sep='\t', row.names=F, col.names=T)



