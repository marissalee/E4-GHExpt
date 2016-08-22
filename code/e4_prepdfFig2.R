#e4_prepdfFig2.R
#Prep dataframe for Figure 2


### Remove unnecessary cols ###

#str(data)
removecols<-c('julydate',
              'pavi','sobi','weed',
              'nhdi','nodi','totdi','ammonifd','nitrifd','minzd','soilmoi',
              'notes')
indx<-colnames(data) %in% removecols
data.r<-data[,!indx]

#colnames(data.r)
ru.data<- data.frame(potid=data.r$potid,
                     bk=data.r$bk,
                     type=data.r$type,
                     comptrt=data.r$comptrt,
                     mvtrt=data.r$mvtrt,
                     relmivi=data.r$relmivi,
                     mivi=data.r$mivi,
                     compabund=data.r$compabund,
                     total=data.r$total)
data2.1 <- ru.data
#View(data2.1)




### Reshape ###
library(reshape2)

## Reshape so that plant biomass values are all in one column (biomval), with an identifier column to identify what type of biomass that value represents (biommeas)  
data2.2 <- melt(data2.1, measure.vars=c('relmivi','mivi','compabund','total'), id.vars=c('potid','bk','type','comptrt','mvtrt'))
whichvar<-which(colnames(data2.2)=='variable')
whichval<-which(colnames(data2.2)=='value')
colnames(data2.2)[whichvar]<-'biommeas'
colnames(data2.2)[whichval]<-'biomval'




### Check structure ###

data2.2$potid<-as.factor(data2.2$potid)
data2.2$bk<-as.factor(data2.2$bk)
data2.2$mvtrt<-as.ordered(data2.2$mvtrt)
#str(data2.2)

contrasts(data2.2$mvtrt) <- contr.poly(6, c(0,1,2,4,5,6)) 


### Make it a nice name ###

data2 <- data2.2






