#e4_prepdfFig2.R
#Prep dataframe for Figure 2


### Add relmivi column ###

## Calculate the relative Mv abundance, add it as a column  
#str(data)
data$relmivi <- (data$mivi / data$total) * 100

# Force pots with mivi = 0 and total = 0 to have relmivi = 0 too
tmp<-data[data$mivi == 0,]
#View(tmp)
len<-length(data[data$mivi == 0,'relmivi'])
data[data$mivi == 0,'relmivi'] <- rep(0,30)




### Remove unnecessary cols ###

#str(data)
removecols<-c('bk','julydate',
              'pavi','sobi','weed',
              'nhdi','nodi','totdi','ammonifd','nitrifd','minzd','soilmoi',
              'notes')
indx<-colnames(data) %in% removecols
data.r<-data[,!indx]

#colnames(data.r)
ru.data<- data.frame(potid=data.r$potid,
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

## Reshape so that plant biomass values are all in one column (biomval), with an identifier column to identify what type of biomass that value represents (biommeas)  
data2.2 <- melt(data2.1, measure.vars=c('relmivi','mivi','compabund','total'), id.vars=c('potid','type','comptrt','mvtrt'))
colnames(data2.2)[5]<-'biommeas'
colnames(data2.2)[6]<-'biomval'




### Check structure ###

data2.2$potid<-as.factor(data2.2$potid)
data2.2$mvtrt<-as.factor(data2.2$mvtrt)
#str(data2.2)




### Make it a nice name ###

data2 <- data2.2






