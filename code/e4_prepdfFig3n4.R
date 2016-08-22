#e4_prepdfFig3n4.R
#Prep dataframe for Figure 3 and 4


### Remove unnecessary cols ###

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
data3.1 <- ru.data




### Reshape ###
library(reshape2)

## Reshape so that plant biomass values are all in one column (biomval), with an identifier column to identify what type of biomass that value represents (biommeas) and add a column that weights the soilvals by total plant biomass
data3.2 <- melt(data3.1, measure.vars=c('nhdi','nodi','totdi','ammonifd','nitrifd','minzd','soilmoi'), id.vars=c('potid','bk','type','comptrt','mvtrt','mivi','total'))
whichvar<-which(colnames(data3.2)=='variable')
whichval<-which(colnames(data3.2)=='value')
colnames(data3.2)[whichvar]<-'soilmeas'
colnames(data3.2)[whichval]<-'soilval'




### Check structure ###

#str(data3.2)




### Make it a nice name ###

data3 <- data3.2










