#e4_prepdfFig4.R
#Prep dataframe for Figure 4


### Remove unnecessary cols ###

#str(datas)
removecols<-c('s0Epid','s0Ppid','s0E','s0P','julydate','type','delta.sE','delta.sP')
indx<-colnames(datas) %in% removecols
datas.r<-datas[,!indx]
ru.datas <- data.frame(sFpid=datas.r$sFpid,
                       bk=datas.r$bk,
                       comptrt=datas.r$comptrt,
                       mvtrt=datas.r$mvtrt,
                       mivi=datas.r$mivi,
                       compabund=datas.r$compabund,
                       total=datas.r$total,
                       soilmeas=datas.r$scol,
                       sF=datas.r$sF)
data4.1 <- ru.datas




### Reshape ###

## Reshape so that plant biomass values are all in one column (biomval), with an identifier column to identify what type of biomass that value represents (biommeas)  
data4.2 <- melt(data4.1, measure.vars=c('mivi','compabund','total'), id.vars=c('sFpid','bk','comptrt','mvtrt','soilmeas','sF'))
whichvar<-which(colnames(data4.2)=='variable')
whichval<-which(colnames(data4.2)=='value')
colnames(data4.2)[whichvar]<-'biommeas'
colnames(data4.2)[whichval]<-'biomval'




### Check structure ###

#str(data4.2)




### Make it a nice name ###

data4 <- data4.2






