#e4_makeFig4.R
#Make Figure 4. Mivi vs soilval for comptrt == N

### Load libraries ###
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)


### My plot theme ###
source('e4Code/mytheme.R')


### Prep dataframe ###
source('e4Code/e4_prepdfFig3n4.R')
#str(data3)


NamPval<-function(result, terms){
  SOILMEAS<-result$soilmeas
  i <- 0
  store<-numeric(0)
  result2<-result
  #storage containers
  for (i in 1:length(SOILMEAS)){
    vec<-result2[result2$soilmeas==SOILMEAS[i],-1]
    labelvec<-paste(terms, vec)
    store<-rbind(store, labelvec)
  }
  store1<-data.frame(soilmeas=SOILMEAS,store)
  store1
  return(store1)
}


PullPvals<-function(lmeobj){
  #Make pval labels
  thing<-SoilUnlist(lmeobj)$anovatab
  terms<-row.names(thing[[1]])
  SOILMEAS<-names(thing)
  s <- 0
  store<-numeric(0)
  #storage containers
  for (s in 1:length(SOILMEAS)){
    pvals<-thing[[SOILMEAS[s]]]$'Pr..F.'
    store<-rbind(store, pvals)
  }
  colnames(store)<-terms
  result<-data.frame(soilmeas=SOILMEAS, store)
  
  i<-0
  result2<-result
  for (i in 1:length(terms)){
    ns <-which(result2[,i+1] >= 0.1)
    s.<-which(result2[,i+1] < 0.1 & result2[,i+1] > 0.05)
    st<-which(result2[,i+1] <= 0.05 & result2[,i+1] > 0.01)
    stt<-which(result2[,i+1] <= 0.01)
    
    result2[ns, i+1] <- 'ns'
    result2[s., i+1] <- '.'
    result2[st, i+1] <- '*'
    result2[stt, i+1] <- '**'
  }
  
  return(result2)
}



### Order the factors a logical way ###

data3$soilmeas <- factor(data3$soilmeas, levels = c('nhdi', 'nodi', 'totdi', 'ammonifd', 'nitrifd','minzd','soilmoi'))
data3$type <- factor(data3$type, levels = c('Empty', 'Mivi', 'Pavi', 'Sobi', 'CompEmpty', 'CompSobi', 'CompPavi'))



### Make prettier labels for the  soil measurements ###

soilmeas_names <- list(
  'nhdi'="Ammonium\n(ugN/G)",
  'nodi'="Nitrate\n(ugN/G)",
  'totdi'="Total Inorganic N\n(ugN/G)",
  'ammonifd'="Ammonification\n(ugN/G*d)",
  'nitrifd'="Nitrification\n(ugN/G*d)",
  'minzd'="Mineralization\n(ugN/G*d)",
  'soilmoi'="Soil Moisture\n(%)"
)

soilmeas_labeller <- function(variable,value){
  return(soilmeas_names[value])
}




### FIGURE 4. Plot M.v. biomass vs soil value in facets for comptrt=N ###

sub1<-subset(data3, type == 'Mivi' | type == 'Empty' | type == 'CompEmpty')

#attached the predicted values from the lme fixed effects
#dim(sub1);length(lme.fig4.predict)
sub1$model.predict<-lme.fig4.predict

fig4 <- ggplot(sub1, aes(x=mivi, y=soilval)) + 
  mytheme +
  ylab('Soil measurement') +
  xlab('Dry aboveground\nMicrostegium biomass (g)') +
  facet_grid( soilmeas ~. , scales='free', labeller=soilmeas_labeller) +
  geom_point(size=1.5) +
  geom_line(data=sub1, aes(x=mivi, y=model.predict))
#fig4
# # Make a aux dataframe to hold the lme model fixed effect p values
result<-PullPvals(lme.fig4) #ignore warning message, pull pvals
terms<-c('mv','mv2')
df.aux<-NamPval(result, terms)
df.aux$x<-rep(45, dim(df.aux)[1])
df.aux$y<-c(35,100,100,5,12,12,90)
size<-2
fig4.text <- fig4 + 
  geom_text(aes(x, y, label=X1), data=df.aux, vjust=1, hjust=0, size=size) + 
  geom_text(aes(x, y, label=X2), data=df.aux, vjust=2, hjust=0, size=size) 
#fig4.text

ggsave(filename="e4Output_figures/fig4.pdf", plot=fig4.text, width = 3.5, height = 8.5, units = 'in') #save the plot and define its size




