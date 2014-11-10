#e4_makeFig5.R
#Make Figure 5: Mixture plant biomass vs soil measure (sF)

### Load libraries ###
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)


### My plot theme ###
source('e4Code/mytheme.R')


### Prep dataframe ###
source('e4Code/e4_prepdfFig5.R')
#str(data4)
data5<-data4 #just switch names


### Order the factors a logical way and give them pretty labels ###

data5 <- transform(data5, soilmeas = factor(soilmeas, levels=c("nhdi", "nodi", "totdi", "ammonifd", "nitrifd","minzd", "soilmoi")))
data5 <- transform(data5, comptrt = factor(comptrt, levels=c("N","P","S")))
data5 <- transform(data5, biommeas = factor(biommeas, levels=c("mivi","comptrt","total","relmivi")))


### Make prettier labels for the  soil measurements ###

# soilmeas_names <- list(
#   'nhdi'="Ammonium\n(ugN/G)",
#   'nodi'="Nitrate\n(ugN/G)",
#   'totdi'="Total Inorganic N\n(ugN/G)",
#   'ammonifd'="Ammonification\n(ugN/G*d)",
#   'nitrifd'="Nitrification\n(ugN/G*d)",
#   'minzd'="Mineralization\n(ugN/G*d)",
#   'soilmoi'="Soil Moisture\n(%)"
# )

soilmeas_labeller <- function(variable,value){
  return(soilmeas_names[value])
}


LoopIt<-function(sub1){
  #loop through each soil measure
  SOILMEAS<-unique(sub1$soilmeas)
  s <- 0
  store<-numeric(0)
  #storage containers
  for (s in 1:length(SOILMEAS)){
    #subset data by soil measure
    df <- subset(sub1, soilmeas == SOILMEAS[s])
    
    #run the full model with lmerTest active
    model <- lmer(sF ~ biomval + I(biomval^2) + comptrt + biomval:comptrt + (1|bk), data = df) 
    
    keep<-predict(model, re.form=NA) #this sets the random effects to 0
    df$pred<-keep
    
    store<-rbind(store, df)
  }
  return(store)
}



LoopIt.total<-function(sub1){
  #loop through each soil measure
  SOILMEAS<-unique(sub1$soilmeas)
  s <- 0
  store<-numeric(0)
  #storage containers
  for (s in 1:length(SOILMEAS)){
    #subset data by soil measure
    df <- subset(sub1, soilmeas == SOILMEAS[s])
    
    #run the full model with lmerTest active
    model <- lmer(sF ~ biomval + I(biomval^2) + (1|bk), data = df) 
    
    keep<-predict(model, re.form=NA) #this sets the random effects to 0
    df$pred<-keep
    
    store<-rbind(store, df)
  }
  return(store)
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



terms<-c('mv','mv2','neightrt','mv:neightrt')
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




### Figure 5a. Mivi vs soilmeasures, shapes are comptrt ###

### Subset data - I'm only looking at mivi biomass this panel ###
sub1<-subset(data5, biommeas == "mivi")

# Make a new dataframe with the predicted values attached
datafram<-LoopIt(sub1)

## Plot
fig5a<- ggplot(datafram) +
  ylab("Soil measurement") + 
  xlab("Microstegium\nbiomass (g)") + 
  geom_point(aes(x=biomval, y=sF, colour=comptrt, shape=comptrt), size=1.5) +
  geom_line(aes(x=biomval, y=pred, colour=comptrt, linetype=comptrt)) +
  facet_grid(soilmeas ~. , scale='free', labeller=soilmeas_labeller) +
  mytheme +
  scale_colour_manual(values=c("darkgrey", "black", "black"), 
                      name="Neighbor\ntreatment",
                      breaks=c("N", "P", "S"),
                      labels=c("No Neighbor", "Panicum", "Sorghum")) +
  scale_shape_manual(values=c(16, 17, 0), 
                     name="Neighbor\ntreatment",
                     breaks=c("N", "P", "S"),
                     labels=c("No Neighbor", "Panicum", "Sorghum")) +
  scale_linetype_manual(values=c(1, 2, 1), 
                     name="Neighbor\ntreatment",
                     breaks=c("N", "P", "S"),
                     labels=c("No Neighbor", "Panicum", "Sorghum"))
fig5a

# # Make a aux dataframe to hold the lme model fixed effect p values
result<-PullPvals(lme.fig5.mivi) #ignore warning message, pull pvals
terms<-c('mv','mv2','neightrt','mv:neightrt')
df.aux<-NamPval(result, terms)
df.aux$x<-rep(20, dim(df.aux)[1])
df.aux$y<-c(40,100,100,4,15,15,65)
size<-2
fig5a.text <- fig5a + 
  geom_text(aes(x, y, label=X1), data=df.aux, vjust=1, hjust=0, size=size) + 
  geom_text(aes(x, y, label=X2), data=df.aux, vjust=2, hjust=0, size=size) +
  geom_text(aes(x, y, label=X3), data=df.aux, vjust=3, hjust=0, size=size) +
  geom_text(aes(x, y, label=X4), data=df.aux, vjust=4, hjust=0, size=size)
fig5a.text


### Figure 5b. RelMivi vs soilmeasures, shapes are comptrt ###

### Subset data - I'm only looking at relmivi biomass this panel ###
sub1<-subset(data5, biommeas == "relmivi" & comptrt != 'N')

# Make a new dataframe with the predicted values attached
datafram<-LoopIt(sub1)

## Plot
fig5b<- ggplot(datafram) +
  ylab("Soil measurement") + 
  xlab("Microstegium\nrelative abundance (%)") + 
  geom_point(aes(x=biomval, y=sF, colour=comptrt, shape=comptrt), size=1.5) +
  geom_line(aes(x=biomval, y=pred, colour=comptrt, linetype=comptrt)) +
  facet_grid(soilmeas ~. , scale='free', labeller=soilmeas_labeller) +
  mytheme +
  scale_colour_manual(values=c("black", "black"), 
                      name="Neighbor\ntreatment",
                      breaks=c("P", "S"),
                      labels=c("Panicum", "Sorghum")) +
  scale_shape_manual(values=c(17, 0), 
                     name="Neighbor\ntreatment",
                     breaks=c("P", "S"),
                     labels=c("Panicum", "Sorghum")) +
  scale_linetype_manual(values=c(2, 1), 
                        name="Neighbor\ntreatment",
                        breaks=c("P", "S"),
                        labels=c("Panicum", "Sorghum"))
fig5b

# # Make a aux dataframe to hold the lme model fixed effect p values
result<-PullPvals(lme.fig5.relmivi) #ignore warning message, pull pvals
terms<-c('relmv','relmv2','neightrt','relmv:neightrt')
df.aux<-NamPval(result, terms)
df.aux$x<-rep(50, dim(df.aux)[1])
df.aux$y<-c(40,100,100,4,15,15,65)
size<-2
fig5b.text <- fig5b + 
  geom_text(aes(x, y, label=X1), data=df.aux, vjust=1, hjust=0, size=size) + 
  geom_text(aes(x, y, label=X2), data=df.aux, vjust=2, hjust=0, size=size) +
  geom_text(aes(x, y, label=X3), data=df.aux, vjust=3, hjust=0, size=size) +
  geom_text(aes(x, y, label=X4), data=df.aux, vjust=4, hjust=0, size=size)
fig5b.text






### Figure 6. Total biomass vs soilmeasures, shapes are comptrt ###

### Subset data - I'm only looking at total biomass this panel ###
sub1<-subset(data5, biommeas == "total")

# Make a new dataframe with the predicted values attached
datafram<-LoopIt.total(sub1)

fig6<- ggplot(datafram) +
  ylab("Soil measurement") + 
  xlab("Total dry above-\nground plant biomass (g)") + 
  geom_point(aes(x=biomval, y=sF), size=1.5) +
  geom_line(aes(x=biomval, y=pred)) +
  facet_grid(soilmeas ~. , scale='free', labeller=soilmeas_labeller) +
  mytheme 
fig6

# # Make a aux dataframe to hold the lme model fixed effect p values
result<-PullPvals(lme.fig5.total) #ignore warning message, pull pvals
terms<-c('total','total2')
df.aux<-NamPval(result, terms)
df.aux$x<-rep(100, dim(df.aux)[1])
df.aux$y<-c(35,85,85,4,15,15,80)
size<-2
fig6.text <- fig6 + 
  geom_text(aes(x, y, label=X1), data=df.aux, vjust=1, hjust=0, size=size) + 
  geom_text(aes(x, y, label=X2), data=df.aux, vjust=2, hjust=0, size=size) 
fig6.text






### ALL PANELS ###
fig5a.text
fig5b.text


#FUNCTION TO ASSIGN A PLOT'S LEGEND AS A GROB OBJECT
g_legend<-function(p){
  tmp <- ggplotGrob(p)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

#pick the main legend for all panels
legend <- g_legend(fig5a.text)
lwidth <- sum(legend$width)

#arrange the grob objects
ag<-arrangeGrob(fig5a.text + theme(legend.position="none"),
                fig5b.text + theme(legend.position="none"),
                nrow=1)
fig5<-arrangeGrob(ag, 
                  legend, 
                  widths=unit.c(unit(1, "npc") - lwidth, lwidth), 
                  nrow=1)
fig5
ggsave(filename="e4Output_figures/fig5.pdf", plot=fig5, width = 6, height = 8.5, units = 'in') #save the plot and define its size


fig6.text
ggsave(filename="e4Output_figures/fig6.pdf", plot=fig6.text, width = 3.5, height = 8.5, units = 'in') #save the plot and define its size





