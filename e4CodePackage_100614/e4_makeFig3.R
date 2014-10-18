#e4_makeFig3.R
#Make Figure 3: Monoculture type vs soil measure value


### Load libraries ###
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)


### My plot theme ###
source('e4CodePackage_100614/mytheme.R')


### Prep dataframe ###
source('e4CodePackage_100614/e4_prepdfFig3n4.R')
#str(data3)




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




### FIGURE 3. Plot monoculture type vs total biomass and soil value in facets ###

#subset data to monocultures
sub1<-subset(data3, type == 'Mivi' | type == 'Empty' | type == 'Pavi' | type == 'Sobi')




### FIGURE 3bs. Plot monoculture type vs total biomass ###
fig3a <- ggplot(sub1, aes(x=type, y=total)) + 
  mytheme +
  ggtitle('a') +
  ylab('Total dry above-\nground biomass (g)') +
  xlab('Monoculture type') +
  stat_summary(fun.y = mean, geom='point', size=2) +
  stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', mult = 1, width=0.2, size=0.5)
fig3a 

#ammend dataframe to include posthoc letters
# Empty = a
# Mivi = b
# Pavi = b
# Sobi = c
labels <- c('a','b','b','c')
addlabels <- function(x) {
  labpos <- mean(x) + 8
  names(labpos) <- c("y") #this is what ggplot is expecting
  return (labpos)
}
#fig3a <- fig3a + stat_summary(fun.data = 'addlabels', geom='text', label=labels)




### FIGURE 3b. Plot monoculture type vs soil value in facets ###
fig3b <- ggplot(sub1, aes(x=type, y=soilval)) + 
  mytheme +
  ggtitle('b') +
  ylab('Soil measurement') +
  xlab('Monoculture type') +
  facet_grid( soilmeas ~. , scales='free', labeller=soilmeas_labeller) +
  stat_summary(fun.y = mean, geom='point', size=2) +
  stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', mult = 1, width=0.2, size=0.5)
fig3b

#ammend dataframe to include posthoc letters .. I don't know how to do this on a faceted plot yet




### FIGURE 3, ALL PANELS ###

#check out the individual panels
#fig3a
#fig3b 

#arrange the grob objects
fig3<-arrangeGrob(fig3a + theme(legend.position="none"),
                  fig3b + theme(legend.position="none"),
                  heights=unit.c(unit(1/6, "native"), 
                                 unit(5/6, "native")),
                  nrow=2)
fig3



ggsave(filename="fig3.pdf", plot=fig3, width = 4, height = 10, units = 'in') #save the plot and define its size




