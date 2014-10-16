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




### FIGURE 3. Plot monoculture type vs soil value in facets ###

sub1<-subset(data3, type == 'Mivi' | type == 'Empty' | type == 'Pavi' | type == 'Sobi')
  
fig3 <- ggplot(sub1, aes(x=type, y=soilval)) + 
  mytheme +
  ylab('Soil measurement') +
  xlab('Monoculture type') +
  facet_grid( soilmeas ~. , scales='free', labeller=soilmeas_labeller) +
  stat_summary(fun.y = mean, geom='point', size=3) +
  stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', mult = 1, width=0.2)
fig3
ggsave(filename="fig3.pdf", plot=fig3, width = 5, height = 12, units = 'in') #save the plot and define its size















